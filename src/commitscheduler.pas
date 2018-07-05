{
  This file is part of Tester Web

  Copyright (C) 2017 Alexander Kernozhitsky <sh200105@mail.ru>

  This program is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}
unit commitscheduler;

{$mode objfpc}{$H+}{$inline on}

interface

uses
  Classes, SysUtils, datastorages, fgl, dateutils, serverevents, tswebobservers,
  tswebconfig;

type
  TDataStorageList = specialize TFPGObjectList<TAbstractDataStorage>;

  { TCommitScheduler }

  TCommitScheduler = class(TObject, IFPObserver, IMessageSubscriber)
  private
    FCommitIntervalSeconds: integer;
    FStorages: TDataStorageList;
    FLastCommitTime: TDateTime;
  protected
    procedure FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation;
      {%H-}Data: Pointer);
    procedure CommitAll;
    function CanCommit: boolean; virtual;
    procedure MessageReceived(AMessage: TAuthorMessage);
  public
    property CommitIntervalSeconds: integer read FCommitIntervalSeconds
      write FCommitIntervalSeconds;
    procedure AttachStorage(AStorage: TAbstractDataStorage);
    procedure DetachStorage(AStorage: TAbstractDataStorage);
    procedure CheckCommit;
    procedure BackUp;
    constructor Create;
    destructor Destroy; override;
  end;

function Scheduler: TCommitScheduler; inline;

implementation

var
  FScheduler: TCommitScheduler;

function Scheduler: TCommitScheduler;
begin
  Result := FScheduler;
end;

{ TCommitScheduler }

procedure TCommitScheduler.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  if Operation = ooFree then
    DetachStorage(ASender as TAbstractDataStorage);
end;

procedure TCommitScheduler.CommitAll;
var
  I: integer;
begin
  for I := 0 to FStorages.Count - 1 do
    FStorages[I].Commit;
  FLastCommitTime := Now;
end;

function TCommitScheduler.CanCommit: boolean;
var
  CurTime: TDateTime;
  CommitTime: TDateTime;
begin
  CurTime := Now;
  // if curTime < lastCommitTime, we moved the system clock back and it's better to commit
  if CompareDateTime(CurTime, FLastCommitTime) < 0 then
  begin
    Result := True;
    Exit;
  end;
  // otherwise, check if the interval has passed
  CommitTime := IncSecond(FLastCommitTime, FCommitIntervalSeconds);
  Result := CompareDateTime(CurTime, CommitTime) >= 0;
end;

procedure TCommitScheduler.MessageReceived(AMessage: TAuthorMessage);
begin
  if AMessage is TIdleMessage then
    CheckCommit;
end;

procedure TCommitScheduler.AttachStorage(AStorage: TAbstractDataStorage);
begin
  FStorages.Add(AStorage);
  AStorage.FPOAttachObserver(Self);
end;

procedure TCommitScheduler.DetachStorage(AStorage: TAbstractDataStorage);
begin
  FStorages.Remove(AStorage);
  AStorage.FPODetachObserver(Self);
end;

procedure TCommitScheduler.CheckCommit;
begin
  if CanCommit then
  begin
    BackUp;
    CommitAll;
  end;
end;

procedure TCommitScheduler.BackUp;
var
  I: integer;
begin
  for I := 0 to FStorages.Count - 1 do
    FStorages[I].BackUp;
end;

constructor TCommitScheduler.Create;
begin
  IdleMessenger.Subscribe(Self);
  FStorages := TDataStorageList.Create(False);
  FCommitIntervalSeconds := Config.Storages_CommitIntervalSeconds;
  FLastCommitTime := Now;
end;

destructor TCommitScheduler.Destroy;
var
  I: integer;
begin
  for I := 0 to FStorages.Count - 1 do
    FStorages[I].FPODetachObserver(Self);
  FreeAndNil(FStorages);
  IdleMessenger.Unsubscribe(Self);
  inherited Destroy;
end;

initialization
  FScheduler := TCommitScheduler.Create;

finalization
  FreeAndNil(FScheduler);

end.

