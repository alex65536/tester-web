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
unit contests;

// Currently this unit is just a template.
// It only inherits contest and its sessions from editable object.
// TODO : Implement contest system !!!

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, editableobjects, webstrconsts, datastorages, users;

type
  TContest = class;

  { TContestAccessSession }

  TContestAccessSession = class(TEditableObjectAccessSession)
  protected
    {%H-}constructor Create(AManager: TEditableManager; AUser: TUser;
      AObject: TEditableObject);
  end;

  { TBaseContestTransaction }

  TBaseContestTransaction = class(TEditableTransaction)
  protected
    procedure DoReload; override;
    procedure DoCommit; override;
    procedure DoClone(ADest: TEditableTransaction); override;
    {%H-}constructor Create(AManager: TEditableManager; AUser: TUser;
      AObject: TEditableObject);
  public
    procedure Validate; override;
  end;

  { TContestTransaction }

  TContestTransaction = class(TBaseContestTransaction)
  public
  end;

  { TContestManagerSession }

  TContestManagerSession = class(TEditableManagerSession)
  protected
    {%H-}constructor Create(AManager: TEditableManager; AUser: TUser);
  end;

  TContest = class(TEditableObject)
  protected
    procedure HandleSelfDeletion; override;
    {%H-}constructor Create(const AName: string; AManager: TEditableManager);
  public
    function CreateAccessSession(AUser: TUser): TEditableObjectAccessSession; override;
    function CreateTransaction(AUser: TUser): TEditableTransaction; override;
  end;

  TContestManager = class(TEditableManager)
  protected
    function ObjectTypeName: string; override;
    function CreateObject(const AName: string): TEditableObject; override;
    function CreateStorage: TAbstractDataStorage; override;
  public
    function CreateManagerSession(AUser: TUser): TEditableManagerSession; override;
  end;

implementation

{ TContestManager }

function TContestManager.ObjectTypeName: string;
begin
  Result := SContestTypeName;
end;

function TContestManager.CreateObject(const AName: string): TEditableObject;
begin
  Result := TContest.Create(AName, Self);
end;

function TContestManager.CreateStorage: TAbstractDataStorage;
begin
  Result := TXmlDataStorage.Create('contests');
end;

function TContestManager.CreateManagerSession(AUser: TUser): TEditableManagerSession;
begin
  Result := TContestManagerSession.Create(Self, AUser);
end;

{ TContest }

procedure TContest.HandleSelfDeletion;
begin
  inherited HandleSelfDeletion;
  // to be implemented later ...
  // TODO : Implement it, make "later" come :)
end;

constructor TContest.Create(const AName: string; AManager: TEditableManager);
begin
  inherited Create(AName, AManager);
end;

function TContest.CreateAccessSession(AUser: TUser): TEditableObjectAccessSession;
begin
  Result := TContestAccessSession.Create(Manager, AUser, Self);
end;

function TContest.CreateTransaction(AUser: TUser): TEditableTransaction;
begin
  Result := TContestTransaction.Create(Manager, AUser, Self);
end;

{ TContestManagerSession }

constructor TContestManagerSession.Create(AManager: TEditableManager; AUser: TUser);
begin
  inherited Create(AManager, AUser);
end;

{ TBaseContestTransaction }

procedure TBaseContestTransaction.DoReload;
begin
  inherited DoReload;
end;

procedure TBaseContestTransaction.DoCommit;
begin
  inherited DoCommit;
end;

procedure TBaseContestTransaction.DoClone(ADest: TEditableTransaction);
begin
  inherited DoClone(ADest);
end;

constructor TBaseContestTransaction.Create(AManager: TEditableManager;
  AUser: TUser; AObject: TEditableObject);
begin
  inherited Create(AManager, AUser, AObject);
end;

procedure TBaseContestTransaction.Validate;
begin
  inherited Validate;
end;

{ TContestAccessSession }

constructor TContestAccessSession.Create(AManager: TEditableManager;
  AUser: TUser; AObject: TEditableObject);
begin
  inherited Create(AManager, AUser, AObject);
end;

end.

