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
  Classes, SysUtils, editableobjects, webstrconsts, datastorages, users,
  tswebutils;

type
  EContestAction = class(EEditableAction);
  EContestValidate = class(EEditableValidate);
  EContestAccessDenied = class(EEditableAccessDenied);

  TContest = class;
  TContestManager = class;

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

  { TContestParticipantSession }

  TContestParticipantSession = class(TEditableObjectSession)
  private
    FContest: TContest;
  protected
    {%H-}constructor Create(AManager: TEditableManager; AUser: TUser;
      AObject: TEditableObject);
  public
    property Contest: TContest read FContest;
    function CanAddParticipant: boolean; virtual;
    function CanDeleteParticipant: boolean; virtual;
    function CanListParticipants: boolean; virtual;
    procedure AddParticipant(AInfo: TUserInfo);
    procedure DeleteParticipant(AInfo: TUserInfo);
    function ListParticipants: TStringList;
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

  { TContest }

  TContest = class(TEditableObject)
  private
    function GetManager: TContestManager;
  protected
    function ParticipantsSectionName: string;
    function ParticipantsFullKeyName(ParticipantID: integer): string;
    procedure AddParticipant(AInfo: TUserInfo);
    procedure DoAddParticipant(AInfo: TUserInfo);
    procedure DeleteParticipant(AInfo: TUserInfo);
    procedure DoDeleteParticipant(AInfo: TUserInfo);
    function HasParticipant(AInfo: TUserInfo): boolean;
    function ListParticipants: TStringList;
    procedure HandleSelfDeletion; override;
    procedure HandleUserDeleting(AInfo: TUserInfo); override;
    {%H-}constructor Create(const AName: string; AManager: TEditableManager);
  public
    property Manager: TContestManager read GetManager;
    function CreateAccessSession(AUser: TUser): TEditableObjectAccessSession; override;
    function CreateParticipantSession(AUser: TUser): TContestParticipantSession; virtual;
    function CreateTransaction(AUser: TUser): TEditableTransaction; override;
  end;

  { TContestManager }

  TContestManager = class(TEditableManager)
  protected
    function ObjectTypeName: string; override;
    function CreateObject(const AName: string): TEditableObject; override;
    function CreateStorage: TAbstractDataStorage; override;
  public
    function CreateManagerSession(AUser: TUser): TEditableManagerSession; override;
  end;

implementation

{ TContestParticipantSession }

constructor TContestParticipantSession.Create(AManager: TEditableManager;
  AUser: TUser; AObject: TEditableObject);
begin
  inherited Create(AManager, AUser, AObject);
  FContest := AObject as TContest;
end;

function TContestParticipantSession.CanAddParticipant: boolean;
begin
  Result := AccessLevel in AccessCanWriteSet;
end;

function TContestParticipantSession.CanDeleteParticipant: boolean;
begin
  Result := AccessLevel in AccessCanWriteSet;
end;

function TContestParticipantSession.CanListParticipants: boolean;
begin
  Result := AccessLevel in AccessCanReadSet;
end;

procedure TContestParticipantSession.AddParticipant(AInfo: TUserInfo);
begin
  if not CanAddParticipant then
    raise EContestAccessDenied.Create(SAccessDenied);
  Contest.AddParticipant(AInfo);
end;

procedure TContestParticipantSession.DeleteParticipant(AInfo: TUserInfo);
begin
  if not CanDeleteParticipant then
    raise EContestAccessDenied.Create(SAccessDenied);
  Contest.DeleteParticipant(AInfo);
end;

function TContestParticipantSession.ListParticipants: TStringList;
begin
  if not CanListParticipants then
    raise EContestAccessDenied.Create(SAccessDenied);
  Result := Contest.ListParticipants;
end;

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

function TContest.GetManager: TContestManager;
begin
  Result := (inherited Manager) as TContestManager;
end;

function TContest.ParticipantsSectionName: string;
begin
  Result := FullKeyName('participants');
end;

function TContest.ParticipantsFullKeyName(ParticipantID: integer): string;
begin
  Result := ParticipantsSectionName + '.' + Id2Str(ParticipantID);
end;

procedure TContest.AddParticipant(AInfo: TUserInfo);
begin
  if HasParticipant(AInfo) then
    raise EContestValidate.CreateFmt(SParticipantAlreadyAdded, [AInfo.Username]);
  DoAddParticipant(AInfo);
  UpdateModifyTime;
end;

procedure TContest.DoAddParticipant(AInfo: TUserInfo);
begin
  Storage.WriteBool(ParticipantsFullKeyName(AInfo.ID), True);
end;

procedure TContest.DeleteParticipant(AInfo: TUserInfo);
begin
  if not HasParticipant(AInfo) then
    raise EContestValidate.CreateFmt(SParticipantAlreadyDeleted, [AInfo.Username]);
  DoDeleteParticipant(AInfo);
  UpdateModifyTime;
end;

procedure TContest.DoDeleteParticipant(AInfo: TUserInfo);
begin
  Storage.DeleteVariable(ParticipantsFullKeyName(AInfo.ID));
end;

function TContest.HasParticipant(AInfo: TUserInfo): boolean;
begin
  Result := Storage.VariableExists(ParticipantsFullKeyName(AInfo.ID));
end;

function TContest.ListParticipants: TStringList;
var
  Keys: TStringList;
  Key: string;
begin
  Result := TStringList.Create;
  try
    Keys := Storage.GetChildElements(ParticipantsSectionName);
    try
      if Keys <> nil then
        for Key in Keys do
          Result.Add(UserManager.IdToUsername(Str2Id(Key)));
    finally
      FreeAndNil(Keys);
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TContest.HandleSelfDeletion;
begin
  inherited HandleSelfDeletion;
  // to be implemented later ...
  // TODO : Implement it, make "later" come :)
end;

procedure TContest.HandleUserDeleting(AInfo: TUserInfo);
begin
  inherited HandleUserDeleting(AInfo);
  DoDeleteParticipant(AInfo);
end;

constructor TContest.Create(const AName: string; AManager: TEditableManager);
begin
  inherited Create(AName, AManager);
end;

function TContest.CreateAccessSession(AUser: TUser): TEditableObjectAccessSession;
begin
  Result := TContestAccessSession.Create(Manager, AUser, Self);
end;

function TContest.CreateParticipantSession(AUser: TUser): TContestParticipantSession;
begin
  Result := TContestParticipantSession.Create(Manager, AUser, Self);
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

