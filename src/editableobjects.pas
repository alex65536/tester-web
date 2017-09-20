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
unit editableobjects;

{$mode objfpc}{$H+}{$M+}

interface

uses
  Classes, SysUtils, TypInfo, webstrconsts, users, datastorages, tswebobservers;

type
  TEditableAccessRights = (erNone, erRead, erWrite, erOwner);
  TEditableAccessRightsSet = set of TEditableAccessRights;

const
  AccessCanReadSet = [erRead, erWrite, erOwner];
  AccessCanWriteSet = [erWrite, erOwner];
  EditorsSet = [urEditor, urAdmin, urOwner];

type
  EEditableAction = class(Exception);
  EEditableNotExist = class(EEditableAction);
  EEditableValidate = class(EEditableAction);
  EEditableAccess = class(EEditableAction);
  EEditableAccessDenied = class(EEditableAction);

  { TEditorUser }

  TEditorUser = class(TUser)
  protected
    function DoGetRole: TUserRole; override;
  end;

  TEditableObject = class;
  TEditableManager = class;

  { TEditableCustomSession }

  TEditableCustomSession = class
  private
    FManager: TEditableManager;
    FStorage: TAbstractDataStorage;
    FUser: TEditorUser;
  protected
    property Storage: TAbstractDataStorage read FStorage;
    {%H-}constructor Create(AManager: TEditableManager; AUser: TEditorUser);
  public
    property Manager: TEditableManager read FManager;
    property User: TEditorUser read FUser;
    constructor Create;
  end;

  { TEditableObjectSession }

  TEditableObjectSession = class(TEditableCustomSession)
  private
    FEditableObject: TEditableObject;
  protected
    function FullUserKeyName(const Key: string): string;
    {%H-}constructor Create(AManager: TEditableManager; AUser: TEditorUser;
      AObject: TEditableObject);
  public
    property EditableObject: TEditableObject read FEditableObject;
    function AccessLevel: TEditableAccessRights;
  end;

  { TEditableObjectAccessSession }

  TEditableObjectAccessSession = class(TEditableObjectSession)
  public
    function CanAddUser({%H-}Target: TUserInfo): boolean; virtual;
    function CanDeleteUser(Target: TUserInfo): boolean; virtual;
    function CanGrantAccessRights(Target: TUserInfo;
      AAccess: TEditableAccessRights): boolean; virtual;
    procedure AddUser(Target: TUserInfo);
    procedure DeleteUser(Target: TUserInfo);
    procedure GrantAccessRights(Target: TUserInfo; AAccess: TEditableAccessRights);
  end;

  { TEditableTransaction }

  TEditableTransaction = class(TEditableObjectSession)
  protected
    procedure DoCommit; virtual; abstract;
  public
    function CanReadData: boolean; virtual;
    function CanWriteData: boolean; virtual;
    procedure Commit;
  end;

  { TEditableManagerSession }

  TEditableManagerSession = class(TEditableCustomSession)
  public
    function CanCreateNewObject: boolean; virtual;
    function CanDeleteObject(const AName: string): boolean; virtual;
    function CreateNewObject(const AName: string): TEditableObject;
    procedure DeleteObject(const AName: string);
    function ListAvailableObjects: TStringList;
  end;

  { TEditableObject }

  TEditableObject = class(IMessageSubscriber)
  private
    FManager: TEditableManager;
    FName: string;
    FStorage: TAbstractDataStorage;
    function GetID: integer;
  protected
    property Manager: TEditableManager read FManager;
    property Storage: TAbstractDataStorage read FStorage;
    procedure CheckHasAccess(Target: TUserInfo);
    procedure CheckNoAccess(Target: TUserInfo);
    function FormatExceptionMessage(const AMessage, AUsername: string): string;
    function FullKeyName(const Key: string): string;
    function UserSection(UserID: integer): string;
    function UsersSection: string;
    function FullUserKeyName(UserID: integer; const Key: string): string;
    function ObjectTypeName: string; virtual;
    procedure AddUser(Target: TUserInfo);
    procedure DoAddUser(Target: TUserInfo); virtual;
    procedure DeleteUser(Target: TUserInfo);
    procedure DoDeleteUser(Target: TUserInfo); virtual;
    procedure SafeDeleteUser(Target: TUserInfo);
    procedure GrantAccessRights(Target: TUserInfo;
      AAccess: TEditableAccessRights);
    procedure DoGrantAccessRights(Target: TUserInfo;
      AAccess: TEditableAccessRights); virtual;
    procedure HandleUserDeleting(AInfo: TUserInfo); virtual;
    procedure HandleUserChangedRole(AInfo: TUserInfo); virtual;
    procedure MessageReceived(AMessage: TAuthorMessage);
    {%H-}constructor Create(const AName: string; AManager: TEditableManager);
  public
    property Name: string read FName;
    property ID: integer read GetID;
    function GetObjectAuthor: TUserInfo;
    function GetObjectAuthorName: string;
    function GetAccessRights(Target: TUserInfo): TEditableAccessRights;
    function GetAccessRights(AUser: TEditorUser): TEditableAccessRights;
    function CreateAccessSession(AUser: TEditorUser): TEditableObjectAccessSession;
      virtual; abstract;
    function CreateTransaction(AUser: TEditorUser): TEditableTransaction;
      virtual; abstract;
    function ListUsers: TStringList; virtual;
    constructor Create;
  end;

  { TEditableManager }

  TEditableManager = class(TMessageAuthor, IMessageSubscriber)
  private
    FStorage: TAbstractDataStorage;
  protected
    property Storage: TAbstractDataStorage read FStorage;

    function ObjectSection(const AObject: string): string;
    function FullObjectKeyName(const AObject, AKey: string): string;
    function UserSection(const AObject: string; UserID: integer): string;
    function UsersSection(const AObject: string): string;
    function FullUserKeyName(const AObject: string; UserID: integer;
      const Key: string): string;
    function GetIdKey(AID: integer): string;

    function NextID: integer;
    procedure DoCreateNewObject({%H-}AObject: TEditableObject); virtual;
    function ObjectTypeName: string; virtual; abstract;
    function CreateStorage: TAbstractDataStorage; virtual; abstract;
    function CreateObject(const AName: string): TEditableObject; virtual; abstract;
    function GetObjectAuthor(const AObjectName: string): TUserInfo;
    function CreateNewObject(AOwner: TEditorUser;
      const AName: string): TEditableObject; virtual;
    procedure DeleteObject(const AName: string); virtual;
    procedure HandleUserDeleting({%H-}AInfo: TUserInfo); virtual;
    procedure HandleUserChangedRole({%H-}AInfo: TUserInfo); virtual;
    procedure SpreadMessageToObjects(AMessage: TAuthorMessage);
    procedure MessageReceived(AMessage: TAuthorMessage);
  public
    function IdToObjectName(AID: integer): string;
    function ObjectNameToId(const AName: string): integer;
    function ObjectExists(const AName: string): boolean;
    function GetObject(const AName: string): TEditableObject;
    function ListAllAvailableObjects: TStringList;
    function ListAvailableObjects(AUser: TEditorUser): TStringList;
    function GetAccessRights(const AObject: string; Target: TUserInfo): TEditableAccessRights;
    constructor Create;
    destructor Destroy; override;
  end;

function AccessRightsToStr(ARights: TEditableAccessRights): string;
function StrToAccessRights(const S: string): TEditableAccessRights;

procedure ValidateObjectName(const ObjType, ObjName: string);

implementation

function AccessRightsToStr(ARights: TEditableAccessRights): string;
begin
  Result := GetEnumName(TypeInfo(TEditableAccessRights), Ord(ARights));
end;

function StrToAccessRights(const S: string): TEditableAccessRights;
var
  R: TEditableAccessRights;
begin
  for R in TEditableAccessRights do
    if AccessRightsToStr(R) = S then
      Exit(R);
  raise EConvertError.Create(SNoSuchAccessRights);
end;

procedure ValidateObjectName(const ObjType, ObjName: string);
const
  MinObjNameLen = 3;
  MaxObjNameLen = 24;
  ObjAvailableChars = ['A' .. 'Z', 'a' .. 'z', '0' .. '9', '_', '-'];

  function AvailableCharsStr: string; inline;
  begin
    Result := '[''A'' .. ''Z'', ''a'' .. ''z'', ''0'' .. ''9'', ''_'', ''-'']';
  end;

var
  C: char;
begin
  if (Length(ObjName) < MinObjNameLen) or (Length(ObjName) > MaxObjNameLen) then
    raise EUserValidate.CreateFmt(SUsernameLength, [ObjType, MinObjNameLen, MaxObjNameLen]);
  for C in ObjName do
    if not (C in ObjAvailableChars) then
      raise EUserValidate.CreateFmt(SUsernameChars, [ObjType, AvailableCharsStr]);
end;

{ TEditableManager }

function TEditableManager.ObjectSection(const AObject: string): string;
begin
  Result := 'objects.' + AObject;
end;

function TEditableManager.FullObjectKeyName(const AObject, AKey: string): string;
begin
  Result := ObjectSection(AObject) + '.' + AKey;
end;

function TEditableManager.UserSection(const AObject: string; UserID: integer): string;
begin
  Result := UsersSection(AObject) + '.id' + IntToStr(UserID);
end;

function TEditableManager.UsersSection(const AObject: string): string;
begin
  Result := FullObjectKeyName(AObject, 'users');
end;

function TEditableManager.FullUserKeyName(const AObject: string;
  UserID: integer; const Key: string): string;
begin
  Result := UserSection(AObject, UserId) + '.' + Key;
end;

function TEditableManager.GetIdKey(AID: integer): string;
begin
  Result := Format('ids.id%d', [AID]);
end;

function TEditableManager.NextID: integer;
begin
  Result := FStorage.ReadInteger('lastId', 0);
  Inc(Result);
  FStorage.WriteInteger('lastId', Result);
end;

procedure TEditableManager.DoCreateNewObject(AObject: TEditableObject);
begin
  // do nothing
end;

function TEditableManager.GetObjectAuthor(const AObjectName: string): TUserInfo;
var
  AuthorID: integer;
begin
  AuthorID := FStorage.ReadInteger(FullObjectKeyName(AObjectName, 'authorId'), -1);
  Result := UserManager.GetUserInfo(AuthorID);
  if Result = nil then
  begin
    Result := UserManager.ServerOwnerInfo;
    FStorage.WriteInteger(FullObjectKeyName(AObjectName, 'authorId'), Result.ID);
  end;
end;

function TEditableManager.CreateNewObject(AOwner: TEditorUser;
  const AName: string): TEditableObject;
var
  ObjectID: integer;
  OwnerInfo: TUserInfo;
begin
  // validate object
  ValidateObjectName(ObjectTypeName, AName);
  if ObjectExists(AName) then
    raise EEditableValidate.CreateFmt(SObjectExists, [ObjectTypeName, AName]);
  // create object
  Result := CreateObject(AName);
  try
    // assign IDs
    ObjectID := NextID;
    FStorage.WriteString(GetIdKey(ObjectID), AName);
    FStorage.WriteInteger(FullObjectKeyName(AName, 'id'), ObjectID);
    // assign author
    FStorage.WriteInteger(FullObjectKeyName(AName, 'authorId'), AOwner.ID);
    // give author and owner rights
    Result.AddUser(AOwner.Info);
    Result.GrantAccessRights(AOwner.Info, erOwner);
    OwnerInfo := UserManager.ServerOwnerInfo;
    try
      Result.AddUser(OwnerInfo);
      Result.GrantAccessRights(OwnerInfo, erOwner);
    finally
      FreeAndNil(OwnerInfo);
    end;
    // do the rest
    DoCreateNewObject(Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TEditableManager.DeleteObject(const AName: string);
begin
  if not ObjectExists(AName) then
    raise EEditableNotExist.CreateFmt(SObjectDoesNotExist, [ObjectTypeName, AName]);
  FStorage.DeleteVariable(GetIdKey(ObjectNameToId(AName)));
  FStorage.DeletePath(ObjectSection(AName));
end;

procedure TEditableManager.HandleUserDeleting(AInfo: TUserInfo);
begin
  // do nothing
end;

procedure TEditableManager.HandleUserChangedRole(AInfo: TUserInfo);
begin
  // do nothing
end;

procedure TEditableManager.SpreadMessageToObjects(AMessage: TAuthorMessage);
var
  Objects: TStringList;
  ObjName: string;
  OneObject: TEditableObject;
begin
  Objects := ListAllAvailableObjects;
  for ObjName in Objects do
  begin
    OneObject := GetObject(ObjName);
    try
      OneObject.MessageReceived(AMessage);
    finally
      FreeAndNil(OneObject);
    end;
  end;
end;

procedure TEditableManager.MessageReceived(AMessage: TAuthorMessage);
begin
  // spread message
  SpreadMessageToObjects(AMessage);
  // handle message
  if AMessage is TUserDeletingMessage then
    HandleUserDeleting((AMessage as TUserDeletingMessage).Info)
  else if AMessage is TUserChangedRoleMessage then
    HandleUserDeleting((AMessage as TUserChangedRoleMessage).Info);
end;

function TEditableManager.IdToObjectName(AID: integer): string;
begin
  Result := FStorage.ReadString(GetIdKey(AID), '');
end;

function TEditableManager.ObjectNameToId(const AName: string): integer;
begin
  if not ObjectExists(AName) then
    Result := -1
  else
    Result := FStorage.ReadInteger(FullObjectKeyName(AName, 'id'), -1);
end;

function TEditableManager.ObjectExists(const AName: string): boolean;
begin
  try
    ValidateObjectName(ObjectTypeName, AName);
    Result := FStorage.VariableExists(FullObjectKeyName(AName, 'id'));
  except
    Result := False;
  end;
end;

function TEditableManager.GetObject(const AName: string): TEditableObject;
begin
  if not ObjectExists(AName) then
    raise EEditableNotExist.CreateFmt(SObjectDoesNotExist, [ObjectTypeName, AName]);
  Result := CreateObject(AName);
end;

function TEditableManager.ListAllAvailableObjects: TStringList;
begin
  Result := FStorage.GetChildElements('objects');
end;

function TEditableManager.ListAvailableObjects(AUser: TEditorUser): TStringList;
var
  AllAvailable: TStringList;
  ObjName: string;
begin
  Result := TStringList.Create;
  try
    AllAvailable := TStringList.Create;
    try
      for ObjName in AllAvailable do
        if GetAccessRights(ObjName, AUser.Info) <> erNone then
          Result.Add(ObjName);
    finally
      FreeAndNil(AllAvailable);
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TEditableManager.GetAccessRights(const AObject: string;
  Target: TUserInfo): TEditableAccessRights;
var
  RightsStr: string;
begin
  RightsStr := FStorage.ReadString(FullUserKeyName(AObject, Target.ID, 'rights'),
    AccessRightsToStr(erNone));
  Result := StrToAccessRights(RightsStr);
end;

constructor TEditableManager.Create;
begin
  FStorage := CreateStorage;
  UserManager.Subscribe(Self);
end;

destructor TEditableManager.Destroy;
begin
  UserManager.Unsubscribe(Self);
  FreeAndNil(FStorage);
  inherited Destroy;
end;

{ TEditableObject }

function TEditableObject.GetID: integer;
begin
  Result := FStorage.ReadInteger(FullKeyName('id'), -1);
end;

procedure TEditableObject.CheckHasAccess(Target: TUserInfo);
begin
  if GetAccessRights(Target) = erNone then
    raise EEditableAccess.Create(FormatExceptionMessage(SObjectUserNoAccess, Target.Username));
end;

procedure TEditableObject.CheckNoAccess(Target: TUserInfo);
begin
  if GetAccessRights(Target) <> erNone then
    raise EEditableAccess.Create(FormatExceptionMessage(SObjectUserHasAccess, Target.Username));
end;

function TEditableObject.FormatExceptionMessage(const AMessage,
  AUsername: string): string;
begin
  Result := Format(AMessage, [ObjectTypeName, FName, AUsername]);
end;

function TEditableObject.FullKeyName(const Key: string): string;
begin
  Result := FManager.FullObjectKeyName(FName, Key);
end;

function TEditableObject.UserSection(UserID: integer): string;
begin
  Result := FManager.UserSection(FName, UserID);
end;

function TEditableObject.UsersSection: string;
begin
  Result := FManager.UsersSection(FName);
end;

function TEditableObject.FullUserKeyName(UserID: integer; const Key: string): string;
begin
  Result := FManager.FullUserKeyName(FName, UserID, Key);
end;

function TEditableObject.ObjectTypeName: string;
begin
  Result := Manager.ObjectTypeName;
end;

procedure TEditableObject.AddUser(Target: TUserInfo);
begin
  CheckNoAccess(Target);
  DoAddUser(Target);
end;

procedure TEditableObject.DoAddUser(Target: TUserInfo);
begin
  FStorage.WriteString(FullUserKeyName(Target.ID, 'rights'), AccessRightsToStr(erRead));
end;

procedure TEditableObject.DeleteUser(Target: TUserInfo);
begin
  CheckHasAccess(Target);
  if GetAccessRights(Target) = erOwner then
    raise EEditableAccessDenied.Create(SAccessDenied);
  DoDeleteUser(Target);
end;

procedure TEditableObject.DoDeleteUser(Target: TUserInfo);
begin
  FStorage.DeletePath(UserSection(Target.ID));
end;

procedure TEditableObject.SafeDeleteUser(Target: TUserInfo);
var
  Rights: TEditableAccessRights;
begin
  Rights := GetAccessRights(Target);
  if Rights = erNone then
    Exit;
  // just delete
  DoDeleteUser(Target);
  // refresh author if needed
  if Rights = erOwner then
    with Manager.GetObjectAuthor(Name) do
      Free;
end;

procedure TEditableObject.GrantAccessRights(Target: TUserInfo; AAccess: TEditableAccessRights);
begin
  CheckHasAccess(Target);
  if AAccess = erNone then
    raise EEditableAccess.Create(SObjectCannotGrantNoneRole);
  DoGrantAccessRights(Target, AAccess);
end;

procedure TEditableObject.DoGrantAccessRights(Target: TUserInfo;
  AAccess: TEditableAccessRights);
begin
  FStorage.WriteString(FullUserKeyName(Target.ID, 'rights'), AccessRightsToStr(AAccess));
end;

procedure TEditableObject.HandleUserDeleting(AInfo: TUserInfo);
begin
  SafeDeleteUser(AInfo);
end;

procedure TEditableObject.HandleUserChangedRole(AInfo: TUserInfo);
begin
  if not (AInfo.Role in EditorsSet) then
    SafeDeleteUser(AInfo);
end;

procedure TEditableObject.MessageReceived(AMessage: TAuthorMessage);
begin
  if AMessage is TUserDeletingMessage then
    HandleUserDeleting((AMessage as TUserDeletingMessage).Info)
  else if AMessage is TUserChangedRoleMessage then
    HandleUserChangedRole((AMessage as TUserChangedRoleMessage).Info);
end;

constructor TEditableObject.Create(const AName: string; AManager: TEditableManager);
begin
  FName := AName;
  FManager := AManager;
  FStorage := AManager.Storage;
end;

function TEditableObject.GetObjectAuthor: TUserInfo;
begin
  Result := Manager.GetObjectAuthor(FName);
end;

function TEditableObject.GetObjectAuthorName: string;
begin
  with GetObjectAuthor do
    try
      Result := Username;
    finally
      Free;
    end;
end;

function TEditableObject.GetAccessRights(Target: TUserInfo): TEditableAccessRights;
begin
  Result := FManager.GetAccessRights(FName, Target);
end;

function TEditableObject.GetAccessRights(AUser: TEditorUser): TEditableAccessRights;
begin
  Result := GetAccessRights(AUser.Info);
end;

function TEditableObject.ListUsers: TStringList;
var
  IdSons: TStringList;
  IdStr: string;
  SonId: integer;
  Username: string;
begin
  Result := TStringList.Create;
  try
    IdSons := FStorage.GetChildElements(UsersSection);
    try
      for IdStr in IdSons do
      begin
        SonId := IdStr.Substring(2).ToInteger;
        Username := UserManager.IdToUsername(SonId);
        if Username <> '' then
          Result.Add(Username);
      end;
    finally
      FreeAndNil(IdSons);
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

constructor TEditableObject.Create;
begin
  // we don't want the object to be created publicly!
  raise EInvalidOperation.CreateFmt(SCreationPublic, [ClassName]);
end;

{ TEditableManagerSession }

function TEditableManagerSession.CanCreateNewObject: boolean;
begin
  Result := True;
end;

function TEditableManagerSession.CanDeleteObject(const AName: string): boolean;
begin
  Result := Manager.GetAccessRights(AName, User.Info) = erOwner;
end;

function TEditableManagerSession.CreateNewObject(const AName: string): TEditableObject;
begin
  if not CanCreateNewObject then
    raise EEditableAccessDenied.Create(SAccessDenied);
  Result := Manager.CreateNewObject(User, AName);
end;

procedure TEditableManagerSession.DeleteObject(const AName: string);
begin
  if not CanDeleteObject(AName) then
    raise EEditableAccessDenied.Create(SAccessDenied);
  Manager.DeleteObject(AName);
end;

function TEditableManagerSession.ListAvailableObjects: TStringList;
begin
  Result := Manager.ListAvailableObjects(User);
end;

{ TEditableTransaction }

function TEditableTransaction.CanReadData: boolean;
begin
  Result := EditableObject.GetAccessRights(User) in AccessCanReadSet;
end;

function TEditableTransaction.CanWriteData: boolean;
begin
  Result := EditableObject.GetAccessRights(User) in AccessCanWriteSet;
end;

procedure TEditableTransaction.Commit;
begin
  if not CanWriteData then
    raise EEditableAccessDenied.Create(SAccessDenied);
  DoCommit;
end;

{ TEditableObjectAccessSession }

function TEditableObjectAccessSession.CanAddUser(Target: TUserInfo): boolean;
begin
  Result := EditableObject.GetAccessRights(User) in AccessCanWriteSet;
end;

function TEditableObjectAccessSession.CanDeleteUser(Target: TUserInfo): boolean;
var
  UserRights, TargetRights: TEditableAccessRights;
begin
  // compute rights
  UserRights := AccessLevel;
  TargetRights := EditableObject.GetAccessRights(Target);
  // check
  Result := False;
  if TargetRights = erNone then
    Exit;
  if Target.Username = User.Username then
    Exit;
  if UserRights = erOwner then
    Result := TargetRights <> erOwner
  else if UserRights = erWrite then
    Result := TargetRights = erRead;
end;

function TEditableObjectAccessSession.CanGrantAccessRights(Target: TUserInfo;
  AAccess: TEditableAccessRights): boolean;
var
  UserRights, TargetRights: TEditableAccessRights;
begin
  // compute rights
  UserRights := AccessLevel;
  TargetRights := EditableObject.GetAccessRights(Target);
  // check
  Result := False;
  if (TargetRights = erNone) or (AAccess = erNone) then
    Exit;
  if Target.Username = User.Username then
    Exit;
  if UserRights = erOwner then
    Result := (TargetRights <> erOwner) and (AAccess <> erOwner)
  else if UserRights = erWrite then
    Result := (TargetRights in [erRead, erWrite]) and (AAccess = erRead);
end;

procedure TEditableObjectAccessSession.AddUser(Target: TUserInfo);
begin
  if not CanAddUser(Target) then
    raise EEditableAccessDenied.Create(SAccessDenied);
  EditableObject.AddUser(Target);
end;

procedure TEditableObjectAccessSession.DeleteUser(Target: TUserInfo);
begin
  if not CanDeleteUser(Target) then
    raise EEditableAccessDenied.Create(SAccessDenied);
  EditableObject.DeleteUser(Target);
end;

procedure TEditableObjectAccessSession.GrantAccessRights(Target: TUserInfo;
  AAccess: TEditableAccessRights);
begin
  if not CanGrantAccessRights(Target, AAccess) then
    raise EEditableAccessDenied.Create(SAccessDenied);
  EditableObject.GrantAccessRights(Target, AAccess);
end;

{ TEditableObjectSession }

function TEditableObjectSession.FullUserKeyName(const Key: string): string;
begin
  Result := EditableObject.FullUserKeyName(User.ID, Key);
end;

constructor TEditableObjectSession.Create(AManager: TEditableManager;
  AUser: TEditorUser; AObject: TEditableObject);
begin
  inherited Create(AManager, AUser);
  FEditableObject := AObject;
end;

function TEditableObjectSession.AccessLevel: TEditableAccessRights;
begin
  Result := EditableObject.GetAccessRights(User);
end;

{ TEditableCustomSession }

constructor TEditableCustomSession.Create(AManager: TEditableManager;
  AUser: TEditorUser);
begin
  FManager := AManager;
  FStorage := AManager.Storage;
  FUser := AUser;
end;

constructor TEditableCustomSession.Create;
begin
  // we don't want the sessions to be created publicly!
  raise EInvalidOperation.CreateFmt(SCreationPublic, [ClassName]);
end;

{ TEditorUser }

function TEditorUser.DoGetRole: TUserRole;
begin
  Result := urEditor;
end;

initialization
  UserClass[urEditor] := TEditorUser;

end.
