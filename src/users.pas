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
unit users;

{$mode objfpc}{$H+}{$M+}{$B-}{$inline on}

interface

uses
  Classes, SysUtils, HTTPDefs, datastorages, tswebcrypto, typinfo, webstrconsts,
  serverconfig, commitscheduler;

type
  EUserAction = class(Exception);
  EUserAuthentificate = class(EUserAction);
  EUserAccessDenied = class(EUserAction);
  EUserValidate = class(EUserAction);
  EUserNotExist = class(EUserAction);

  TUserManager = class;
  TUserRole = (urBlocked, urSimple, urEditor, urAdmin, urOwner);
  TUserRoleSet = set of TUserRole;

const
  AllUserRoles = [Low(TUserRole) .. High(TUserRole)];

// TODO : Internally keep users by unique ID!

type

  { TUserInfo }

  TUserInfo = class
  private
    FManager: TUserManager;
    FUsername: string;
    function GetFirstName: string;
    function GetID: integer;
    function GetLastName: string;
    function GetLastVisit: TDateTime;
    function GetRegisteredAt: TDateTime;
    function GetStorage: TAbstractDataStorage;
    function GetUserRole: TUserRole;
  protected
    property Storage: TAbstractDataStorage read GetStorage;
    function FullKeyName(const Key: string): string;
    // creating users should only be done via TUserManager or TUser!
    {%H-}constructor Create(AManager: TUserManager; const AUsername: string);
  public
    property Manager: TUserManager read FManager;
    property Username: string read FUsername;
    property ID: integer read GetID;
    property Role: TUserRole read GetUserRole;
    property FirstName: string read GetFirstName;
    property LastName: string read GetLastName;
    property LastVisit: TDateTime read GetLastVisit;
    property RegisteredAt: TDateTime read GetRegisteredAt;
    constructor Create;
  end;

  { IUserInfo }

  {$interfaces CORBA}
  IUserInfo = interface
    ['{A6032A55-52B8-4715-890C-E436A25951EE}']
    function GetInfo: TUserInfo;
  end;
  {$interfaces COM}

  { TUser }

  TUser = class
  private
    FAuthentificated: boolean;
    FInfo: TUserInfo;
    FManager: TUserManager;
    FRole: TUserRole;
    FUsername: string;
    FFirstName: string;
    FLastName: string;
    FUpdating: boolean;
    function GetFirstName: string;
    function GetID: integer;
    function GetLastName: string;
    function GetLastVisit: TDateTime;
    function GetRegisteredAt: TDateTime;
    function GetStorage: TAbstractDataStorage;
    procedure RequireUpdating;
    procedure SetFirstName(AValue: string);
    procedure SetLastName(AValue: string);
  protected
    property Storage: TAbstractDataStorage read GetStorage;
    procedure GeneratePassword(const Password: string);
    function CheckPassword(const Password: string): boolean;
    function FullKeyName(const Key: string): string;
    function DoGetRole: TUserRole; virtual;
    // creating users should only be done via TUserManager!
    {%H-}constructor Create(AManager: TUserManager; const AUsername: string); virtual;
  public
    property Manager: TUserManager read FManager;
    property Info: TUserInfo read FInfo;
    property Role: TUserRole read FRole;
    property Username: string read FUsername;
    property ID: integer read GetID;
    property FirstName: string read GetFirstName write SetFirstName;
    property LastName: string read GetLastName write SetLastName;
    property LastVisit: TDateTime read GetLastVisit;
    property RegisteredAt: TDateTime read GetRegisteredAt;
    procedure BeginUpdate; virtual;
    procedure EndUpdate(Apply: boolean); virtual;
    procedure ChangePassword(const OldPassword, NewPassword,
      NewPasswordConfirm: string);
    procedure Authentificate(const Password: string);
    procedure NeedsAuthentification;
    procedure UpdateLastVisit;
    procedure WriteRole;
    constructor Create;
    destructor Destroy; override;
  end;

  TUserClass = class of TUser;

  { TUserManager }

  TUserManager = class
  protected const
    StoragePath = 'users';
  private
    FStorage: TAbstractDataStorage;
    function GetUserCount: integer;
    procedure IncUserCount(Delta: integer);
  protected
    property Storage: TAbstractDataStorage read FStorage;
    function NextID: integer;
    function UserSection(const Username: string): string;
    function GetIdKey(AID: integer): string;
    function FullKeyName(const Username, Key: string): string;
    function CreateUserClass(AClass: TUserClass; const Username: string): TUser;
    function CreateUser(ARole: TUserRole; const Username: string): TUser; virtual;
    function CreateDataStorage: TAbstractDataStorage; virtual;
  public
    function IdToUsername(AID: integer): string;
    function UsernameToId(const AUsername: string): integer;
    function UserExists(const Username: string): boolean;
    function LoadUserFromUsername(const Username: string): TUser;
    function LoadUserFromID(AID: integer): TUser;
    function LoadUserFromSession(ASession: TCustomSession): TUser;
    function GetUserInfo(const Username: string): TUserInfo;
    procedure AuthentificateSession(ASession: TCustomSession;
      const Username, Password: string);
    procedure AddNewUser(const AUsername, APassword, APasswordConfirm: string;
      const AFirstName, ALastName: string; ARole: TUserRole = urSimple);
    function GetRole(const AUsername: string): TUserRole;
    procedure GrantRole(const AUsername: string; ARole: TUserRole);
    procedure DeleteUser(const AUsername: string);
    constructor Create;
    destructor Destroy; override;
  end;

const
  UserRoleNames: array [TUserRole] of string = (
    SBlockedUserRole, // urBlocked
    SSimpleUserRole,  // urSimple
    SEditorUserRole,  // urEditor
    SAdminUserRole,   // urAdmin
    SOwnerUserRole    // urOwner
    );

function GetUserClass(ARole: TUserRole): TUserClass;
procedure SetUserClass(ARole: TUserRole; AClass: TUserClass);
property UserClass[ARole: TUserRole]: TUserClass read GetUserClass write SetUserClass;

function UserRoleToStr(ARole: TUserRole): string;
function StrToUserRole(const S: string): TUserRole;

procedure ValidateUsername(const Username: string);
procedure ValidatePassword(const Password: string);
procedure ValidateFirstLastName(const Name: string);

function UserManager: TUserManager; inline;

implementation

var
  FManager: TUserManager = nil;

  FUserClasses: array [TUserRole] of TUserClass = (
    nil,
    nil,
    nil,
    nil,
    nil
    );

function GetUserClass(ARole: TUserRole): TUserClass;
begin
  Result := FUserClasses[ARole];
end;

procedure SetUserClass(ARole: TUserRole; AClass: TUserClass);
begin
  if FManager <> nil then
    raise EInvalidOperation.Create(SCannotChangeRoleClass);
  FUserClasses[ARole] := AClass;
end;

function UserRoleToStr(ARole: TUserRole): string;
begin
  Result := GetEnumName(TypeInfo(TUserRole), Ord(ARole));
end;

function StrToUserRole(const S: string): TUserRole;
var
  R: TUserRole;
begin
  for R in TUserRole do
    if UserRoleToStr(R) = S then
      Exit(R);
  raise EConvertError.Create(SNoSuchUserRole);
end;

procedure ValidateUsername(const Username: string);
const
  MinUsernameLen = 3;
  MaxUsernameLen = 24;
  UsernameAvailableChars = ['A' .. 'Z', 'a' .. 'z', '0' .. '9', '_', '-'];

  function AvailableCharsStr: string; inline;
  begin
    Result := '[''A'' .. ''Z'', ''a'' .. ''z'', ''0'' .. ''9'', ''_'', ''-'']';
  end;

var
  C: char;
begin
  if (Length(Username) < MinUsernameLen) or (Length(Username) > MaxUsernameLen) then
    raise EUserValidate.CreateFmt(SUsernameLength, [MinUsernameLen, MaxUsernameLen]);
  for C in Username do
    if not (C in UsernameAvailableChars) then
      raise EUserValidate.CreateFmt(SUsernameChars, [AvailableCharsStr]);
end;

procedure ValidatePassword(const Password: string);
const
  MinPasswordLen = 8;
  MaxPasswordLen = 64;
begin
  if (Length(Password) < MinPasswordLen) or (Length(Password) > MaxPasswordLen) then
    raise EUserValidate.CreateFmt(SPasswordLength, [MinPasswordLen, MaxPasswordLen]);
end;

procedure ValidateFirstLastName(const Name: string);
const
  MinNameLen = 2;
  MaxNameLen = 42;
begin
  if (Length(Name) < MinNameLen) or (Length(Name) > MaxNameLen) then
    raise EUserValidate.CreateFmt(SNameLength, [MinNameLen, MaxNameLen]);
end;

function UserManager: TUserManager;
begin
  try
    if FManager = nil then
      FManager := TUserManager.Create;
    Result := FManager;
  except
    Result := nil;
    raise;
  end;
end;

{ TUserInfo }

function TUserInfo.GetFirstName: string;
begin
  Result := Storage.ReadString(FullKeyName('firstName'), '');
end;

function TUserInfo.GetID: integer;
begin
  Result := Storage.ReadInteger(FullKeyName('id'), -1);
end;

function TUserInfo.GetLastName: string;
begin
  Result := Storage.ReadString(FullKeyName('lastName'), '');
end;

function TUserInfo.GetLastVisit: TDateTime;
begin
  Result := Storage.ReadFloat(FullKeyName('lastVisit'), 0);
end;

function TUserInfo.GetRegisteredAt: TDateTime;
begin
  Result := Storage.ReadFloat(FullKeyName('registerTime'), 0);
end;

function TUserInfo.GetStorage: TAbstractDataStorage;
begin
  Result := Manager.Storage;
end;

function TUserInfo.GetUserRole: TUserRole;
begin
  Result := StrToUserRole(Storage.ReadString(FullKeyName('role'), ''));
end;

constructor TUserInfo.Create(AManager: TUserManager; const AUsername: string);
begin
  FManager := AManager;
  FUsername := AUsername;
end;

function TUserInfo.FullKeyName(const Key: string): string;
begin
  Result := Manager.FullKeyName(FUsername, Key);
end;

constructor TUserInfo.Create;
begin
  // we don't want the users to be created publicly!
  raise EInvalidOperation.CreateFmt(SCreationPublic, [ClassName]);
end;

{ TUserManager }

function TUserManager.GetUserCount: integer;
begin
  Result := FStorage.ReadInteger('userCount', 0);
end;

procedure TUserManager.IncUserCount(Delta: integer);
begin
  FStorage.WriteInteger('userCount', GetUserCount + Delta);
end;

function TUserManager.NextID: integer;
begin
  Result := FStorage.ReadInteger('lastId', 0);
  Inc(Result);
  FStorage.WriteInteger('lastId', Result);
end;

function TUserManager.UserSection(const Username: string): string;
begin
  Result := 'user-' + Username;
end;

function TUserManager.GetIdKey(AID: integer): string;
begin
  Result := Format('ids.id%d', [AID]);
end;

function TUserManager.IdToUsername(AID: integer): string;
begin
  Result := FStorage.ReadString(GetIdKey(AID), '');
end;

function TUserManager.UsernameToId(const AUsername: string): integer;
begin
  if not UserExists(AUsername) then
    Result := -1
  else
    Result := FStorage.ReadInteger(FullKeyName(AUsername, 'id'), -1);
end;

function TUserManager.FullKeyName(const Username, Key: string): string;
begin
  Result := UserSection(Username) + '.' + Key;
end;

function TUserManager.CreateUserClass(AClass: TUserClass; const Username: string): TUser;
begin
  if AClass = nil then
    Result := nil
  else
    Result := AClass.Create(Self, Username);
end;

function TUserManager.CreateUser(ARole: TUserRole; const Username: string): TUser;
begin
  Result := CreateUserClass(UserClass[ARole], Username);
end;

function TUserManager.CreateDataStorage: TAbstractDataStorage;
begin
  Result := TIniDataStorage.Create(StoragePath);
end;

function TUserManager.UserExists(const Username: string): boolean;
begin
  try
    ValidateUsername(Username);
    Result := FStorage.VariableExists(FullKeyName(Username, 'role'));
  except
    Result := False;
  end;
end;

function TUserManager.LoadUserFromUsername(const Username: string): TUser;
var
  Role: TUserRole;
begin
  Result := nil;
  if not UserExists(Username) then
    Exit;
  Role := GetRole(Username);
  Result := CreateUser(Role, Username);
end;

function TUserManager.LoadUserFromID(AID: integer): TUser;
begin
  Result := LoadUserFromUsername(IdToUsername(AID));
end;

function TUserManager.LoadUserFromSession(ASession: TCustomSession): TUser;
var
  ID, Code: integer;
begin
  Val(ASession.Variables['id'], ID, Code);
  if Code = 0 then
    Result := LoadUserFromID(ID)
  else
    Result := nil;
end;

function TUserManager.GetUserInfo(const Username: string): TUserInfo;
begin
  if not UserExists(Username) then
    raise EUserNotExist.CreateFmt(SUserDoesNotExist, [Username]);
  Result := TUserInfo.Create(Self, Username);
end;

procedure TUserManager.AuthentificateSession(ASession: TCustomSession;
  const Username, Password: string);
var
  AUser: TUser;
  Msg: string;
begin
  if UserName = '' then
    raise EUserAuthentificate.Create(SUsernameEmpty);
  if not UserExists(Username) then
    raise EUserAuthentificate.Create(SInvalidUsernamePassword);
  AUser := LoadUserFromUsername(Username);
  if AUser = nil then
    raise EUserAuthentificate.Create(SUnableLogIn);
  try
    try
      AUser.Authentificate(Password);
    except
      on E: EUserAuthentificate do
      begin
        Msg := E.Message;
        if Msg = SInvalidPassword then
          Msg := SInvalidUsernamePassword;
        raise EUserAuthentificate.Create(Msg);
      end
      else
        raise;
    end;
    AUser.NeedsAuthentification;
    AUser.UpdateLastVisit;
    ASession.Variables['id'] := IntToStr(AUser.ID);
  finally
    FreeAndNil(AUser);
  end;
end;

procedure TUserManager.AddNewUser(const AUsername, APassword, APasswordConfirm: string;
  const AFirstName, ALastName: string; ARole: TUserRole);
var
  UserID: integer;
begin
  // check pre-requisites
  ValidateUsername(AUsername);
  ValidatePassword(APassword);
  ValidateFirstLastName(AFirstName);
  ValidateFirstLastName(ALastName);
  if UserExists(AUsername) then
    raise EUserValidate.CreateFmt(SUserExists, [AUsername]);
  if APassword <> APasswordConfirm then
    raise EUserValidate.Create(SPasswordsNotEqual);
  // create user
  with CreateUser(ARole, AUsername) do
    try
      IncUserCount(+1);
      UserID := NextID;
      FStorage.WriteString(GetIdKey(UserID), AUsername);
      FStorage.WriteInteger(FullKeyName('id'), UserID);
      WriteRole;
      GeneratePassword(APassword);
      BeginUpdate;
      try
        FirstName := AFirstName;
        LastName := ALastName;
      except
        EndUpdate(False);
        raise;
      end;
      EndUpdate(True);
      Storage.WriteFloat(FullKeyName('registerTime'), Now);
      UpdateLastVisit;
    finally
      Free;
    end;
end;

function TUserManager.GetRole(const AUsername: string): TUserRole;
begin
  if not UserExists(AUsername) then
    raise EUserNotExist.Create(SUserDoesNotExist);
  Result := StrToUserRole(FStorage.ReadString(FullKeyName(AUsername, 'role'), ''));
end;

procedure TUserManager.GrantRole(const AUsername: string; ARole: TUserRole);
begin
  if not UserExists(AUsername) then
    raise EUserNotExist.Create(SUserDoesNotExist);
  FStorage.WriteString(FullKeyName(AUsername, 'role'), UserRoleToStr(ARole));
end;

procedure TUserManager.DeleteUser(const AUsername: string);
begin
  if not UserExists(AUsername) then
    raise EUserNotExist.Create(SUserDoesNotExist);
  FStorage.DeleteVariable(GetIdKey(UsernameToId(AUsername)));
  FStorage.DeletePath(UserSection(AUsername));
  IncUserCount(-1);
end;

constructor TUserManager.Create;
begin
  FStorage := CreateDataStorage;
  Scheduler.AttachStorage(FStorage);
  if GetUserCount = 0 then
  begin
    // if there is nobody, create an owner.
    with Config do
      AddNewUser(Owner_DefaultUsername, Owner_DefaultPassword, Owner_DefaultPassword,
        Owner_DefaultFirstName, Owner_DefaultLastName, urOwner);
    FStorage.Commit;
  end;
end;

destructor TUserManager.Destroy;
begin
  FreeAndNil(FStorage);
  inherited Destroy;
end;

{ TUser }

function TUser.GetFirstName: string;
begin
  Result := FInfo.FirstName;
end;

function TUser.GetID: integer;
begin
  Result := FInfo.ID;
end;

function TUser.GetLastName: string;
begin
  Result := FInfo.LastName;
end;

function TUser.GetLastVisit: TDateTime;
begin
  Result := FInfo.LastVisit;
end;

function TUser.GetRegisteredAt: TDateTime;
begin
  Result := FInfo.RegisteredAt;
end;

function TUser.GetStorage: TAbstractDataStorage;
begin
  Result := FInfo.Storage;
end;

procedure TUser.RequireUpdating;
begin
  if not FUpdating then
    raise EInvalidOperation.Create(SUpdateNoUpdation);
end;

procedure TUser.GeneratePassword(const Password: string);
var
  Hash, Salt: string;
begin
  Salt := GenSalt;
  Hash := HashPassword(Password, Salt);
  with Storage do
  begin
    WriteString(FullKeyName('password.hash'), Hash);
    WriteString(FullKeyName('password.salt'), Salt);
  end;
end;

function TUser.CheckPassword(const Password: string): boolean;
var
  Hash, Salt: string;
  FoundHash: string;
begin
  // read hash & salt
  with Storage do
  begin
    Hash := ReadString(FullKeyName('password.hash'), '');
    Salt := ReadString(FullKeyName('password.salt'), '');
  end;
  if (Hash = '') or (Salt = '') then
    raise EUserAuthentificate.Create(SAuthDataNotStored);
  // hash current password
  FoundHash := HashPassword(Password, Salt);
  // compare them
  Result := SlowCompareStrings(Hash, FoundHash);
end;

procedure TUser.SetFirstName(AValue: string);
begin
  RequireUpdating;
  ValidateFirstLastName(AValue);
  FFirstName := AValue;
end;

procedure TUser.SetLastName(AValue: string);
begin
  RequireUpdating;
  ValidateFirstLastName(AValue);
  FLastName := AValue;
end;

function TUser.FullKeyName(const Key: string): string;
begin
  Result := FInfo.FullKeyName(Key);
end;

function TUser.DoGetRole: TUserRole;
begin
  Result := urSimple;
end;

procedure TUser.BeginUpdate;
begin
  if FUpdating then
    raise EInvalidOperation.Create(SUpdateAlreadyUpdating);
  FUpdating := True;
  FFirstName := GetFirstName;
  FLastName := GetLastName;
end;

procedure TUser.EndUpdate(Apply: boolean);
begin
  RequireUpdating;
  if Apply then
  begin
    with Storage do
    begin
      WriteString(FullKeyName('firstName'), FFirstName);
      WriteString(FullKeyName('lastName'), FLastName);
    end;
  end;
end;

procedure TUser.ChangePassword(const OldPassword, NewPassword,
  NewPasswordConfirm: string);
begin
  if OldPassword = '' then
    raise EUserAuthentificate.Create(SPasswordEmpty);
  Authentificate(OldPassword);
  NeedsAuthentification;
  if NewPassword <> NewPasswordConfirm then
    raise EUserValidate.Create(SPasswordsNotEqual);
  ValidatePassword(NewPassword);
  GeneratePassword(NewPassword);
end;

procedure TUser.Authentificate(const Password: string);
begin
  if Password = '' then
    raise EUserAuthentificate.Create(SPasswordEmpty);
  if not CheckPassword(Password) then
    raise EUserAuthentificate.Create(SInvalidPassword);
  FAuthentificated := True;
end;

procedure TUser.NeedsAuthentification;
begin
  if not FAuthentificated then
    raise EUserAuthentificate.Create(SAuthRequired);
  FAuthentificated := False;
end;

constructor TUser.Create(AManager: TUserManager; const AUsername: string);
begin
  FManager := AManager;
  FInfo := TUserInfo.Create(AManager, AUsername);
  FUsername := AUsername;
  FRole := DoGetRole;
  FAuthentificated := False;
  FUpdating := False;
end;

procedure TUser.UpdateLastVisit;
begin
  Storage.WriteFloat(FullKeyName('lastVisit'), Now);
end;

procedure TUser.WriteRole;
begin
  Storage.WriteString(FullKeyName('role'), UserRoleToStr(Role));
end;

constructor TUser.Create;
begin
  // we don't want the users to be created publicly!
  raise EInvalidOperation.CreateFmt(SCreationPublic, [ClassName]);
end;

destructor TUser.Destroy;
begin
  FreeAndNil(FInfo);
  inherited Destroy;
end;

initialization
  UserClass[urSimple] := TUser;

finalization
  FreeAndNil(FManager);

end.
