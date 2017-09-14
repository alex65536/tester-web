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

{$mode objfpc}{$H+}{$M+}{$inline on}

interface

uses
  Classes, SysUtils, HTTPDefs, datastorages, tswebcrypto, typinfo, webstrconsts,
  serverevents, serverconfig;

type
  EUserAction = class(Exception);
  EUserAuthentificate = class(EUserAction);
  EUserAccessDenied = class(EUserAction);
  EUserValidate = class(EUserAction);
  EUserNotExist = class(EUserAction);

  TUserManager = class;
  TUserRole = (urBlocked, urSimple, urAdmin, urOwner);
  TUserRoleSet = set of TUserRole;

  { TUser }

  TUser = class
  private
    FAuthentificated: boolean;
    FManager: TUserManager;
    FRole: TUserRole;
    FUsername: string;
    FFirstName: string;
    FLastName: string;
    FUpdating: boolean;
    function GetFirstName: string;
    function GetLastName: string;
    procedure RequireUpdating;
    procedure SetFirstName(AValue: string);
    procedure SetLastName(AValue: string);
  protected
    procedure GeneratePassword(const Password: string);
    function CheckPassword(const Password: string): boolean;
    function FullKeyName(const Key: string): string;
    function DoGetRole: TUserRole; virtual;
    // creating users should only be done via TUserManager!
    {%H-}constructor Create(AManager: TUserManager; const AUsername: string); virtual;
  public
    property Manager: TUserManager read FManager;
    property Role: TUserRole read FRole;
    property Username: string read FUsername;
    property FirstName: string read GetFirstName write SetFirstName;
    property LastName: string read GetLastName write SetLastName;
    procedure BeginUpdate; virtual;
    procedure EndUpdate(Apply: boolean); virtual;
    procedure ChangePassword(
      const OldPassword, NewPassword, NewPasswordConfirm: string);
    procedure Authentificate(const Password: string);
    procedure NeedsAuthentification;
    procedure UpdateLastVisit;
    procedure WriteRole;
    constructor Create;
  end;

  TUserClass = class of TUser;

  { TAdminUser }

  TAdminUser = class(TUser)
  protected
    function DoGetRole: TUserRole; override;
  public
    function CanGrantRole(WasRole, NewRole: TUserRole): boolean; virtual;
    procedure GrantRole(const AUsername: string; ARole: TUserRole);
  end;

  { TOwnerUser }

  TOwnerUser = class(TAdminUser)
  protected
    function DoGetRole: TUserRole; override;
  public
    function CanGrantRole(WasRole, NewRole: TUserRole): boolean; override;
    procedure TerminateServer;
  end;

  { TUserManager }

  TUserManager = class
  private
    FStorage: TAbstractDataStorage;
    function GetUserCount: integer;
    procedure IncUserCount;
  protected
    function CreateUserClass(AClass: TUserClass; const Username: string): TUser;
    function CreateUser(ARole: TUserRole; const Username: string): TUser; virtual;
  public
    property Storage: TAbstractDataStorage read FStorage;
    function UserExists(const Username: string): boolean;
    function LoadUserFromUsername(const Username: string): TUser;
    function LoadUserFromSession(ASession: TCustomSession): TUser;
    procedure AuthentificateSession(ASession: TCustomSession;
      const Username, Password: string);
    procedure AddNewUser(const AUsername, APassword, AFirstName, ALastName: string;
      ARole: TUserRole = urSimple);
    function GetRole(const AUsername: string): TUserRole;
    procedure GrantRole(const AUsername: string; ARole: TUserRole);
    constructor Create;
    destructor Destroy; override;
  end;

const
  UserClassesByRole: array [TUserRole] of TUserClass = (
    nil,        // urBlocked : we don't create blocked users!
    TUser,      // urSimple
    TAdminUser, // urAdmin
    TOwnerUser  // urOwner
    );

function UserRoleToStr(ARole: TUserRole): string;
function StrToUserRole(const S: string): TUserRole;

procedure ValidateUsername(const Username: string);
procedure ValidatePassword(const Password: string);
procedure ValidateFirstLastName(const Name: string);

function UserManager: TUserManager; inline;

implementation

var
  FManager: TUserManager;

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
  Result := FManager;
end;

{ TUserManager }

function TUserManager.GetUserCount: integer;
begin
  Result := FStorage.ReadInteger('userCount', 0);
end;

procedure TUserManager.IncUserCount;
begin
  FStorage.WriteInteger('userCount', GetUserCount + 1);
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
  Result := CreateUserClass(UserClassesByRole[ARole], Username);
end;

function TUserManager.UserExists(const Username: string): boolean;
begin
  try
    ValidateUsername(Username);
    Result := FStorage.VariableExists(Username + '.role');
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

function TUserManager.LoadUserFromSession(ASession: TCustomSession): TUser;
begin
  Result := LoadUserFromUsername(ASession.Variables['userName']);
  if Result <> nil then
    Result.UpdateLastVisit;
end;

procedure TUserManager.AuthentificateSession(ASession: TCustomSession;
  const Username, Password: string);
var
  AUser: TUser;
begin
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
        if E.Message = SInvalidPassword then
          E.Message := SInvalidUsernamePassword;
        raise E;
      end
      else
        raise;
    end;
    AUser.NeedsAuthentification;
    AUser.UpdateLastVisit;
    ASession.Variables['userName'] := Username;
  finally
    FreeAndNil(AUser);
  end;
end;

procedure TUserManager.AddNewUser(
  const AUsername, APassword, AFirstName, ALastName: string; ARole: TUserRole);
begin
  // check pre-requisites
  ValidateUsername(AUsername);
  ValidatePassword(APassword);
  ValidateFirstLastName(AFirstName);
  ValidateFirstLastName(ALastName);
  if UserExists(AUsername) then
    raise EUserValidate.CreateFmt(SUserExists, [AUsername]);
  // create user
  with CreateUser(ARole, AUsername) do
    try
      IncUserCount;
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
      UpdateLastVisit;
    finally
      Free;
    end;
end;

function TUserManager.GetRole(const AUsername: string): TUserRole;
begin
  if not UserExists(AUsername) then
    raise EUserNotExist.Create(SUserDoesNotExist);
  Result := StrToUserRole(FStorage.ReadString(AUsername + '.role', ''));
end;

procedure TUserManager.GrantRole(const AUsername: string; ARole: TUserRole);
begin
  if not UserExists(AUsername) then
    raise EUserNotExist.Create(SUserDoesNotExist);
  FStorage.WriteString(AUsername + '.role', UserRoleToStr(ARole));
end;

constructor TUserManager.Create;
begin
  FStorage := TIniDataStorage.Create('users');
  if GetUserCount = 0 then
  begin
    // if there is nobody, create an owner.
    with Config do
      AddNewUser(Owner_DefaultUsername, Owner_DefaultPassword, Owner_DefaultFirstName,
        Owner_DefaultLastName, urOwner);
    FStorage.Commit;
  end;
end;

destructor TUserManager.Destroy;
begin
  FreeAndNil(FStorage);
  inherited Destroy;
end;

{ TOwnerUser }

function TOwnerUser.CanGrantRole(WasRole, NewRole: TUserRole): boolean;
begin
  Result := (WasRole <> urOwner) and (NewRole <> urOwner);
end;

function TOwnerUser.DoGetRole: TUserRole;
begin
  Result := urOwner;
end;

procedure TOwnerUser.TerminateServer;
begin
  if Assigned(OnServerTerminate) then
    OnServerTerminate()
  else
    raise EUserAction.Create(SCannotTerminateServer);
end;

{ TAdminUser }

function TAdminUser.CanGrantRole(WasRole, NewRole: TUserRole): boolean;
begin
  Result := (WasRole <> urOwner) and (WasRole <> urAdmin) and
    (NewRole <> urOwner) and (NewRole <> urAdmin);
end;

function TAdminUser.DoGetRole: TUserRole;
begin
  Result := urAdmin;
end;

procedure TAdminUser.GrantRole(const AUsername: string; ARole: TUserRole);
begin
  NeedsAuthentification;
  if not CanGrantRole(Manager.GetRole(AUsername), ARole) then
    raise EUserAccessDenied.Create(SAccessDenied);
  Manager.GrantRole(AUsername, ARole);
end;

{ TUser }

function TUser.GetFirstName: string;
begin
  Result := Manager.Storage.ReadString(FullKeyName('firstName'), '');
end;

function TUser.GetLastName: string;
begin
  Result := Manager.Storage.ReadString(FullKeyName('lastName'), '');
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
  with Manager.Storage do
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
  with Manager.Storage do
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
  Result := FUsername + '.' + Key;
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
    with Manager.Storage do
    begin
      WriteString(FullKeyName('firstName'), FFirstName);
      WriteString(FullKeyName('lastName'), FLastName);
    end;
  end;
end;

procedure TUser.ChangePassword(
  const OldPassword, NewPassword, NewPasswordConfirm: string);
begin
  Authentificate(OldPassword);
  NeedsAuthentification;
  if NewPassword <> NewPasswordConfirm then
    raise EUserValidate.Create(SNewPasswordsNotEqual);
  ValidatePassword(NewPassword);
  GeneratePassword(NewPassword);
end;

procedure TUser.Authentificate(const Password: string);
begin
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
  FUsername := AUsername;
  FRole := DoGetRole;
  FAuthentificated := False;
  FUpdating := False;
end;

procedure TUser.UpdateLastVisit;
begin
  Manager.Storage.WriteFloat(FullKeyName('lastVisit'), Now);
end;

procedure TUser.WriteRole;
begin
  Manager.Storage.WriteString(FullKeyName('role'), UserRoleToStr(Role));
end;

constructor TUser.Create;
begin
  // we don't want the users to be created publicly!
  raise EInvalidOperation.Create(SUserCreationPublic);
end;

initialization
  FManager := TUserManager.Create;

finalization
  FreeAndNil(FManager);

end.
