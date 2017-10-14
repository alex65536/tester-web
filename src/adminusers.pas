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
unit adminusers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, editableobjects, users, serverevents, webstrconsts;

type

  { TAdminUser }

  TAdminUser = class(TEditorUser)
  protected
    function DoGetRole: TUserRole; override;
    function DoCanGrantRole(Target: TUserInfo; ARole: TUserRole): boolean; virtual;
    function DoCanDeleteUser({%H-}Target: TUserInfo): boolean; virtual;
  public
    function CanGrantRole(Target: TUserInfo; ARole: TUserRole): boolean;
    function CanDeleteUser(Target: TUserInfo): boolean;
    procedure GrantRole(Target: TUserInfo; ARole: TUserRole);
    procedure DeleteUser(Target: TUserInfo);
  end;

  { TOwnerUser }

  TOwnerUser = class(TAdminUser)
  protected
    function DoGetRole: TUserRole; override;
    function DoCanGrantRole(Target: TUserInfo; ARole: TUserRole): boolean; override;
    function DoCanDeleteUser({%H-}Target: TUserInfo): boolean; override;
  public
    procedure TerminateServer;
  end;

implementation

{ TOwnerUser }

function TOwnerUser.DoGetRole: TUserRole;
begin
  Result := urOwner;
end;

function TOwnerUser.DoCanGrantRole(Target: TUserInfo; ARole: TUserRole): boolean;
begin
  Result := (Target.Role <> urOwner) and (ARole <> urOwner);
end;

function TOwnerUser.DoCanDeleteUser(Target: TUserInfo): boolean;
begin
  Result := True;
end;

procedure TOwnerUser.TerminateServer;
begin
  NeedsAuthentification;
  if Assigned(OnServerTerminate) then
    OnServerTerminate()
  else
    raise EUserAction.Create(SCannotTerminateServer);
end;

{ TAdminUser }

function TAdminUser.DoCanGrantRole(Target: TUserInfo; ARole: TUserRole): boolean;
begin
  Result := (Target.Role <> urOwner) and (Target.Role <> urAdmin) and
    (ARole <> urOwner) and (ARole <> urAdmin);
end;

function TAdminUser.DoCanDeleteUser(Target: TUserInfo): boolean;
begin
  Result := False;
end;

function TAdminUser.DoGetRole: TUserRole;
begin
  Result := urAdmin;
end;

function TAdminUser.CanGrantRole(Target: TUserInfo; ARole: TUserRole): boolean;
begin
  if Target.Username = Username then
    Result := False
  else
    Result := DoCanGrantRole(Target, ARole);
end;

function TAdminUser.CanDeleteUser(Target: TUserInfo): boolean;
begin
  if Target.Username = Username then
    Result := False
  else
    Result := DoCanDeleteUser(Target);
end;

procedure TAdminUser.GrantRole(Target: TUserInfo; ARole: TUserRole);
begin
  NeedsAuthentification;
  if not CanGrantRole(Target, ARole) then
    raise EUserAccessDenied.Create(SAccessDenied);
  Manager.GrantRole(Target.Username, ARole);
end;

procedure TAdminUser.DeleteUser(Target: TUserInfo);
begin
  NeedsAuthentification;
  if not CanDeleteUser(Target) then
    raise EUserAccessDenied.Create(SAccessDenied);
  Manager.DeleteUser(Target.Username);
end;

initialization
  UserClass[urAdmin] := TAdminUser;
  UserClass[urOwner] := TOwnerUser;

end.

