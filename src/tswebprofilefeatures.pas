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
unit tswebprofilefeatures;

{$mode objfpc}{$H+}{$B-}

interface

uses
  SysUtils, tswebpagesbase, tswebfeatures, users, htmlpages, webstrconsts,
  userpages, htmlpreprocess, allusers;

type

  { TUserInfoFeature }

  TUserInfoFeature = class(TTesterPageFeature)
  private
    FInfo: TUserInfo;
  protected
    procedure InternalSatisfy; virtual; abstract;
  public
    property Info: TUserInfo read FInfo;
    procedure Satisfy; override;
  end;

  { TProfilePageFeature }

  TProfilePageFeature = class(TUserInfoFeature)
  protected
    procedure InternalSatisfy; override;
  end;

  { TProfileChangeRoleFeature }

  TProfileChangeRoleFeature = class(TUserInfoFeature)
  protected
    procedure InternalSatisfy; override;
  public
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

  { TProfileSettingsBtnFeature }

  TProfileSettingsBtnFeature = class(TUserInfoFeature)
  protected
    procedure InternalSatisfy; override;
  public
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

  { TProfileDeleteBtnFeature }

  TProfileDeleteBtnFeature = class(TUserInfoFeature)
  protected
    procedure InternalSatisfy; override;
  public
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

  { TFindUserPageFeature }

  TFindUserPageFeature = class(TTesterPageFeature)
  public
    procedure Satisfy; override;
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

implementation

{ TFindUserPageFeature }

procedure TFindUserPageFeature.Satisfy;
begin
  with Parent.Variables do
  begin
    ItemsAsText['findUserRequest'] := SFindUserRequest;
    ItemsAsText['findUserLabel'] := SFindUserLabel;
    ItemsAsText['findUserPrompt'] := SFindUserPrompt;
    ItemsAsText['findUserSubmit'] := SFindUserSubmit;
  end;
  LoadPagePart('', 'findUser');
end;

procedure TFindUserPageFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TPostDataFeature);
end;

{ TProfileDeleteBtnFeature }

procedure TProfileDeleteBtnFeature.InternalSatisfy;
var
  User: TUser;
begin
  User := (Parent as TUserPage).User;
  if (User = nil) or not (User is TAdminUser) then
    Exit;
  if not (User as TAdminUser).CanDeleteUser(Info) then
    Exit;
  LoadPagePart('profile', 'profileDeleteUserBtn');
  Parent.Variables.ItemsAsText['profileDeleteUserBtnPrompt'] := SDeleteUserTitle;
end;

procedure TProfileDeleteBtnFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TProfilePageFeature);
end;

{ TProfileSettingsBtnFeature }

procedure TProfileSettingsBtnFeature.InternalSatisfy;
var
  User: TUser;
begin
  User := (Parent as TUserPage).User;
  if (User = nil) or (User.Username <> Info.Username) then
    Exit;
  LoadPagePart('profile', 'profileUpdateSettingsBtn');
  Parent.Variables.ItemsAsText['profileUpdateSettingsBtnPrompt'] := SUpdateSettingsTitle;
end;

procedure TProfileSettingsBtnFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TProfilePageFeature);
end;

{ TProfileChangeRoleFeature }

procedure TProfileChangeRoleFeature.InternalSatisfy;
var
  CanIntoRole: array [TUserRole] of boolean;
  User: TUser;
  Role: TUserRole;
  CanChange: boolean;
  Options: TIndentTaggedStrings;
begin
  User := (Parent as TUserPage).User;
  if User = nil then
    Exit;
  // find available roles
  CanChange := False;
  for Role in TUserRole do
  begin
    if User is TAdminUser then
      CanIntoRole[Role] := (User as TAdminUser).CanGrantRole(Info, Role)
    else
      CanIntoRole[Role] := False;
    if CanIntoRole[Role] then
      CanChange := True;
  end;
  // if cannot change, do not create profile changer
  if not CanChange then
    Exit;
  // fill options
  Options := TIndentTaggedStrings.Create;
  try
    for Role in TUserRole do
      if CanIntoRole[Role] then
      begin
        with Parent.Variables do
        begin
          ItemsAsText['profileNewUserRole'] := UserRoleToStr(Role);
          ItemsAsText['profileNewUserRoleName'] := UserRoleNames[Role];
          if Role = Info.Role then
            ItemsAsText['profileNewUserRoleSelected'] := ' selected'
          else
            ItemsAsText['profileNewUserRoleSelected'] := '';
        end;
        Options.Add(Parent.Preprocessor.PreprocessFile(TemplateLocation('profile', 'profileChangeRoleOption')));
      end;
    Parent.PageParts.SetItemAsStrings('profileRoleOptions', Options);
  finally
    FreeAndNil(Options);
  end;
  // fill other variables
  with Parent.Variables do
  begin
    ItemsAsText['profileChangeRoleKey'] := SProfileChangeRoleKey;
    ItemsAsText['profileDoChangeRole'] := SProfileDoChangeRole;
  end;
  LoadPagePart('profile', 'profileChangeRole');
end;

procedure TProfileChangeRoleFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TProfilePageFeature);
end;

{ TProfilePageFeature }

procedure TProfilePageFeature.InternalSatisfy;
var
  RoleStr: string;
begin
  with Parent.Variables, Info do
  begin
    RoleStr := UserRoleToStr(Role).Substring(2);

    ItemsAsText['profileUserNameKey'] := SProfileUserNameKey;
    ItemsAsText['profileUserName'] := Username;

    ItemsAsText['profileRealNameKey'] := SProfileRealNameKey;
    ItemsAsText['profileFirstName'] := FirstName;
    ItemsAsText['profileLastName'] := LastName;

    ItemsAsText['profileUserRoleKey'] := SProfileUserRoleKey;
    ItemsAsText['profileUserRoleName'] := UserRoleNames[Role];
    ItemsAsText['profileUserRole'] := RoleStr;
    ItemsAsText['profileUserRoleLower'] := RoleStr.ToLower;

    ItemsAsText['profileRegisterTimeKey'] := SProfileRegisterTimeKey;
    ItemsAsText['profileRegisterTime'] := FormatDateTime(SPreferredDateTimeFormat, RegisteredAt);

    ItemsAsText['profileLoginTimeKey'] := SProfileLoginTimeLey;
    ItemsAsText['profileLoginTime'] := FormatDateTime(SPreferredDateTimeFormat, LastVisit);
  end;
  LoadPagePart('profile', 'profile');
end;

{ TUserInfoFeature }

procedure TUserInfoFeature.Satisfy;
begin
  FInfo := (Parent as IUserInfo).GetInfo;
  if FInfo = nil then
    raise EInvalidPointer.CreateFmt(SMustNonNil, ['Info']);
  try
    InternalSatisfy;
  finally
    FreeAndNil(FInfo);
  end;
end;

end.

