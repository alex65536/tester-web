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
unit tswebmodules;

{$mode objfpc}{$H+}{$B-}

interface

uses
  SysUtils, Classes, webmodules, tswebnavbars, navbars, htmlpreprocess, fphttp,
  htmlpages, tswebpages, HTTPDefs, users, webstrconsts, authwebmodules,
  tswebprofilefeatures, tswebpagesbase, tsmiscwebmodules, allusers, userpages,
  tswebfeatures;

type

  { TDefaultNavBar }

  TDefaultNavBar = class(TTesterNavBar)
  protected
    procedure DoCreateElements; override;
  end;

  { TSimpleHtmlPage }

  TSimpleHtmlPage = class(TDefaultHtmlPage)
  private
    FTextContent: string;
  protected
    function CreateNavBar: TNavBar; override;
    procedure DoGetInnerContents(Strings: TIndentTaggedStrings); override;
  public
    property TextContent: string read FTextContent write FTextContent;
  end;

  { TProfileHtmlPage }

  TProfileHtmlPage = class(TDefaultHtmlPage, IUserInfo)
  private
    FUsername: string;
  protected
    function CreateNavBar: TNavBar; override;
    procedure DoGetInnerContents(Strings: TIndentTaggedStrings); override;
    procedure AddFeatures; override;
    function GetInfo: TUserInfo;
  public
    constructor Create; override;
    constructor Create(const AUsername: string);
  end;

  { TNavLoginPage }

  TNavLoginPage = class(TLoginHtmlPage)
  protected
    function CreateNavBar: TNavBar; override;
  end;

  { TNavRegisterPage }

  TNavRegisterPage = class(TRegisterHtmlPage)
  protected
    function CreateNavBar: TNavBar; override;
  end;

  { TNavConfirmPasswordPage }

  TNavConfirmPasswordPage = class(TConfirmPasswordHtmlPage)
  protected
    function CreateNavBar: TNavBar; override;
  end;

  { TNavSettingsPage }

  TNavSettingsPage = class(TSettingsHtmlPage)
  protected
    function CreateNavBar: TNavBar; override;
  end;

  { TFindUserPage }

  TFindUserPage = class(TPostHtmlPage)
  protected
    function CreateNavBar: TNavBar; override;
    procedure AddFeatures; override;
    procedure DoGetInnerContents(Strings: TIndentTaggedStrings); override;
  public
    procedure AfterConstruction; override;
  end;

  { TIndexWebModule }

  TIndexWebModule = class(THtmlPageWebModule)
  protected
    function DoCreatePage: THtmlPage; override;
  end;

  { TLoginWebModule }

  TLoginWebModule = class(TAuthWebModule)
  protected
    function DoCreatePage: THtmlPage; override;
    procedure DoHandlePost(ARequest: TRequest); override;
  end;

  { TRegisterWebModule }

  TRegisterWebModule = class(TAuthWebModule)
  protected
    function DoCreatePage: THtmlPage; override;
    procedure DoHandlePost(ARequest: TRequest); override;
  end;

  { TNavConfirmPasswordWebModule }

  TNavConfirmPasswordWebModule = class(TConfirmPasswordWebModule)
  protected
    function DoCreatePage: THtmlPage; override;
  end;

  { TUserActionPerformModule }

  TUserActionPerformModule = class(TNavConfirmPasswordWebModule)
  private
    FInfo: TUserInfo;
  protected
    property Info: TUserInfo read FInfo;
    procedure ConfirmationSuccess(var ACanRedirect: boolean;
      var ARedirect: string); override;
    procedure DoPerformAction(var ACanRedirect: boolean;
      var ARedirect: string); virtual; abstract;
  end;

  { TChangeUserRoleWebModule }

  TChangeUserRoleWebModule = class(TUserActionPerformModule)
  protected
    procedure DoPerformAction(var ACanRedirect: boolean; var ARedirect: string);
      override;
  public
    procedure AfterConstruction; override;
  end;

  { TDeleteUserWebModule }

  TDeleteUserWebModule = class(TUserActionPerformModule)
  protected
    procedure DoPerformAction(var ACanRedirect: boolean; var ARedirect: string); override;
  public
    procedure AfterConstruction; override;
  end;

  { TNavSettingsWebModule }

  TNavSettingsWebModule = class(TSettingsWebModule)
  protected
    function DoCreatePage: THtmlPage; override;
  end;

  { TProfileWebModule }

  TProfileWebModule = class(THtmlPageWebModule)
  protected
    function DoCreatePage: THtmlPage; override;
  end;

  { TKillServerWebModule }

  TKillServerWebModule = class(TConfirmPasswordWebModule)
  private
    FKilled: boolean;
  protected
    procedure ConfirmationSuccess(var ACanRedirect: boolean;
      var {%H-}ARedirect: string); override;
    procedure DoBeforeRequest; override;
    function DoCreatePage: THtmlPage; override;
    procedure DoPageAfterConstruction(APage: THtmlPage); override;
    procedure InternalHandleRequest; override;
  public
    procedure AfterConstruction; override;
  end;

  { TFindUserWebModule }

  TFindUserWebModule = class(TPostWebModule)
  private
    FUsername: string;
  protected
    function CanRedirect: boolean; override;
    function RedirectLocation: string; override;
    procedure DoHandlePost(ARequest: TRequest); override;
    procedure DoBeforeRequest; override;
    function DoCreatePage: THtmlPage; override;
  end;

implementation

{ TFindUserWebModule }

function TFindUserWebModule.CanRedirect: boolean;
begin
  Result := FUsername <> '';
end;

function TFindUserWebModule.RedirectLocation: string;
begin
  Result := DocumentRoot + '/profile?user=' + FUsername;
end;

procedure TFindUserWebModule.DoHandlePost(ARequest: TRequest);
var
  Username: string;
begin
  Username := ARequest.ContentFields.Values['user'];
  if not UserManager.UserExists(Username) then
    raise EUserNotExist.CreateFmt(SUserDoesNotExist, [Username]);
  FUsername := Username;
end;

procedure TFindUserWebModule.DoBeforeRequest;
begin
  inherited DoBeforeRequest;
  FUsername := '';
end;

function TFindUserWebModule.DoCreatePage: THtmlPage;
begin
  Result := TFindUserPage.Create;
end;

{ TFindUserPage }

function TFindUserPage.CreateNavBar: TNavBar;
begin
  Result := TDefaultNavBar.Create(Self);
end;

procedure TFindUserPage.AddFeatures;
begin
  inherited AddFeatures;
  AddFeature(TUserBarFeature);
  AddFeature(TFindUserPageFeature);
end;

procedure TFindUserPage.DoGetInnerContents(Strings: TIndentTaggedStrings);
begin
  Strings.Text := '~+#findUser;';
end;

procedure TFindUserPage.AfterConstruction;
begin
  inherited AfterConstruction;
  Title := SFindUserTitle;
end;

{ TDeleteUserWebModule }

procedure TDeleteUserWebModule.DoPerformAction(var ACanRedirect: boolean;
  var ARedirect: string);
begin
  (User as TAdminUser).DeleteUser(Info);
  ACanRedirect := True;
  ARedirect := DocumentRoot + '/';
end;

procedure TDeleteUserWebModule.AfterConstruction;
begin
  inherited AfterConstruction;
  Handlers.Insert(0, TDeclineNotLoggedWebModuleHandler.Create([urAdmin, urOwner]));
end;

{ TUserActionPerformModule }

procedure TUserActionPerformModule.ConfirmationSuccess(
  var ACanRedirect: boolean; var ARedirect: string);
begin
  FInfo := UserManager.GetUserInfo(Request.QueryFields.Values['user']);
  try
    DoPerformAction(ACanRedirect, ARedirect);
  finally
    FreeAndNil(FInfo);
  end;
end;

{ TNavConfirmPasswordWebModule }

function TNavConfirmPasswordWebModule.DoCreatePage: THtmlPage;
begin
  Result := TNavConfirmPasswordPage.Create;
end;

{ TNavSettingsWebModule }

function TNavSettingsWebModule.DoCreatePage: THtmlPage;
begin
  Result := TNavSettingsPage.Create;
end;

{ TNavSettingsPage }

function TNavSettingsPage.CreateNavBar: TNavBar;
begin
  Result := TDefaultNavBar.Create(Self);
end;

{ TChangeUserRoleWebModule }

procedure TChangeUserRoleWebModule.DoPerformAction(var ACanRedirect: boolean;
  var ARedirect: string);
var
  Role: TUserRole;
begin
  Role := StrToUserRole(Request.QueryFields.Values['newrole']);
  (User as TAdminUser).GrantRole(Info, Role);
  ACanRedirect := True;
  ARedirect := DocumentRoot + '/profile?user=' + Info.Username;
end;

procedure TChangeUserRoleWebModule.AfterConstruction;
begin
  inherited AfterConstruction;
  Handlers.Insert(0, TDeclineNotLoggedWebModuleHandler.Create([urAdmin, urOwner]));
end;

{ TNavConfirmPasswordPage }

function TNavConfirmPasswordPage.CreateNavBar: TNavBar;
begin
  Result := TDefaultNavBar.Create(Self);
end;

{ TProfileWebModule }

function TProfileWebModule.DoCreatePage: THtmlPage;
begin
  Result := TProfileHtmlPage.Create(Request.QueryFields.Values['user']);
end;

{ TProfileHtmlPage }

function TProfileHtmlPage.CreateNavBar: TNavBar;
begin
  Result := TDefaultNavBar.Create(Self);
end;

procedure TProfileHtmlPage.DoGetInnerContents(Strings: TIndentTaggedStrings);
begin
  Strings.Text := '~#+profile;';
end;

procedure TProfileHtmlPage.AddFeatures;
begin
  inherited AddFeatures;
  AddFeature(TProfilePageFeature);
  AddFeature(TProfileChangeRoleFeature);
  AddFeature(TProfileSettingsBtnFeature);
  AddFeature(TProfileDeleteBtnFeature);
end;

function TProfileHtmlPage.GetInfo: TUserInfo;
begin
  if FUsername = '' then
  begin
    if User = nil then
      raise EUserAccessDenied.Create(SAccessDenied)
    else
      Result := UserManager.GetUserInfo(User.Username);
  end
  else
    Result := UserManager.GetUserInfo(FUsername);
end;

constructor TProfileHtmlPage.Create;
begin
  Create('');
end;

constructor TProfileHtmlPage.Create(const AUsername: string);
begin
  inherited Create;
  FUsername := AUsername;
  if FUsername = '' then
    Title := SProfile
  else
    Title := Format(SProfileOf, [FUsername]);
end;

{ TNavRegisterPage }

function TNavRegisterPage.CreateNavBar: TNavBar;
begin
  Result := TDefaultNavBar.Create(Self);
end;

{ TNavLoginPage }

function TNavLoginPage.CreateNavBar: TNavBar;
begin
  Result := TDefaultNavBar.Create(Self);
end;

{ TKillServerWebModule }

procedure TKillServerWebModule.ConfirmationSuccess(var ACanRedirect: boolean;
  var ARedirect: string);
begin
  (User as TOwnerUser).TerminateServer;
  FKilled := True;
  ACanRedirect := False;
end;

procedure TKillServerWebModule.DoBeforeRequest;
begin
  inherited DoBeforeRequest;
  FKilled := False;
end;

function TKillServerWebModule.DoCreatePage: THtmlPage;
begin
  Result := TNavConfirmPasswordPage.Create;
end;

procedure TKillServerWebModule.DoPageAfterConstruction(APage: THtmlPage);
begin
  inherited DoPageAfterConstruction(APage);
  (APage as TNavConfirmPasswordPage).Title := SKillPageTitle;
end;

procedure TKillServerWebModule.InternalHandleRequest;
begin
  if not FKilled then
    inherited InternalHandleRequest;
end;

procedure TKillServerWebModule.AfterConstruction;
begin
  Handlers.Add(TDeclineNotLoggedWebModuleHandler.Create([urOwner]));
  inherited AfterConstruction;
end;

{ TRegisterWebModule }

function TRegisterWebModule.DoCreatePage: THtmlPage;
begin
  Result := TNavRegisterPage.Create;
end;

procedure TRegisterWebModule.DoHandlePost(ARequest: TRequest);
var
  Username, Password, Password2, FirstName, LastName: string;
begin
  Username := ARequest.ContentFields.Values['username'];
  Password := ARequest.ContentFields.Values['password'];
  Password2 := ARequest.ContentFields.Values['password2'];
  FirstName := ARequest.ContentFields.Values['first-name'];
  LastName := ARequest.ContentFields.Values['last-name'];
  UserManager.AddNewUser(Username, Password, Password2, FirstName, LastName);
  UserManager.AuthentificateSession(Session, Username, Password);
end;

{ TLoginWebModule }

function TLoginWebModule.DoCreatePage: THtmlPage;
begin
  Result := TNavLoginPage.Create;
end;

procedure TLoginWebModule.DoHandlePost(ARequest: TRequest);
var
  Username, Password: string;
begin
  Username := ARequest.ContentFields.Values['username'];
  Password := ARequest.ContentFields.Values['password'];
  UserManager.AuthentificateSession(Session, Username, Password);
end;

{ TIndexWebModule }

function TIndexWebModule.DoCreatePage: THtmlPage;
begin
  Result := TSimpleHtmlPage.Create;
  try
    with Result as TSimpleHtmlPage do
    begin
      Title := SMainPage;
      TextContent := STsWebGreeting;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

{ TDefaultNavBar }

procedure TDefaultNavBar.DoCreateElements;
var
  User: TUser;
begin
  // retreive user
  if Parent is TUserPage then
    User := (Parent as TUserPage).User
  else
    User := nil;
  // add user-independent pages
  AddElement(SMainPage, '~documentRoot;/main');
  AddElement(SFindUserTitle, '~documentRoot;/find-user');
  // add solve contest page (for all users)
  if User <> nil then
  begin
    AddSplitter;
    AddElement(SContestSolveTitle, '~documentRoot;/solve');
  end;
  // add contest/problem lists (for editors)
  if (User <> nil) and (User is TEditorUser) then
  begin
    AddSplitter;
    AddElement(SProblemList, '~documentRoot;/problems');
    AddElement(SContestList, '~documentRoot;/contests');
  end;
  // add "kill server" (for owner)
  if (User <> nil) and (User is TOwnerUser) then
  begin
    AddSplitter;
    AddElement(SKillPageTitle, '~documentRoot;/kill');
  end;
  // add login/register for guests, and profile/logout for users
  if Parent is TUserPage then
  begin
    AddSplitter;
    if User = nil then
    begin
      AddElement(SUserDoLogIn, '~documentRoot;/login');
      AddElement(SUserDoRegister, '~documentRoot;/register');
    end
    else
    begin
      AddElement(SProfile, '~documentRoot;/profile');
      AddElement(SUpdateSettingsTitle, '~documentRoot;/settings');
      AddElement(SUserDoLogOut, '~documentRoot;/logout');
    end;
  end;
end;

{ TSimpleHtmlPage }

function TSimpleHtmlPage.CreateNavBar: TNavBar;
begin
  Result := TDefaultNavBar.Create(Self);
end;

procedure TSimpleHtmlPage.DoGetInnerContents(Strings: TIndentTaggedStrings);
begin
  Strings.Text := FTextContent;
end;

initialization
  RegisterHTTPModule('main', TIndexWebModule, True);
  RegisterHTTPModule('login', TLoginWebModule, True);
  RegisterHTTPModule('logout', TLogoutWebModule, True);
  RegisterHTTPModule('register', TRegisterWebModule, True);
  RegisterHTTPModule('kill', TKillServerWebModule, True);
  RegisterHTTPModule('profile', TProfileWebModule, True);
  RegisterHTTPModule('change-role', TChangeUserRoleWebModule, True);
  RegisterHTTPModule('settings', TNavSettingsWebModule, True);
  RegisterHTTPModule('delete-user', TDeleteUserWebModule, True);
  RegisterHTTPModule('find-user', TFindUserWebModule, True);

end.
