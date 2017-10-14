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
unit tswebfeatures;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, htmlpages, htmlpreprocess, webstrconsts, tswebpagesbase,
  navbars, users, userpages, tswebsessions, serverconfig;

type

  { TPageBaseFeature }

  TPageBaseFeature = class(THtmlPageFeature)
  public
    procedure Satisfy; override;
    procedure DependsOn({%H-}ADependencies: THtmlPageFeatureList); override;
  end;

  { TTesterPageFeature }

  TTesterPageFeature = class(THtmlPageFeature)
  protected
    procedure LoadPagePart(const ALocation, AName: string; AVarName: string = '');
  public
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

  { THeaderFeature }

  THeaderFeature = class(TTesterPageFeature)
  public
    procedure Satisfy; override;
  end;

  { TFooterFeature }

  TFooterFeature = class(TTesterPageFeature)
  public
    procedure Satisfy; override;
  end;

  { TContentFeature }

  TContentFeature = class(TTesterPageFeature)
  public
    procedure Satisfy; override;
  end;

  { TNavBarFeature }

  TNavBarFeature = class(TTesterPageFeature)
  public
    procedure Satisfy; override;
  end;

  { TUserInfoFeature }

  TUserInfoFeature = class(TTesterPageFeature)
  public
    procedure Satisfy; override;
  end;

  { TContentHeaderFeature }

  TContentHeaderFeature = class(TTesterPageFeature)
  public
    procedure Satisfy; override;
  end;

  { TContentFooterFeature }

  TContentFooterFeature = class(TTesterPageFeature)
  public
    procedure Satisfy; override;
  end;

  { TUserBarFeature }

  TUserBarFeature = class(TTesterPageFeature)
  public
    procedure Satisfy; override;
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

  { TPostDataFeature }

  TPostDataFeature = class(TTesterPageFeature)
    procedure Satisfy; override;
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

  { TSessionTokenFeature }

  TSessionTokenFeature = class(TTesterPageFeature)
    procedure Satisfy; override;
  end;

implementation

{ TContentFooterFeature }

procedure TContentFooterFeature.Satisfy;
begin
  LoadPagePart('', 'contentFooter');
end;

{ TContentHeaderFeature }

procedure TContentHeaderFeature.Satisfy;
begin
  LoadPagePart('', 'contentHeader');
end;

{ TSessionTokenFeature }

procedure TSessionTokenFeature.Satisfy;
begin
  with Parent.Variables do
    ItemsAsText['sessionToken'] := (Parent.Session as TTesterWebSession).Token;
  LoadPagePart('', 'formToken');
end;

{ TPostDataFeature }

procedure TPostDataFeature.Satisfy;
begin
  with Parent.Variables do
  begin
    ItemsAsText['authError'] := (Parent as IPostHtmlPage).Error;
    ItemsAsText['authSuccess'] := (Parent as IPostHtmlPage).Success;
  end;
end;

procedure TPostDataFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TSessionTokenFeature);
end;

{ TUserInfoFeature }

procedure TUserInfoFeature.Satisfy;
var
  User: TUser;
  RoleStr: string;
begin
  User := (Parent as TUserPage).User;
  if User = nil then
    Exit;
  with Parent.Variables, User do
  begin
    RoleStr := UserRoleToStr(Role);
    Delete(RoleStr, 1, 2);
    ItemsAsText['userRoleName'] := UserRoleNames[Role];
    ItemsAsText['userRole'] := RoleStr;
    ItemsAsText['userRoleLower'] := RoleStr.ToLower;
    ItemsAsText['userName'] := Username;
    ItemsAsText['firstName'] := FirstName;
    ItemsAsText['lastName'] := LastName;
  end;
end;

{ TUserBarFeature }

procedure TUserBarFeature.Satisfy;
var
  User: TUser;
begin
  User := (Parent as TUserPage).User;
  if User = nil then
  begin
    // guest session
    with Parent.Variables do
    begin
      ItemsAsText['loggedAsGuest'] := SLoggedAsGuest;
      ItemsAsText['doLogIn'] := SUserDoLogIn;
      ItemsAsText['doRegister'] := SUserDoRegister;
    end;
    LoadPagePart('', 'userGuest', 'userBarInner');
  end
  else
  begin
    // user logged in
    with Parent.Variables do
    begin
      ItemsAsText['loggedAsUser'] := SLoggedAsUser;
      ItemsAsText['loggedUserLink'] :=
        (Parent as TTesterHtmlPage).GenerateUserLink(User.Info);
      ItemsAsText['doViewProfile'] := SUserDoViewProfile;
      ItemsAsText['doLogout'] := SUserDoLogOut;
    end;
    LoadPagePart('', 'userLogged', 'userBarInner');
  end;
  LoadPagePart('', 'userBox');
end;

procedure TUserBarFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TUserInfoFeature);
  ADependencies.Add(THeaderFeature);
end;

{ TNavBarFeature }

procedure TNavBarFeature.Satisfy;
var
  Strings: TIndentTaggedStrings;
  NavBarIntf: IPageNavBar;
begin
  NavBarIntf := Parent as IPageNavBar;
  Strings := TIndentTaggedStrings.Create;
  try
    NavBarIntf.NavBar.GetContents(Strings);
    Parent.PageParts.SetItemAsStrings('nav', Strings);
  finally
    FreeAndNil(Strings);
  end;
end;

{ TContentFeature }

procedure TContentFeature.Satisfy;
var
  Strings: TIndentTaggedStrings;
begin
  Strings := TIndentTaggedStrings.Create;
  try
    with (Parent as TContentHtmlPage) do
    begin
      GetInnerContents(Strings);
      PageParts.SetItemAsStrings('content', Strings);
    end;
  finally
    FreeAndNil(Strings);
  end;
end;

{ TFooterFeature }

procedure TFooterFeature.Satisfy;
begin
  with Parent do
  begin
    Variables.ItemsAsText['copyright'] := SCopyright;
    Variables.ItemsAsText['license'] := SLicenseNotice;
    Variables.ItemsAsText['github'] := SSourcesNotice;
  end;
  LoadPagePart('', 'footer');
end;

{ THeaderFeature }

procedure THeaderFeature.Satisfy;
begin
  LoadPagePart('', 'header');
end;

{ TPageBaseFeature }

procedure TPageBaseFeature.Satisfy;
begin
  with Parent.Variables do
  begin
    ItemsAsText['documentRoot'] := DocumentRoot;
    ItemsAsText['dataRoot'] := DataRoot;
    if Parent is TTesterHtmlPage then
    begin
      ItemsAsText['pageHeader'] := (Parent as TTesterHtmlPage).Title;
      ItemsAsText['title'] := (Parent as TTesterHtmlPage).Title;
    end;
    ItemsAsText['fileTooBigAlertHead'] := SFileTooBigAlertHead;
    ItemsAsText['fileTooBigAlertTail'] := SFileTooBigAlertTail;
    ItemsAsText['nameMaxLength'] := IntToStr(Config.Strings_MaxNameLength);
    ItemsAsText['titleMaxLength'] := IntToStr(Config.Strings_MaxTitleLength);
    ItemsAsText['passwordMaxLength'] := IntToStr(Config.Strings_MaxPasswordLength);
  end;
end;

procedure TPageBaseFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  // do nothing
end;

{ TTesterPageFeature }

procedure TTesterPageFeature.LoadPagePart(const ALocation, AName: string;
  AVarName: string);
begin
  if AVarName = '' then
    AVarName := AName;
  Parent.PageParts.SetFromFile(AVarName, TemplateLocation(ALocation, AName));
end;

procedure TTesterPageFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  ADependencies.Add(TPageBaseFeature);
end;

end.

