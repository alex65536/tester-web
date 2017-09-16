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
unit tswebauthfeatures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tswebpagesbase, tswebfeatures, users, htmlpages,
  webstrconsts;

type

  { TAuthFormFeature }

  TAuthFormFeature = class(TTesterPageFeature)
  public
    procedure Satisfy; override;
  end;

  { TAuthLoginFormFeature }

  TAuthLoginFormFeature = class(TTesterPageFeature)
    procedure Satisfy; override;
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

  { TAuthRegisterFormFeature }

  TAuthRegisterFormFeature = class(TTesterPageFeature)
    procedure Satisfy; override;
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

  { TAuthConfirmPasswordFeature }

  TAuthConfirmPasswordFeature = class(TTesterPageFeature)
    procedure Satisfy; override;
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

implementation

{ TAuthConfirmPasswordFeature }

procedure TAuthConfirmPasswordFeature.Satisfy;
begin
  LoadPagePart('auth', 'authConfirmPassword', 'authForm');
  with Parent.Variables do
  begin
    ItemsAsText['authFillRequest'] := SConfirmPasswordRequest;
    ItemsAsText['authSubmit'] := SConfirmPasswordSubmit;
  end;
end;

procedure TAuthConfirmPasswordFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TAuthFormFeature);
end;

{ TAuthRegisterFormFeature }

procedure TAuthRegisterFormFeature.Satisfy;
begin
  LoadPagePart('auth', 'authRegister', 'authForm');
  with Parent.Variables do
  begin
    ItemsAsText['authFillRequest'] := SRegisterRequest;
    ItemsAsText['authSubmit'] := SRegisterSubmit;
  end;
end;

procedure TAuthRegisterFormFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TAuthFormFeature);
end;

{ TAuthLoginFormFeature }

procedure TAuthLoginFormFeature.Satisfy;
begin
  LoadPagePart('auth', 'authLogin', 'authForm');
  with Parent.Variables do
  begin
    ItemsAsText['authFillRequest'] := SLoginRequest;
    ItemsAsText['authSubmit'] := SLoginSubmit;
  end;
end;

procedure TAuthLoginFormFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TAuthFormFeature);
end;

{ TAuthFormFeature }

procedure TAuthFormFeature.Satisfy;
begin
  LoadPagePart('auth', 'auth', 'authContent');
  LoadPagePart('auth', 'authInner');
  LoadPagePart('auth', 'authNameItem');
  LoadPagePart('auth', 'authUsernameItem');
  LoadPagePart('auth', 'authFirstNameItem');
  LoadPagePart('auth', 'authLastNameItem');
  LoadPagePart('auth', 'authPasswordItem');
  LoadPagePart('auth', 'authEnterPasswordItem');
  with Parent.Variables do
  begin
    ItemsAsText['authError'] := (Parent as IAuthHtmlPage).Error;
    ItemsAsText['authUsername'] := SAuthUsername;
    ItemsAsText['authUsernamePrompt'] := SAuthUsernamePrompt;
    ItemsAsText['authFirstName'] := SAuthFirstName;
    ItemsAsText['authFirstNamePrompt'] := SAuthFirstNamePrompt;
    ItemsAsText['authLastName'] := SAuthLastName;
    ItemsAsText['authLastNamePrompt'] := SAuthLastNamePrompt;
    ItemsAsText['authEnterPassword'] := SAuthEnterPassword;
    ItemsAsText['authEnterPasswordPrompt'] := SAuthEnterPasswordPrompt;
    ItemsAsText['authRetypePassword'] := SAuthRetypePassword;
    ItemsAsText['authRetypePasswordPrompt'] := SAuthRetypePasswordPrompt;
  end;
end;

end.

