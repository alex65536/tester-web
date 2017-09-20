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
unit tsmiscwebmodules;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, webmodules, users, HTTPDefs, htmlpages, tswebpagesbase, webstrconsts,
  tswebcrypto, tswebsessions;

type

  { TRedirectLoggedWebModuleHandler }

  TRedirectLoggedWebModuleHandler = class(TWebModuleHandler)
  public
    procedure HandleRequest({%H-}ARequest: TRequest; AResponse: TResponse;
      var Handled: boolean); override;
  end;

  { TDeclineNotLoggedWebModuleHandler }

  TDeclineNotLoggedWebModuleHandler = class(TWebModuleHandler)
  private
    FAllowUsers: TUserRoleSet;
    FRedirectIfFail: boolean;
  public
    procedure HandleRequest({%H-}ARequest: TRequest; {%H-}AResponse: TResponse;
      var {%H-}Handled: boolean); override;
    constructor Create(AAllowUsers: TUserRoleSet = AllUserRoles;
      ARedirectIfFail: boolean = False);
  end;

  { TPostWebModule }

  TPostWebModule = class(THtmlPageWebModule)
  private
    FError: string;
    FSuccess: string;
    procedure PostHandleRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: boolean);
  protected
    property Error: string read FError write FError;
    property Success: string read FSuccess write FSuccess;
    function CanRedirect: boolean; virtual;
    function RedirectLocation: string; virtual;
    procedure DoHandleAuth(ARequest: TRequest); virtual; abstract;
    procedure DoPageAfterConstruction(APage: THtmlPage); override;
    procedure DoBeforeRequest; override;
    procedure DoAfterRequest; override;
    function CompareTokens: boolean;
  public
    procedure AfterConstruction; override;
  end;

  { TPostUserWebModule }

  TPostUserWebModule = class(TPostWebModule)
  private
    FUser: TUser;
  protected
    property User: TUser read FUser;
    procedure DoSessionCreated; override;
    procedure DoAfterRequest; override;
    function CanRedirect: boolean; override;
  public
    procedure AfterConstruction; override;
  end;

implementation

{ TDeclineNotLoggedWebModuleHandler }

procedure TDeclineNotLoggedWebModuleHandler.HandleRequest(ARequest: TRequest;
  AResponse: TResponse; var Handled: boolean);
var
  AUser: TUser;
begin
  AUser := UserManager.LoadUserFromSession(Parent.Session);
  try
    if (AUser = nil) or not (AUser.Role in FAllowUsers) then
    begin
      if FRedirectIfFail then
      begin
        AResponse.Location := DocumentRoot + '/';
        AResponse.Code := 303;
        Handled := True;
      end
      else
        raise EUserAccessDenied.Create(SAccessDenied);
    end;
  finally
    FreeAndNil(AUser);
  end;
end;

constructor TDeclineNotLoggedWebModuleHandler.Create(AAllowUsers: TUserRoleSet;
  ARedirectIfFail: boolean);
begin
  FAllowUsers := AAllowUsers;
  FRedirectIfFail := ARedirectIfFail;
end;

{ TRedirectLoggedWebModuleHandler }

procedure TRedirectLoggedWebModuleHandler.HandleRequest(ARequest: TRequest;
  AResponse: TResponse; var Handled: boolean);
var
  AUser: TUser;
begin
  AUser := UserManager.LoadUserFromSession(Parent.Session);
  if AUser <> nil then
  begin
    FreeAndNil(AUser);
    AResponse.Location := DocumentRoot + '/';
    AResponse.Code := 303;
    Handled := True;
  end;
end;

{ TPostUserWebModule }

procedure TPostUserWebModule.DoSessionCreated;
begin
  inherited DoSessionCreated;
  FUser := UserManager.LoadUserFromSession(Session);
end;

procedure TPostUserWebModule.DoAfterRequest;
begin
  inherited DoAfterRequest;
  FreeAndNil(FUser);
end;

function TPostUserWebModule.CanRedirect: boolean;
begin
  Result := False;
end;

procedure TPostUserWebModule.AfterConstruction;
begin
  inherited AfterConstruction;
  Handlers.Insert(0, TDeclineNotLoggedWebModuleHandler.Create(AllUserRoles, True));
end;

{ TPostWebModule }

function TPostWebModule.CompareTokens: boolean;
var
  GotToken, ExpectedToken: string;
begin
  GotToken := Request.ContentFields.Values['token'];
  ExpectedToken := (Session as TTesterWebSession).Token;
  Result := SlowCompareStrings(GotToken, ExpectedToken);
end;

procedure TPostWebModule.PostHandleRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: boolean);
begin
  if ARequest.Method.ToUpper = 'POST' then
  begin
    if not CompareTokens then
    begin
      FError := SInvalidSessionToken;
      Exit;
    end;
    try
      DoHandleAuth(ARequest);
      // if no exception, we send redirect and don't render that page
      if CanRedirect then
      begin
        AResponse.Location := RedirectLocation;
        AResponse.Code := 303;
        Handled := True;
      end;
    except
      on E: EUserAction do
        FError := E.Message
      else
        raise;
    end;
  end;
end;

function TPostWebModule.CanRedirect: boolean;
begin
  Result := True;
end;

function TPostWebModule.RedirectLocation: string;
begin
  Result := DocumentRoot + '/';
end;

procedure TPostWebModule.DoPageAfterConstruction(APage: THtmlPage);
begin
  inherited DoPageAfterConstruction(APage);
  (APage as IPostHtmlPage).Error := FError;
  (APage as IPostHtmlPage).Success := FSuccess;
end;

procedure TPostWebModule.DoBeforeRequest;
begin
  inherited DoBeforeRequest;
  FError := '';
  FSuccess := '';
end;

procedure TPostWebModule.DoAfterRequest;
begin
  inherited DoAfterRequest;
  FError := '';
  FSuccess := '';
end;

procedure TPostWebModule.AfterConstruction;
begin
  inherited AfterConstruction;
  AddEventHandler(@PostHandleRequest);
end;

end.

