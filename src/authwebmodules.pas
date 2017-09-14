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
unit authwebmodules;

{$mode objfpc}{$H+}{$B-}

interface

uses
  SysUtils, webmodules, users, HTTPDefs, webstrconsts, htmlpages, tswebpages;

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
  public
    procedure HandleRequest({%H-}ARequest: TRequest; {%H-}AResponse: TResponse;
      var {%H-}Handled: boolean); override;
    constructor Create(AAllowUsers: TUserRoleSet = [Low(TUserRoleSet) .. High(TUserRoleSet)]);
  end;

  { TAuthWebModule }

  TAuthWebModule = class(THtmlPageWebModule)
  private
    FError: string;
    procedure PostHandleRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: boolean);
  protected
    procedure DoHandleAuth(ARequest: TRequest); virtual; abstract;
    procedure DoPageAfterConstruction(APage: THtmlPage); override;
    procedure DoBeforeRequest; override;
    procedure DoAfterRequest; override;
  public
    procedure AfterConstruction; override;
  end;

  { TLogoutWebModule }

  TLogoutWebModule = class(THandlerWebModule)
  protected
    procedure InternalHandleRequest; override;
  public
    procedure AfterConstruction; override;
  end;

implementation

{ TLogoutWebModule }

procedure TLogoutWebModule.InternalHandleRequest;
begin
  Session.Terminate;
  Response.Location := '/';
  Response.Code := 303;
end;

procedure TLogoutWebModule.AfterConstruction;
begin
  inherited AfterConstruction;
  Handlers.Add(TDeclineNotLoggedWebModuleHandler.Create);
end;

{ TAuthWebModule }

procedure TAuthWebModule.PostHandleRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: boolean);
begin
  if ARequest.Method.ToUpper = 'POST' then
  begin
    try
      DoHandleAuth(ARequest);
      // if no exception, we send redirect and don't render that page
      AResponse.Location := '/';
      AResponse.Code := 303;
      Handled := True;
    except
      on E: EUserAction do
        FError := E.Message
      else
        raise;
    end;
  end;
end;

procedure TAuthWebModule.DoPageAfterConstruction(APage: THtmlPage);
begin
  inherited DoPageAfterConstruction(APage);
  (APage as TAuthHtmlPage).Error := FError;
end;

procedure TAuthWebModule.DoBeforeRequest;
begin
  inherited DoBeforeRequest;
  FError := '';
end;

procedure TAuthWebModule.DoAfterRequest;
begin
  inherited DoAfterRequest;
  FError := '';
end;

procedure TAuthWebModule.AfterConstruction;
begin
  inherited AfterConstruction;
  Handlers.Add(TRedirectLoggedWebModuleHandler.Create);
  AddEventHandler(@PostHandleRequest);
end;

{ TDeclineNotLoggedWebModuleHandler }

procedure TDeclineNotLoggedWebModuleHandler.HandleRequest(ARequest: TRequest;
  AResponse: TResponse; var Handled: boolean);
var
  AUser: TUser;
begin
  AUser := UserManager.LoadUserFromSession(Parent.Session);
  try
    if (AUser = nil) or not (AUser.Role in FAllowUsers) then
      raise EUserAccessDenied.Create(SAccessDenied);
  finally
    FreeAndNil(AUser);
  end;
end;

constructor TDeclineNotLoggedWebModuleHandler.Create(AAllowUsers: TUserRoleSet);
begin
  FAllowUsers := AAllowUsers;
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
    AResponse.Location := '/';
    AResponse.Code := 303;
    Handled := True;
  end;
end;

end.

