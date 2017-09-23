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
  SysUtils, webmodules, users, HTTPDefs, webstrconsts, tswebpagesbase, tsmiscwebmodules;

type

  { TAuthWebModule }

  TAuthWebModule = class(TPostWebModule)
  public
    procedure AfterConstruction; override;
  end;

  { TConfirmPasswordWebModule }

  TConfirmPasswordWebModule = class(TPostUserWebModule)
  private
    FConfirmed: boolean;
    procedure ConfirmedHandleRequest(Sender: TObject; {%H-}ARequest: TRequest;
      AResponse: TResponse; var Handled: boolean);
  protected
    property Confirmed: boolean read FConfirmed;
    procedure ConfirmationSuccess(var ACanRedirect: boolean; var ARedirect: string);
      virtual; abstract;
    procedure DoHandlePost(ARequest: TRequest); override;
    procedure DoBeforeRequest; override;
    procedure DoAfterRequest; override;
  public
    procedure AfterConstruction; override;
  end;

  { TSettingsWebModule }

  TSettingsWebModule = class(TPostUserWebModule)
  protected
    procedure DoHandlePost(ARequest: TRequest); override;
  end;

  { TLogoutWebModule }

  TLogoutWebModule = class(THandlerWebModule)
  protected
    procedure InternalHandleRequest; override;
  public
    procedure AfterConstruction; override;
  end;

implementation

{ TSettingsWebModule }

procedure TSettingsWebModule.DoHandlePost(ARequest: TRequest);
var
  FirstName, LastName: string;
  OldPassword, NewPassword, ConfirmPassword: string;
begin
  with ARequest.ContentFields do
  begin
    FirstName := Values['first-name'];
    LastName := Values['last-name'];
    OldPassword := Values['old-password'];
    NewPassword := Values['new-password'];
    ConfirmPassword := Values['confirm-password'];
  end;
  if (FirstName = '') and (LastName = '') and (OldPassword = '') and
    (NewPassword = '') and (ConfirmPassword = '') then
    raise EUserAction.Create(SSettingsNothingToUpdate);
  User.BeginUpdate;
  try
    if FirstName <> '' then
      User.FirstName := FirstName;
    if LastName <> '' then
      User.LastName := LastName;
    if (OldPassword <> '') or (NewPassword <> '') or (ConfirmPassword <> '') then
      User.ChangePassword(OldPassword, NewPassword, ConfirmPassword);
  except
    User.EndUpdate(False);
    raise;
  end;
  User.EndUpdate(True);
  Success := SSettingsUpdateSuccessful;
end;

{ TAuthWebModule }

procedure TAuthWebModule.AfterConstruction;
begin
  inherited AfterConstruction;
  Handlers.Insert(0, TRedirectLoggedWebModuleHandler.Create);
end;

{ TConfirmPasswordWebModule }

procedure TConfirmPasswordWebModule.ConfirmedHandleRequest(Sender: TObject;
  ARequest: TRequest; AResponse: TResponse; var Handled: boolean);
var
  ACanRedirect: boolean;
  ARedirect: string;
begin
  if FConfirmed then
  begin
    ACanRedirect := True;
    ARedirect := DocumentRoot + '/';
    ConfirmationSuccess(ACanRedirect, ARedirect);
    if ACanRedirect then
    begin
      AResponse.Location := ARedirect;
      AResponse.Code := 303;
      Handled := True;
    end;
  end;
end;

procedure TConfirmPasswordWebModule.DoHandlePost(ARequest: TRequest);
var
  Password: string;
begin
  if User = nil then
    raise EUserAccessDenied.Create(SAccessDenied);
  Password := ARequest.ContentFields.Values['password'];
  User.Authentificate(Password);
  FConfirmed := True;
end;

procedure TConfirmPasswordWebModule.DoBeforeRequest;
begin
  inherited DoBeforeRequest;
  FConfirmed := False;
end;

procedure TConfirmPasswordWebModule.DoAfterRequest;
begin
  inherited DoAfterRequest;
  FConfirmed := False;
end;

procedure TConfirmPasswordWebModule.AfterConstruction;
begin
  inherited AfterConstruction;
  AddEventHandler(@ConfirmedHandleRequest);
end;

{ TLogoutWebModule }

procedure TLogoutWebModule.InternalHandleRequest;
begin
  Session.Terminate;
  Response.Location := DocumentRoot + '/';
  Response.Code := 303;
end;

procedure TLogoutWebModule.AfterConstruction;
begin
  inherited AfterConstruction;
  Handlers.Add(TDeclineNotLoggedWebModuleHandler.Create(AllUserRoles, True));
end;

end.

