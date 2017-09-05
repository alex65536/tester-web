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
unit index;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, httpdefs, fpHTTP, fpWeb, fphttpapp;

type

  { TIndexModule }

  TIndexModule = class(TFPWebModule)
    procedure DataModuleNewSession(Sender: TObject);
    procedure DataModuleRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure DataModuleSessionExpired(Sender: TObject);
  end;

var
  IndexModule: TIndexModule;

implementation

{$R *.lfm}

{ TIndexModule }

procedure TIndexModule.DataModuleNewSession(Sender: TObject);
begin
  WriteLn('New Session : ', Session.SessionID);
end;

procedure TIndexModule.DataModuleRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  AResponse.Contents.Add('Session id : ' + Session.SessionID + '<br>');
  AResponse.Contents.Add('Var a = ' + Session.Variables['a'] + '<br>');
  AResponse.Contents.Add('Var b = ' + Session.Variables['b'] + '<br>');
  if ARequest.QueryFields.Values['addVar'] <> '' then
  begin
    AResponse.Contents.Add('Added variable : ' + ARequest.QueryFields.Values['addVar'] + '<br>');
    Session.Variables[ARequest.QueryFields.Values['addVar']] := IntToStr(Random(256));
  end;
  if ARequest.QueryFields.Values['stop'] <> '' then
    Session.Terminate;
  if ARequest.QueryFields.Values['kill'] <> '' then
    Application.Terminate;
  Handled := True;
end;

procedure TIndexModule.DataModuleSessionExpired(Sender: TObject);
begin
  WriteLn('Session expired!');
end;

initialization
  RegisterHTTPModule('index', TIndexModule);
end.

