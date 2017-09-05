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
unit tswebsessions;

// The code here is based on the implementation of iniwebsession.

{$mode objfpc}{$H+}{$B-}

interface

uses
  Classes, SysUtils, datastorages, serverconfig, fphttp, HTTPDefs, webstrconsts,
  DateUtils;

const
  StoragePath = 'sessions';
  SessionCookieName = 'TsWebSession';
  SessionActiveKey = 'active';
  SessionTimeKey = 'startTime';
  SessionPeriodKey = 'expirePeriod';
  SessionVarsKey = 'vars';

type

  { TTesterWebSession }

  TTesterWebSession = class(TCustomSession)
  private
    FStorage: TAbstractDataStorage;
    FTerminated: boolean;
    FSessionID: string;
  protected
    procedure CheckSession;
    procedure RemoveSession;
    procedure AddSessionToStorage;
    procedure UpdateSessionTime;
    function VarNameToStoragePath(const VarName: string): string;
    function GenerateSessionID: string;
    function GetSessionID: String; override;
    function GetSessionVariable(VarName: string): string; override;
    procedure SetSessionVariable(VarName: string; const AValue: string); override;
  public
    property Storage: TAbstractDataStorage read FStorage;
    procedure Terminate; override;
    procedure UpdateResponse(AResponse: TResponse); override;
    procedure InitSession(ARequest: TRequest; OnNewSession, OnExpired: TNotifyEvent);
      override;
    procedure InitResponse(AResponse: TResponse); override;
    procedure RemoveVariable(VariableName: string); override;
    constructor Create(AOwner: TComponent); override;
    constructor Create(AOwner: TComponent; AStorage: TAbstractDataStorage); virtual;
  end;
  TTesterWebSessionClass = class of TTesterWebSession;

  { TTesterWebSessionFactory }

  TTesterWebSessionFactory = class(TSessionFactory)
  private
    FStorage: TAbstractDataStorage;
  protected
    function DoCreateSession(ARequest: TRequest): TCustomSession; override;
    procedure DoCleanupSessions; override;
    procedure DoDoneSession(var ASession: TCustomSession); override;
    procedure CreateDataStorage; virtual;
  public
    function SessionExpired(const ASessionCookie: string): boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  TesterWebSessionClass: TTesterWebSessionClass = TTesterWebSession;

implementation

uses
  tswebcrypto;

{ TTesterWebSessionFactory }

function TTesterWebSessionFactory.DoCreateSession(ARequest: TRequest): TCustomSession;
var
  Session: TTesterWebSession;
begin
  Session := TTesterWebSessionClass.Create(nil, FStorage);
  Session.SessionCookie := SessionCookie;
  Session.SessionCookiePath := SessionCookiePath;
  Result := Session;
end;

procedure TTesterWebSessionFactory.DoCleanupSessions;
var
  Sessions: TStringList;
  Session: string;
begin
  Sessions := FStorage.GetRootElements;
  try
    for Session in Sessions do
      if SessionExpired(Session) then
        FStorage.DeletePath(Session);
  finally
    FreeAndNil(Sessions);
  end;
end;

procedure TTesterWebSessionFactory.DoDoneSession(var ASession: TCustomSession);
begin
  FreeAndNil(ASession);
end;

procedure TTesterWebSessionFactory.CreateDataStorage;
begin
  FStorage := TIniDataStorage.Create(StoragePath);
end;

function TTesterWebSessionFactory.SessionExpired(const ASessionCookie: string): boolean;
var
  StartTime: TDateTime;
  Period: integer;
  ExpireTime: TDateTime;
begin
  if not FStorage.ReadBool(SessionCookie + '.' + SessionActiveKey, False) then
  begin
    Result := False;
    Exit;
  end;
  StartTime := FStorage.ReadFloat(SessionCookie + '.' + SessionTimeKey, 0.0);
  Period := FStorage.ReadInteger(SessionCookie + '.' + SessionTimeKey, Config.Session_AliveTime);
  ExpireTime := IncMinute(StartTime, Period);
  Result := CompareDateTime(Now, ExpireTime) > 0;
end;

constructor TTesterWebSessionFactory.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateDataStorage;
end;

destructor TTesterWebSessionFactory.Destroy;
begin
  FreeAndNil(FStorage);
  inherited Destroy;
end;

{ TTesterWebSession }

procedure TTesterWebSession.CheckSession;
begin
  if FStorage.VariableExists(GetSessionID + '.' + SessionActiveKey) then
    Exit;
  if FTerminated then
    raise EWebSessionError.Create(SErrSessionTerminated)
  else
    raise EWebSessionError.Create(SErrNoSession);
end;

procedure TTesterWebSession.RemoveSession;
begin
  FStorage.DeletePath(GetSessionID);
end;

procedure TTesterWebSession.AddSessionToStorage;
begin
  FStorage.WriteBool(GetSessionID + '.' + SessionActiveKey, True);
  FStorage.WriteFloat(GetSessionID + '.' + SessionTimeKey, Now);
  FStorage.WriteFloat(GetSessionID + '.' + SessionPeriodKey, TimeOutMinutes);
end;

procedure TTesterWebSession.UpdateSessionTime;
begin
  FStorage.WriteFloat(GetSessionID + '.' + SessionTimeKey, Now);
end;

function TTesterWebSession.VarNameToStoragePath(const VarName: string): string;
begin
  Result := GetSessionID + '.' + SessionVarsKey +  '.' + VarName;
end;

function TTesterWebSession.GenerateSessionID: string;
begin
  Result := 's' + RandomSequenceBase64(Config.Session_IDLength);
  // replace some characters for better compatibility with data stroages
  Result.Replace('+', '_');
  Result.Replace('/', '-');
end;

function TTesterWebSession.GetSessionID: String;
begin
  if FSessionID = '' then
    Result := GenerateSessionID
  else
    Result := FSessionID;
end;

function TTesterWebSession.GetSessionVariable(VarName: string): string;
begin
  CheckSession;
  Result := FStorage.ReadString(VarNameToStoragePath(VarName), '');
end;

procedure TTesterWebSession.SetSessionVariable(VarName: string; const AValue: string);
begin
  CheckSession;
  FStorage.WriteString(VarNameToStoragePath(VarName), AValue);
end;

procedure TTesterWebSession.Terminate;
begin
  RemoveSession;
  FTerminated := True;
end;

procedure TTesterWebSession.UpdateResponse(AResponse: TResponse);
begin
  // do nothing
end;

procedure TTesterWebSession.InitSession(ARequest: TRequest;
  OnNewSession, OnExpired: TNotifyEvent);
var
  CookieValue: string;
  Factory: TTesterWebSessionFactory;
begin
  Factory := SessionFactory as TTesterWebSessionFactory;
  // first initialize all session-dependent properties to their default, because
  // in Apache-modules or fcgi programs the session-instance is re-used
  FSessionID := '';
  FTerminated := False;
  // check session cookie
  if SessionCookie = '' then
    SessionCookie := SessionCookieName;
  CookieValue := ARequest.ContentFields.Values[SessionCookie];
  FSessionID := CookieValue;
  // check if session expired
  if (CookieValue <> '') and Factory.SessionExpired(CookieValue) then
  begin
    if Assigned(OnExpired) then
      OnExpired(Self);
    RemoveSession;
    FSessionID := '';
    CookieValue := '';
  end;
  // create new session if necessary
  if CookieValue = '' then
  begin
    if Assigned(OnNewSession) then
      OnNewSession(Self);
    GetSessionID;
    AddSessionToStorage;
  end
  else
    UpdateSessionTime;
end;

procedure TTesterWebSession.InitResponse(AResponse: TResponse);
Var
  C : TCookie;
begin
  C := AResponse.Cookies.FindCookie(SessionCookie);
  if C = nil then
  begin
    C := AResponse.Cookies.Add;
    C.Name := SessionCookie;
  end;
  if FTerminated then
    C.Value := ''
  else
  begin
    C.Value := FSessionID;
    C.Path := SessionCookiePath;
  end;
end;

procedure TTesterWebSession.RemoveVariable(VariableName: string);
begin
  FStorage.DeleteVariable(VarNameToStoragePath(VariableName));
end;

constructor TTesterWebSession.Create(AOwner: TComponent);
begin
  TimeOutMinutes := Config.Session_AliveTime;
  inherited Create(AOwner);
end;

constructor TTesterWebSession.Create(AOwner: TComponent;
  AStorage: TAbstractDataStorage);
begin
  FStorage := AStorage;
  Create(AOwner);
end;

initialization
  SessionFactoryClass := TTesterWebSessionFactory;

end.
