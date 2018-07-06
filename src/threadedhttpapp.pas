{
  This file is part of Tester Web

  Copyright (C) 2017-2018 Alexander Kernozhitsky <sh200105@mail.ru>

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
unit threadedhttpapp;

// TODO : Implement logging!

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CustApp, custhttpapp, HTTPDefs, tswebconfig, errorpages,
  webstrconsts, dateutils, logging, termhandler;

type
  EThreadedHttpApplication = class(Exception);

  THttpApplicationHandlerThread = class;

  { TThreadedHttpHandler }

  TThreadedHttpHandler = class(TFPHTTPServerHandler)
  private
    FRequest: TRequest;
    FResponse: TResponse;
    FThread: THttpApplicationHandlerThread;
    procedure InternalRequestHandler; virtual;
  public
    property Thread: THttpApplicationHandlerThread read FThread;
    property Request: TRequest read FRequest;
    property Response: TResponse read FResponse;
    procedure ShowRequestException(AResponse: TResponse; AException: Exception); override;
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
    constructor Create(AOwner: TComponent; AThread: THttpApplicationHandlerThread); overload;
  end;

  { THttpApplicationHandlerThread }

  THttpApplicationHandlerThread = class(TThread)
  private
    FWebHandler: TThreadedHttpHandler;
  public
    property WebHandler: TThreadedHttpHandler read FWebHandler;
    procedure Execute; override;
    procedure Terminate;
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt =
      DefaultStackSize);
    destructor Destroy; override;
  end;

  { TThreadedHttpApplication }

  TThreadedHttpApplication = class(TCustomApplication)
  private
    FOnIdle: TNotifyEvent;
    FServerThread: THttpApplicationHandlerThread;
    function GetDefaultModuleName: string;
    procedure ServerTerminated(Sender: TObject);
    procedure SetDefaultModuleName(AValue: string);
  protected
    property ServerThread: THttpApplicationHandlerThread read FServerThread;
    procedure DoLog(EventType: TEventType; const Msg: String); override;
    procedure DoRun; override;
    procedure DoTerminate; virtual;
    procedure SetTitle(const AValue: string); override;
    procedure DoIdle; virtual;
  public
    property OnIdle: TNotifyEvent read FOnIdle write FOnIdle;
    property DefaultModuleName: string read GetDefaultModuleName write SetDefaultModuleName;
    procedure Initialize; override;
    procedure ShowException(E: Exception); override;
    procedure Run;
    procedure Terminate(AExitCode: Integer); override;
    constructor Create(AOwner: TComponent); override;
  end;

var
  Application: TThreadedHttpApplication;

implementation

procedure InitApp;
begin
  Application := TThreadedHttpApplication.Create(nil);
  if not Assigned(CustomApplication) then
    CustomApplication := Application;
end;

procedure DoneApp;
begin
  if Application = CustomApplication then
    CustomApplication := nil;
  FreeAndNil(Application);
end;

{ TThreadedHttpApplication }

procedure TThreadedHttpApplication.ServerTerminated(Sender: TObject);
var
  Thread: THttpApplicationHandlerThread;
begin
  Thread := Sender as THttpApplicationHandlerThread;
  if Thread.FatalException <> nil then
  begin
    LogFatal(SServerTerminateException);
    LogException(lsFatal, Thread.FatalException as Exception);
  end;
  FServerThread := nil;
end;

function TThreadedHttpApplication.GetDefaultModuleName: string;
begin
  Result := FServerThread.WebHandler.DefaultModuleName;
end;

procedure TThreadedHttpApplication.SetDefaultModuleName(AValue: string);
begin
  if not FServerThread.Suspended then
    raise EThreadedHttpApplication.CreateFmt(SServerRunning, ['DefaultModuleName']);
  FServerThread.WebHandler.DefaultModuleName := AValue;
end;

procedure TThreadedHttpApplication.DoLog(EventType: TEventType;
  const Msg: String);
const
  TranslateTable: array [TEventType] of TLogEventSeverity = (
    lsInfo,
    lsNote,
    lsWarning,
    lsError,
    lsInfo
  );
begin
  LogWrite(TranslateTable[EventType], Msg);
end;

procedure TThreadedHttpApplication.DoRun;
begin
  inherited DoRun;
  DoIdle;
  CheckSynchronize(1);
end;

procedure TThreadedHttpApplication.DoTerminate;
var
  Time: TDateTime;
begin
  LogNote(SWebHandlerWait);
  FServerThread.Terminate;
  // wait for the thread
  Time := Now;
  while (FServerThread <> nil) and (MilliSecondSpan(Time, Now) < 3000) do
    CheckSynchronize(1);
  if FServerThread <> nil then
  begin
    LogError(SServerKilling);
    KillThread(FServerThread.Handle);
  end;
  LogNote(SServerTerminated);
end;

procedure TThreadedHttpApplication.SetTitle(const AValue: string);
begin
  inherited SetTitle(AValue);
  FServerThread.WebHandler.Title := AValue;
end;

procedure TThreadedHttpApplication.DoIdle;
begin
  if Assigned(FOnIdle) then
    FOnIdle(Self);
end;

procedure TThreadedHttpApplication.Initialize;
begin
  LogNote(SServerStarted);
  inherited Initialize;
  FServerThread.Start;
end;

procedure TThreadedHttpApplication.ShowException(E: Exception);
begin
  LogFatal(SUnhandledException);
  LogException(lsFatal, E);
end;

procedure TThreadedHttpApplication.Run;
begin
  inherited Run;
  DoTerminate;
end;

procedure TThreadedHttpApplication.Terminate(AExitCode: Integer);
begin
  if Terminated then
    Exit;
  inherited Terminate(AExitCode);
  LogNote(SServerTerminating, [ExitCode]);
end;

constructor TThreadedHttpApplication.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FServerThread := THttpApplicationHandlerThread.Create(True);
  FServerThread.OnTerminate := @ServerTerminated;
  FServerThread.FreeOnTerminate := True;
  with FServerThread.WebHandler do
  begin
    OnLog := @Self.Log;
    Address := Config.Server_Address;
    Port := Config.Server_Port;
    QueueSize := Config.Server_QueueSize;
    PreferModuleName := True;
    AllowDefaultModule := True;
    LegacyRouting := True; // improved router in fcl-web from FPC 3.0.4 breaks Tester Web
    AcceptIdleTimeout := 1;
  end;
end;

{ THttpApplicationHandlerThread }

procedure THttpApplicationHandlerThread.Execute;
begin
  FWebHandler.Run;
end;

procedure THttpApplicationHandlerThread.Terminate;
begin
  FWebHandler.Terminate;
end;

constructor THttpApplicationHandlerThread.Create(CreateSuspended: Boolean;
  const StackSize: SizeUInt);
begin
  inherited Create(CreateSuspended, StackSize);
  FWebHandler := TThreadedHttpHandler.Create(nil, Self);
end;

destructor THttpApplicationHandlerThread.Destroy;
begin
  FreeAndNil(FWebHandler);
  inherited Destroy;
end;

{ TThreadedHttpHandler }

procedure TThreadedHttpHandler.InternalRequestHandler;
var
  Time: TDateTime;
begin
  Time := Now;
  inherited HandleRequest(FRequest, FResponse);
  LogInfo(Format(SRequestShowFormat, [
    FRequest.Method, FRequest.URI, FRequest.RemoteAddress,
    MilliSecondsBetween(Time, Now)
  ]));
end;

procedure TThreadedHttpHandler.ShowRequestException(AResponse: TResponse;
  AException: Exception);
var
  Page: TErrorHtmlPage;
begin
  LogError(SExceptionCaught);
  LogException(lsError, AException);
  try
    Page := TDefaultErrorPage.Create;
    try
      Page.Response := AResponse;
      Page.ExceptObj := AException;
      Page.UpdateResponse;
    finally
      FreeAndNil(Page);
    end;
  except
    on E: Exception do
    begin
      LogFatal(SExceptionWhileException);
      LogException(lsFatal, E);
      // exception while showing an exception :)
      inherited ShowRequestException(AResponse, AException);
    end;
  end;
end;

procedure TThreadedHttpHandler.HandleRequest(ARequest: TRequest;
  AResponse: TResponse);
begin
  FRequest := ARequest;
  FResponse := AResponse;
  try
    FThread.Synchronize(@InternalRequestHandler);
  finally
    FRequest := nil;
    FResponse := nil;
  end;
end;

constructor TThreadedHttpHandler.Create(AOwner: TComponent;
  AThread: THttpApplicationHandlerThread);
begin
  Create(AOwner);
  FThread := AThread;
end;

initialization
  InitApp;

finalization
  DoneApp;

end.

