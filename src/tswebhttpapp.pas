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
unit tswebhttpapp;

// TODO : Implement logging!

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CustApp, custhttpapp, HTTPDefs, serverconfig;

type

  { TTesterWebHandler }

  TTesterWebHandler = class(TFPHTTPServerHandler)
  private
    FRequest: TRequest;
    FResponse: TResponse;
    FThread: TThread;
    procedure InternalRequestHandler; virtual;
  public
    property Thread: TThread read FThread;
    property Request: TRequest read FRequest;
    property Response: TResponse read FResponse;
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
    constructor Create(AOwner: TComponent; AThread: TThread); overload;
  end;

  { TTesterServerThread }

  TTesterServerThread = class(TThread)
  private
    FWebHandler: TTesterWebHandler;
  protected
    procedure Synchronize(AMethod: TThreadMethod);
  public
    property WebHandler: TTesterWebHandler read FWebHandler;
    procedure Execute; override;
    procedure Terminate;
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt =
      DefaultStackSize);
    destructor Destroy; override;
  end;

  { TTesterWebApplication }

  TTesterWebApplication = class(TCustomApplication)
  private
    FServerActive: boolean;
    FOnIdle: TNotifyEvent;
    FServerThread: TTesterServerThread;
    procedure ServerTerminated(Sender: TObject);
  protected
    property ServerThread: TTesterServerThread read FServerThread;
    procedure DoRun; override;
    procedure SetTitle(const AValue: string); override;
    procedure DoIdle; virtual;
  public
    property OnIdle: TNotifyEvent read FOnIdle write FOnIdle;
    procedure Terminate(AExitCode: Integer); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Application: TTesterWebApplication;

implementation

procedure InitApp;
begin
  Application := TTesterWebApplication.Create(nil);
  if not Assigned(CustomApplication) then
    CustomApplication := Application;
end;

procedure DoneApp;
begin
  if Application = CustomApplication then
    CustomApplication := nil;
  FreeAndNil(Application);
end;

{ TTesterWebApplication }

procedure TTesterWebApplication.ServerTerminated(Sender: TObject);
begin
  FServerActive := False;
end;

procedure TTesterWebApplication.DoRun;
begin
  inherited DoRun;
  FServerActive := True;
  FServerThread.Start;
  while FServerActive do
  begin
    DoIdle;
    CheckSynchronize;
    Sleep(15);
  end;
end;

procedure TTesterWebApplication.SetTitle(const AValue: string);
begin
  inherited SetTitle(AValue);
  FServerThread.WebHandler.Title := AValue;
end;

procedure TTesterWebApplication.DoIdle;
begin
  if Assigned(FOnIdle) then
    FOnIdle(Self);
end;

procedure TTesterWebApplication.Terminate(AExitCode: Integer);
begin
  inherited Terminate(AExitCode);
  FServerThread.Terminate;
end;

constructor TTesterWebApplication.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FServerThread := TTesterServerThread.Create(True);
  FServerThread.OnTerminate := @ServerTerminated;
  with FServerThread.WebHandler do
  begin
    OnLog := @Self.Log;
    Address := Config.Server_Address;
    Port := Config.Server_Port;
    DefaultModuleName := 'index';
    PreferModuleName := True;
  end;
end;

destructor TTesterWebApplication.Destroy;
begin
  FreeAndNil(FServerThread);
  inherited Destroy;
end;

{ TTesterServerThread }

procedure TTesterServerThread.Synchronize(AMethod: TThreadMethod);
begin
  inherited Synchronize(AMethod);
end;

procedure TTesterServerThread.Execute;
begin
  FWebHandler.Run;
end;

procedure TTesterServerThread.Terminate;
begin
  FWebHandler.Terminate;
end;

constructor TTesterServerThread.Create(CreateSuspended: Boolean;
  const StackSize: SizeUInt);
begin
  FWebHandler := TTesterWebHandler.Create(nil, Self);
  inherited Create(CreateSuspended, StackSize);
end;

destructor TTesterServerThread.Destroy;
begin
  FreeAndNil(FWebHandler);
  inherited Destroy;
end;

{ TTesterWebHandler }

procedure TTesterWebHandler.InternalRequestHandler;
begin
  inherited HandleRequest(FRequest, FResponse);
end;

procedure TTesterWebHandler.HandleRequest(ARequest: TRequest;
  AResponse: TResponse);
begin
  FRequest := ARequest;
  FResponse := AResponse;
  try
    TTesterServerThread(FThread).Synchronize(@InternalRequestHandler);
  finally
    FRequest := nil;
    FResponse := nil;
  end;
end;

constructor TTesterWebHandler.Create(AOwner: TComponent; AThread: TThread);
begin
  Create(AOwner);
  FThread := AThread;
end;

initialization
  InitApp;

finalization
  DoneApp;

end.

