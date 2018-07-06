{
  termlandler - Handle process termination

  Copyright (C) 2018 Alexander Kernozhitsky <sh200105@mail.ru>

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.
}
unit termhandler;

{$mode objfpc}{$H+}
{$WARN 5024 off : Parameter "$1" not used}

interface

uses
  Classes, SysUtils, CustApp;

implementation

{$IfDef Unix}
  uses
    BaseUnix;
{$EndIf}

{$IfDef Windows}
  uses
    Windows;
{$EndIf}

procedure DoHandleTermination;
begin
  if CustomApplication <> nil then
    CustomApplication.Terminate
  else
    Halt(0);
end;

{$IfDef Unix}
  procedure SigHandler(signal: cint; info: psiginfo; context: psigcontext); cdecl;
  begin
    DoHandleTermination;
  end;

  procedure RegisterSigHandler;
  var
    Action: sigactionrec;
  begin
    with Action do
    begin
      sa_handler := @SigHandler;
      FillChar(sa_mask, SizeOf(sa_mask), #0);
      sa_flags := 0;
      {$IfDef Linux}
        sa_restorer := nil;
      {$EndIf}
    end;
    FPSigaction(SIGINT, @Action, nil);
    FPSigaction(SIGTERM, @Action, nil);
    FPSigaction(SIGQUIT, @Action, nil);
  end;
{$Endif}

{$IfDef Windows}
  type
    TTerminateHandler = class
    public
      procedure HandleTermination;
    end;

  procedure TTerminateHandler.HandleTermination;
  begin
    DoHandleTermination;
  end;

  function CtrlHandler(Signal: DWORD): WINBOOL; stdcall;
  var
    Handler: TTerminateHandler;
  begin
    Handler := TTerminateHandler.Create;
    try
      TThread.Synchronize(TThread.CurrentThread, @Handler.HandleTermination);
    finally
      FreeAndNil(Handler);
    end;
    Result := True;
  end;

  procedure RegisterSigHandler;
  begin
    SetConsoleCtrlHandler(@CtrlHandler, True);
  end;
{$EndIf}

initialization
  RegisterSigHandler;
end.

