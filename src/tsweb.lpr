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
program tsweb;

{$mode objfpc}{$H+}

uses
  heaptrc,
  commitscheduler,
  Classes,
  serverevents,
  tswebsessions,
  hash_3rdparty,
  fphttpapp,
  custhttpapp,
  htmlpreprocess,
  escaping,
  webstrconsts,
  SysUtils,
  datastorages,
  serverconfig,
  tswebcrypto,
  dateutils,
  fpwebfile,
  htmlpages,
  tswebfeatures,
  fpmimetypes,
  navbars,
  tswebpagesbase,
  tswebnavbars,
  webmodules,
  users,
  userpages,
  tswebauthfeatures,
  tswebpages,
  tswebmodules,
  authwebmodules,
  tswebprofilefeatures,
  tsmiscwebmodules,
  editableobjects;

type
  TOpenHandlerApplication = class(THTTPApplication)
  public
    property Handler: TFPHTTPServerHandler read HTTPHandler;
  end;

// TODO : This OnIdle works only when request appears.
// TODO : Invent a better way that ALWAYS synchronizes.
procedure SetIdleEvent(AEvent: TNotifyEvent);
var
  Handler: TFPHTTPServerHandler;
begin
  Handler := TOpenHandlerApplication(Application).Handler;
  Handler.OnIdle := AEvent;
end;

{$ifdef Windows}
var
  F: TextFile;
{$endif}

begin
  {$ifdef Windows}
  AssignFile(F, 'heap.trc');
  Rewrite(F);
  CloseFile(F);
  SetHeapTraceOutput('heap.trc');
  {$endif}

  MimeTypes.AddType('text/html', 'html');
  MimeTypes.AddType('text/css', 'css');
  MimeTypes.AddType('text/javascript', 'js');
  MimeTypes.AddType('image/png', 'png');
  RegisterFileLocation('data', Config.Location_DataDir);

  Application.Title := 'Tester Web';
  Application.Port := 8080;
  Application.DefaultModuleName := 'index';
  Application.PreferModuleName := True;
  Application.Initialize;
  SetIdleEvent(@Scheduler.IdleEventHandler);
  OnServerTerminate := @Application.Terminate;
  Application.Run;
end.
