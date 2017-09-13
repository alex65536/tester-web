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
  serverevents,
  tswebsessions,
  hash_3rdparty,
  fphttpapp,
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
  htmlpagewebmodules,
  users,
  userpages,
  tswebauthfeatures,
  tswebpages,
  tswebmodules;

begin
  {$ifdef Windows}
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
  OnServerTerminate := @Application.Terminate;
  Application.Run;
end.
