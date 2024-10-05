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
program tsweb;

{$mode objfpc}{$H+}

{$Define UseCThreads}

uses {$IfDef UNIX} {$IfDef UseCThreads}
  cthreads,
  cmem, {$EndIf} {$EndIf}
  commitscheduler,
  Classes,
  ts_testerutil,
  ts_testerbase,
  serverevents,
  tswebsessions,
  hash_3rdparty,
  threadedhttpapp,
  allusers,
  htmlpreprocess,
  escaping,
  webstrconsts,
  SysUtils,
  datastorages,
  serverconfig,
  tswebconfig,
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
  editableobjects,
  adminusers,
  tswebobservers,
  tswebdirectories,
  problems,
  tswebeditablefeatures,
  tswebeditablemodules,
  tswebproblemmodules,
  tswebeditableelements,
  archivemanager,
  filemanager,
  tswebproblemfeatures,
  tswebproblempages,
  downloadhandlers,
  webstrutils,
  submissionlanguages,
  errorpages,
  tsweberrorpages,
  submissions,
  tswebutils,
  submissions_tsrun,
  tswebmanagers,
  submissioninfo,
  tswebsubmissionfeatures,
  tswebsubmissionelements,
  objectshredder,
  tswebsubmissionmodules,
  contests,
  tswebcontestfeatures,
  tswebcontestpages,
  tswebcontestmodules,
  tswebeditablepages,
  tswebcontestelements,
  tswebdatetimefeatures,
  contestproblems,
  standings,
  tswebsolvefeatures,
  tswebsolvepages,
  tswebsolvemodules,
  tswebsolveelements,
  tswebicon,
  userpreferences,
  tswebpasswords,
  logging,
  tswebfile,
  LazFileUtils;

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
  MimeTypes.AddType('image/vnd.microsoft.icon', 'ico');
  RegisterFileLocation('data', ExpandFileNameUTF8(Config.Location_DataDir));

  Application.Title := 'Tester Web';
  Application.OnIdle := @IdleMessenger.OnIdle;
  Application.DefaultModuleName := 'main';
  OnServerTerminate := @Application.Terminate;
  Application.Initialize;
  Application.Run;
end.
