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
unit submissions_tsrun;

{$mode objfpc}{$H+}{$B-}

interface

uses
  Classes, SysUtils, submissions, UTF8Process, tswebcrypto, serverconfig,
  filemanager, LazFileUtils, tswebobservers, webstrconsts;

// Quite unsure what to do with this.
// TODO : Maybe try threadless architecture (?)

type
  ETsRunSubmission = class(Exception);

  TTsRunTestSubmission = class;

  { TTsRunThread }

  TTsRunThread = class(TThread)
  private
    FProcess: TProcessUTF8;
    FSubmission: TTsRunTestSubmission;
    FTsRunExe: string;
    FProblemWorkDir: string;
    FProblemPropsFile: string;
    FTestSrc: string;
    FResFile: string;
    FTestDirName: string;
    FTimeout: integer;
    FExitCode: integer;
    procedure InternalExecute;
  public
    property Terminated;
    property Submission: TTsRunTestSubmission read FSubmission write FSubmission;
    property TsRunExe: string read FTsRunExe write FTsRunExe;
    property ProblemWorkDir: string read FProblemWorkDir write FProblemWorkDir;
    property ProblemPropsFile: string read FProblemPropsFile write FProblemPropsFile;
    property TestSrc: string read FTestSrc write FTestSrc;
    property ResFile: string read FResFile write FResFile;
    property TestDirName: string read FTestDirName write FTestDirName;
    property Timeout: integer read FTimeout write FTimeout;
    property ExitCode: integer read FExitCode;
    procedure Execute; override;
    constructor Create;
  end;

  { TTsRunTestSubmission }

  TTsRunTestSubmission = class(TTestSubmission)
  private
    FThread: TTsRunThread;
  protected
    property Thread: TTsRunThread read FThread;
    procedure Prepare; override;
    {%H-}constructor Create(AManager: TSubmissionManager; AID: integer);
  public
    destructor Destroy; override;
  end;

  TTsRunThreadTerminateMessage = class(TAuthorMessage);

  { TTsRunSubmissionPool }

  TTsRunSubmissionPool = class(TSubmissionPool)
  private
    procedure ThreadTerminate(Sender: TObject);
  protected
    procedure DoAdd(ASubmission: TTestSubmission); override;
    procedure DoDelete(ASubmission: TTestSubmission); override;
    {%H-}constructor Create(AQueue: TSubmissionQueue; AMaxPoolSize: integer = 0);
  end;

  { TTsRunSubmissionQueue }

  TTsRunSubmissionQueue = class(TSubmissionQueue)
  protected
    function CreatePool: TSubmissionPool; override;
    {%H-}constructor Create(AManager: TSubmissionManager);
  end;

  { TTsRunSubmissionManager }

  TTsRunSubmissionManager = class(TSubmissionManager)
  protected
    function CreateQueue: TSubmissionQueue; override;
    function DoCreateTestSubmission(AID: integer): TTestSubmission; override;
  end;

implementation

{ TTsRunSubmissionManager }

function TTsRunSubmissionManager.CreateQueue: TSubmissionQueue;
begin
  Result := TTsRunSubmissionQueue.Create(Self);
end;

function TTsRunSubmissionManager.DoCreateTestSubmission(AID: integer): TTestSubmission;
begin
  Result := TTsRunTestSubmission.Create(Self, AID);
end;

{ TTsRunSubmissionQueue }

function TTsRunSubmissionQueue.CreatePool: TSubmissionPool;
begin
  Result := TTsRunSubmissionPool.Create(Self);
end;

constructor TTsRunSubmissionQueue.Create(AManager: TSubmissionManager);
begin
  inherited Create(AManager);
end;

{ TTsRunSubmissionPool }

constructor TTsRunSubmissionPool.Create(AQueue: TSubmissionQueue;
  AMaxPoolSize: integer);
begin
  inherited Create(AQueue, AMaxPoolSize);
end;

procedure TTsRunSubmissionPool.ThreadTerminate(Sender: TObject);
var
  Submission: TTsRunTestSubmission;
begin
  Submission := (Sender as TTsRunThread).Submission;

  // TODO : Move this into submission!!!
  Submission.Finish(Submission.Thread.ExitCode = 0);
  Submission.FThread.Submission := nil;
  Submission.FThread.OnTerminate := nil;
  Submission.FThread := nil; // it will be freed automatically!


  TriggerTestingFinished(Submission);
end;

procedure TTsRunSubmissionPool.DoAdd(ASubmission: TTestSubmission);
begin
  with (ASubmission as TTsRunTestSubmission).Thread do
  begin
    OnTerminate := @ThreadTerminate;
    Start;
  end;
end;

procedure TTsRunSubmissionPool.DoDelete(ASubmission: TTestSubmission);
begin
  with (ASubmission as TTsRunTestSubmission).Thread do
  begin
    Terminate;
    WaitFor;
  end;
end;

{ TTsRunTestSubmission }

procedure TTsRunTestSubmission.Prepare;
begin
  inherited Prepare;
  if FThread <> nil then
    raise ETsRunSubmission.Create(SThreadAlreadyAssigned);
  FThread := TTsRunThread.Create;
  with FThread do
  begin
    Submission := Self;
    ProblemWorkDir := GetUnpackedFileName;
    ProblemPropsFile := CreateRelativePath(GetPropsFileName, ProblemWorkDir);
    TestSrc := FileName;
    ResFile := ResultsFileName;
  end;
end;

constructor TTsRunTestSubmission.Create(AManager: TSubmissionManager;
  AID: integer);
begin
  inherited Create(AManager, AID);
  FThread := nil;
end;

destructor TTsRunTestSubmission.Destroy;
begin
  {if Thread <> nil then
  begin
    Thread.OnTerminate := nil;
    if (not Thread.Finished) and (not Thread.Terminated) then
      Thread.Terminate;
  end;}
  inherited Destroy;
end;

{ TTsRunThread }

procedure TTsRunThread.Execute;
begin
  FProcess := TProcessUTF8.Create(nil);
  try
    InternalExecute;
  finally
    FreeAndNil(FProcess);
  end;
end;

procedure TTsRunThread.InternalExecute;
begin
  // add parameters
  FProcess.Executable := FTsRunExe;
  with FProcess.Parameters do
  begin
    Add(FProblemWorkDir);
    Add(FProblemPropsFile);
    Add(FTestSrc);
    Add(FResFile);
    Add(FTestDirName);
    Add(IntToStr(FTimeout));
  end;
  if Terminated then
    Exit;
  // run
  FProcess.Execute;
  // wait
  while FProcess.Active do
  begin
    if Terminated then
      FProcess.Terminate(42);
    Sleep(15);
  end;
  // retrieve exit code
  FExitCode := FProcess.ExitCode;
  if FExitCode = 0 then
    FExitCode := FProcess.ExitStatus;
  // cleanup working directory
  TryDeleteDir(AppendPathDelim(GetTempDir) + FTestDirName);
end;

constructor TTsRunThread.Create;
begin
  inherited Create(True, DefaultStackSize);
  FProcess := nil;
  FTsRunExe := Config.Location_TsRunExe;
  FTestDirName := 'tsweb-' + RandomFileName(12);
  FTimeout := 300;
  FreeOnTerminate := True;
end;

end.

