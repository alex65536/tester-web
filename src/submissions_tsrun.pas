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
  filemanager, FileUtil, LazFileUtils, tswebobservers, webstrconsts,
  objectshredder, contestproblems;

type
  ETsRunSubmission = class(Exception);

  ETsRunThread = class(Exception);

  { TTsRunThread }

  TTsRunThread = class(TThread)
  private
    FProcess: TProcessUTF8;
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
    FFinishTrigger: boolean;
    procedure ThreadTerminate(Sender: TObject);
  protected
    property Thread: TTsRunThread read FThread;
    {%H-}constructor Create(AManager: TSubmissionManager; AID: integer);
  public
    procedure StartTesting; virtual;
    procedure TerminateTesting; virtual;
  end;

  TTsRunTestingFinishedMessage = class(TTestSubmissionMessage);

  { TTsRunSubmissionPool }

  TTsRunSubmissionPool = class(TSubmissionPool, IMessageSubscriber)
  protected
    procedure DoAdd(ASubmission: TTestSubmission); override;
    procedure DoDelete(ASubmission: TTestSubmission); override;
    procedure MessageReceived(AMessage: TAuthorMessage);
    {%H-}constructor Create(AQueue: TSubmissionQueue; AMaxPoolSize: integer = 0);
  end;

  { TTsRunSubmissionQueue }

  TTsRunSubmissionQueue = class(TSubmissionQueue)
  private
    FShredder: TObjectShredder;
  protected
    procedure DoDestroySubmission(ASubmission: TTestSubmission); override;
    function CreatePool: TSubmissionPool; override;
    {%H-}constructor Create(AManager: TSubmissionManager);
  public
    destructor Destroy; override;
  end;

  { TTsRunContestSubmissionManager }

  TTsRunContestSubmissionManager = class(TContestSubmissionManager)
  protected
    function CreateQueue: TSubmissionQueue; override;
    function DoCreateTestSubmission(AID: integer): TTestSubmission; override;
  end;

implementation

{ TTsRunContestSubmissionManager }

function TTsRunContestSubmissionManager.CreateQueue: TSubmissionQueue;
begin
  Result := TTsRunSubmissionQueue.Create(Self);
end;

function TTsRunContestSubmissionManager.DoCreateTestSubmission(AID: integer): TTestSubmission;
begin
  Result := TTsRunTestSubmission.Create(Self, AID);
end;

{ TTsRunSubmissionQueue }

procedure TTsRunSubmissionQueue.DoDestroySubmission(ASubmission: TTestSubmission);
begin
  FShredder.Add(ASubmission);
end;

function TTsRunSubmissionQueue.CreatePool: TSubmissionPool;
begin
  Result := TTsRunSubmissionPool.Create(Self);
end;

constructor TTsRunSubmissionQueue.Create(AManager: TSubmissionManager);
begin
  FShredder := TObjectShredder.Create;
  inherited Create(AManager);
end;

destructor TTsRunSubmissionQueue.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FShredder);
end;

{ TTsRunSubmissionPool }

constructor TTsRunSubmissionPool.Create(AQueue: TSubmissionQueue;
  AMaxPoolSize: integer);
begin
  inherited Create(AQueue, AMaxPoolSize);
end;

procedure TTsRunSubmissionPool.DoAdd(ASubmission: TTestSubmission);
begin
  ASubmission.Subscribe(Self);
  (ASubmission as TTsRunTestSubmission).StartTesting;
end;

procedure TTsRunSubmissionPool.DoDelete(ASubmission: TTestSubmission);
begin
  (ASubmission as TTsRunTestSubmission).TerminateTesting;
  ASubmission.Unsubscribe(Self);
end;

procedure TTsRunSubmissionPool.MessageReceived(AMessage: TAuthorMessage);
begin
  if AMessage is TTsRunTestingFinishedMessage then
    TriggerTestingFinished((AMessage as TTsRunTestingFinishedMessage).Submission);
end;

{ TTsRunTestSubmission }

procedure TTsRunTestSubmission.ThreadTerminate(Sender: TObject);
var
  FinishSuccess: boolean;
begin
  FinishSuccess := FThread.FatalException = nil;
  FThread.OnTerminate := nil;
  FThread := nil; // it will be freed automatically!
  if FFinishTrigger then
  begin
    Finish(FinishSuccess);
    Broadcast(TTsRunTestingFinishedMessage.Create.AddSubmission(Self).AddSender(Self).Lock);
  end;
end;

procedure TTsRunTestSubmission.StartTesting;
begin
  if FThread <> nil then
    raise ETsRunSubmission.Create(SThreadAlreadyAssigned);
  FThread := TTsRunThread.Create;
  FFinishTrigger := True;
  with FThread do
  begin
    OnTerminate := @ThreadTerminate;
    ProblemWorkDir := GetUnpackedFileName;
    ProblemPropsFile := CreateRelativePath(GetPropsFileName, ProblemWorkDir);
    TestSrc := FileName;
    ResFile := ResultsFileName;
  end;
  FThread.Start;
end;

procedure TTsRunTestSubmission.TerminateTesting;
begin
  if FThread = nil then
    Exit;
  FFinishTrigger := False;
  // terminate thread
  FThread.Terminate;
  // wait for thread
  while (FThread <> nil) and (not FThread.Finished) do
    CheckSynchronize(1);
end;

constructor TTsRunTestSubmission.Create(AManager: TSubmissionManager; AID: integer);
begin
  inherited Create(AManager, AID);
  FThread := nil;
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

  procedure CheckForNotEmpty(const StrName, StrValue: string);
  begin
    if StrValue = '' then
      raise ETsRunThread.CreateFmt(SStringCannotBeEmpty, [StrName]);
  end;

begin
  // validate
  CheckForNotEmpty('ProblemWorkDir', FProblemWorkDir);
  CheckForNotEmpty('ProblemPropsFile', FProblemPropsFile);
  CheckForNotEmpty('TestSrc', FTestSrc);
  CheckForNotEmpty('ResFile', FResFile);
  CheckForNotEmpty('TestDirName', FTestDirName);
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
  // raise exception if non-zero exitcode
  if FExitCode <> 0 then
    raise ETsRunThread.CreateFmt(STsRunNonZeroExitcode, [FExitCode]);
end;

constructor TTsRunThread.Create;
begin
  inherited Create(True, DefaultStackSize);
  FProcess := nil;
  FTsRunExe := Config.Location_TsRunExe;
  FTestDirName := 'tsweb-' + RandomFileName(16);
  FTimeout := 30;
  FreeOnTerminate := True;
end;

end.

