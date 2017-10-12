unit tswebmanagers;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, submissions, submissions_tsrun, commitscheduler, contests, contestproblems;

type

  { TTsWebProblemManager }

  TTsWebProblemManager = class(TContestProblemManager)
  public
    function SubmissionManager: TSubmissionManager; override;
    function ContestManager: TBaseContestManager; override;
    constructor Create;
  end;

  { TTsWebSubmissionManager }

  TTsWebSubmissionManager = class(TTsRunContestSubmissionManager)
  public
    function ProblemManager: TTestableProblemManager; override;
    function ContestManager: TBaseContestManager; override;
    constructor Create;
  end;

  { TTsWebContestManager }

  TTsWebContestManager = class(TContestManager)
  public
    constructor Create;
  end;

function ProblemManager: TTsWebProblemManager;
function SubmissionManager: TTsWebSubmissionManager;
function ContestManager: TTsWebContestManager;

implementation

var
  FProblemManager: TTsWebProblemManager = nil;
  FSubmissionManager: TTsWebSubmissionManager = nil;
  FContestManager: TTsWebContestManager = nil;

function ProblemManager: TTsWebProblemManager;
begin
  Result := FProblemManager;
end;

function SubmissionManager: TTsWebSubmissionManager;
begin
  Result := FSubmissionManager;
end;

function ContestManager: TTsWebContestManager;
begin
  Result := FContestManager;
end;

{ TTsWebContestManager }

constructor TTsWebContestManager.Create;
begin
  inherited Create;
  Scheduler.AttachStorage(Storage);
end;

{ TTsWebProblemManager }

function TTsWebProblemManager.SubmissionManager: TSubmissionManager;
begin
  Result := tswebmanagers.SubmissionManager;
end;

function TTsWebProblemManager.ContestManager: TBaseContestManager;
begin
  Result := tswebmanagers.ContestManager;
end;

constructor TTsWebProblemManager.Create;
begin
  inherited Create;
  Scheduler.AttachStorage(Storage);
end;

{ TTsWebSubmissionManager }

function TTsWebSubmissionManager.ProblemManager: TTestableProblemManager;
begin
  Result := tswebmanagers.ProblemManager;
end;

function TTsWebSubmissionManager.ContestManager: TBaseContestManager;
begin
  Result := tswebmanagers.ContestManager;
end;

constructor TTsWebSubmissionManager.Create;
begin
  inherited Create;
  Scheduler.AttachStorage(Storage);
end;

initialization
  FProblemManager := TTsWebProblemManager.Create;
  FContestManager := TTsWebContestManager.Create;
  FSubmissionManager := TTsWebSubmissionManager.Create;

finalization
  FreeAndNil(FSubmissionManager);
  FreeAndNil(FContestManager);
  FreeAndNil(FProblemManager);

end.

