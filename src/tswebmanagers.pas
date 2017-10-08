unit tswebmanagers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, submissions, submissions_tsrun, problems, commitscheduler,
  contests;

type

  { TTsWebProblemManager }

  TTsWebProblemManager = class(TTestableProblemManager)
  public
    function SubmissionManager: TSubmissionManager; override;
    constructor Create;
  end;

  { TTsWebSubmissionManager }

  TTsWebSubmissionManager = class(TTsRunSubmissionManager)
  public
    function ProblemManager: TTestableProblemManager; override;
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

constructor TTsWebSubmissionManager.Create;
begin
  inherited Create;
  Scheduler.AttachStorage(Storage);
end;

initialization
  FProblemManager := TTsWebProblemManager.Create;
  FSubmissionManager := TTsWebSubmissionManager.Create;
  FContestManager := TTsWebContestManager.Create;

finalization
  FreeAndNil(FContestManager);
  FreeAndNil(FSubmissionManager);
  FreeAndNil(FProblemManager);

end.

