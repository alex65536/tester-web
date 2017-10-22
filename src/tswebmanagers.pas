unit tswebmanagers;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, submissions, submissions_tsrun, commitscheduler, contests,
  contestproblems, standings;

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
    function ProblemManager: TContestProblemManager; override;
    function SubmissionManager: TContestSubmissionManager; override;
    function StandingsManager: TStandingsManager; override;
    constructor Create;
  end;

  { TTsWebStandingsManager }

  TTsWebStandingsManager = class(TStandingsManager)
  public
    function ContestManager: TBaseContestManager; override;
    function SubmissionManager: TContestSubmissionManager; override;
  end;

function ProblemManager: TTsWebProblemManager;
function SubmissionManager: TTsWebSubmissionManager;
function ContestManager: TTsWebContestManager;
function StandingsManager: TTsWebStandingsManager;

implementation

var
  FProblemManager: TTsWebProblemManager = nil;
  FSubmissionManager: TTsWebSubmissionManager = nil;
  FContestManager: TTsWebContestManager = nil;
  FStandingsManager: TTsWebStandingsManager = nil;

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

function StandingsManager: TTsWebStandingsManager;
begin
  Result := FStandingsManager;
end;

{ TTsWebStandingsManager }

function TTsWebStandingsManager.ContestManager: TBaseContestManager;
begin
  Result := tswebmanagers.ContestManager;
end;

function TTsWebStandingsManager.SubmissionManager: TContestSubmissionManager;
begin
  Result := tswebmanagers.SubmissionManager;
end;

{ TTsWebContestManager }

function TTsWebContestManager.ProblemManager: TContestProblemManager;
begin
  Result := tswebmanagers.ProblemManager;
end;

function TTsWebContestManager.SubmissionManager: TContestSubmissionManager;
begin
  Result := tswebmanagers.SubmissionManager;
end;

function TTsWebContestManager.StandingsManager: TStandingsManager;
begin
  Result := tswebmanagers.StandingsManager;
end;

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
  FStandingsManager := TTsWebStandingsManager.Create;

finalization
  FreeAndNil(FStandingsManager);
  FreeAndNil(FSubmissionManager);
  FreeAndNil(FContestManager);
  FreeAndNil(FProblemManager);

end.

