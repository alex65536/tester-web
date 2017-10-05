unit tswebmanagers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, submissions, submissions_tsrun, problems, commitscheduler;

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

function ProblemManager: TTsWebProblemManager;
function SubmissionManager: TTsWebSubmissionManager;

implementation

var
  FProblemManager: TTsWebProblemManager = nil;
  FSubmissionManager: TTsWebSubmissionManager = nil;

function ProblemManager: TTsWebProblemManager;
begin
  if FProblemManager = nil then
    FProblemManager := TTsWebProblemManager.Create;
  Result := FProblemManager;
end;

function SubmissionManager: TTsWebSubmissionManager;
begin
  if FSubmissionManager = nil then
    FSubmissionManager := TTsWebSubmissionManager.Create;
  Result := FSubmissionManager;
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
  ProblemManager;
  SubmissionManager;

finalization
  FreeAndNil(FSubmissionManager);
  FreeAndNil(FProblemManager);

end.

