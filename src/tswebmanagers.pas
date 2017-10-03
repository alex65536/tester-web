unit tswebmanagers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, submissions, submissions_tsrun, problems, commitscheduler;

type

  { TTsWebProblemManager }

  TTsWebProblemManager = class(TTestableProblemManager)
  public
    constructor Create;
  end;

  { TTsWebSubmissionManager }

  TTsWebSubmissionManager = class(TTsRunSubmissionManager)
  protected
    function GetProblemManager: TTestableProblemManager; override;
  public
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

constructor TTsWebProblemManager.Create;
begin
  inherited Create;
  Scheduler.AttachStorage(Storage);
end;

{ TTsWebSubmissionManager }

function TTsWebSubmissionManager.GetProblemManager: TTestableProblemManager;
begin
  Result := tswebmanagers.ProblemManager;
end;

constructor TTsWebSubmissionManager.Create;
begin
  inherited Create;
  Scheduler.AttachStorage(Storage);
end;

finalization
  FreeAndNil(FSubmissionManager);
  FreeAndNil(FProblemManager);

end.

