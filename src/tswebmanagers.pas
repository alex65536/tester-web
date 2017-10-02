unit tswebmanagers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, submissions, submissions_tsrun, problems;

type

  { TTsWebProblemManager }

  TTsWebProblemManager = class(TTestableProblemManager)
  end;

  { TTsWebSubmissionManager }

  TTsWebSubmissionManager = class(TTsRunSubmissionManager)
  protected
    function GetProblemManager: TTestableProblemManager; override;
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

{ TTsWebSubmissionManager }

function TTsWebSubmissionManager.GetProblemManager: TTestableProblemManager;
begin
  Result := ProblemManager;
end;

finalization
  FreeAndNil(FSubmissionManager);
  FreeAndNil(FProblemManager);

end.

