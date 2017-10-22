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
unit tswebsolvemodules;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, webmodules, tswebsolvepages, fphttp, htmlpages, tswebeditablemodules,
  tsmiscwebmodules, tswebsolveelements, contests, users, editableobjects,
  HTTPDefs, tswebpagesbase, tswebmanagers, tswebproblemmodules, problems,
  webstrconsts, contestproblems, submissions;

type
  ESolveWebModule = class(Exception);

  { TSolveContestListModule }

  TSolveContestListModule = class(THtmlPageWebModule)
  public
    function DoCreatePage: THtmlPage; override;
  end;

  { TRedirectIfNoContestAccessWebHandler }

  TRedirectIfNoContestAccessWebHandler = class(TWebModuleHandler)
  public
    procedure HandleRequest({%H-}ARequest: TRequest; AResponse: TResponse;
      var Handled: boolean); override;
  end;

  { TSolveBaseContestWebModule }

  TSolveBaseContestWebModule = class(TUserWebModule, IContestSolveModule)
  private
    FContest: TContest;
  protected
    procedure DoSessionCreated; override;
    procedure DoAfterRequest; override;
  public
    function Contest: TContest;
    procedure AfterConstruction; override;
  end;

  { TSolvePostContestWebModule }

  TSolvePostContestWebModule = class(TPostUserWebModule, IContestSolveModule)
  private
    FContest: TContest;
    FTransaction: TTestContestTransaction;
  protected
    procedure DoSessionCreated; override;
    procedure DoAfterRequest; override;
    function CanRedirect: boolean; override;
    function RedirectLocation: string; override;
  public
    function Contest: TContest;
    property Transaction: TTestContestTransaction read FTransaction;
    procedure AfterConstruction; override;
  end;

  { TSolveProblemListWebModule }

  TSolveProblemListWebModule = class(TSolveBaseContestWebModule)
  protected
    function DoCreatePage: THtmlPage; override;
  end;

  { TSolveContestProblemModule }

  TSolveContestProblemModule = class(TSolvePostContestWebModule)
  private
    FHandler: TProblemTestPostHandler;
  protected
    function ProblemIndex: integer;
    function DoCreatePage: THtmlPage; override;
    procedure DoPageAfterConstruction(APage: THtmlPage); override;
    procedure DoSessionCreated; override;
    procedure DoAfterRequest; override;
    procedure DoHandlePost(ARequest: TRequest); override;
    function CanRedirect: boolean; override;
    function RedirectLocation: string; override;
  end;

  { TSolveDownloadHandler }

  TSolveDownloadHandler = class(TProblemDownloadHandler)
  protected
    procedure InternalHandleDownload; override;
  end;

  { TSolveDownloadWebModule }

  TSolveDownloadWebModule = class(TUserWebModule)
  protected
    function DoCreatePage: THtmlPage; override;
  public
    procedure AfterConstruction; override;
  end;

  { TSolveSubmissionsWebModule }

  TSolveSubmissionsWebModule = class(TSolvePostContestWebModule)
  protected
    procedure DoHandlePost(ARequest: TRequest); override;
    function DoCreatePage: THtmlPage; override;
  end;

  { TSolveStandingsWebModule }

  TSolveStandingsWebModule = class(TSolveBaseContestWebModule)
  protected
    function DoCreatePage: THtmlPage; override;
  end;

function SolveContestFromRequest(ARequest: TRequest): TContest;
function SolveContestNameFromRequest(ARequest: TRequest): string;

implementation

function SolveContestFromRequest(ARequest: TRequest): TContest;
begin
  Result := EditableObjectFromRequest(ARequest, ContestManager, 'contest') as TContest;
end;

function SolveContestNameFromRequest(ARequest: TRequest): string;
begin
  Result := EditableObjectNameFromRequest(ARequest, 'contest');
end;

{ TSolveStandingsWebModule }

function TSolveStandingsWebModule.DoCreatePage: THtmlPage;
begin
  Result := TSolveStandingsPage.Create;
end;

{ TSolveSubmissionsWebModule }

procedure TSolveSubmissionsWebModule.DoHandlePost(ARequest: TRequest);
var
  SubmissionSession: TContestProblemSubmissionSession;
begin
  if ARequest.ContentFields.Values['query'] <> 'rejudge' then
    Exit;
  SubmissionSession := ProblemManager.CreateSubmissionSession(User) as TContestProblemSubmissionSession;
  try
    SubmissionSession.RejudgeSubmissions(Contest);
  finally
    FreeAndNil(SubmissionSession);
  end;
end;

function TSolveSubmissionsWebModule.DoCreatePage: THtmlPage;
begin
  Result := TSolveSubmissionsPage.Create;
end;

{ TSolveDownloadHandler }

procedure TSolveDownloadHandler.InternalHandleDownload;

  procedure ParseNames(out ContestName: string; out ProblemIndex: integer);
  var
    ContestAndProblemName: string;
    I: integer;
    Delim: integer;
  begin
    ContestAndProblemName := ChangeFileExt(FileName, '');
    // find the delimiter
    Delim := -1;
    for I := 1 to Length(ContestAndProblemName) do
      if ContestAndProblemName[I] = '-' then
        Delim := I;
    if Delim < 0 then
      raise ESolveWebModule.Create(SUnableParseFilename);
    // now, split the string on two
    ContestName := ContestAndProblemName.Substring(0, Delim - 1);
    ProblemIndex := StrToInt(ContestAndProblemName.Substring(Delim)) - 1;
  end;

var
  ContestName: string;
  ProblemIndex: integer;
  Contest: TContest;
  ContestTransaction: TTestContestTransaction;
  Problem: TContestProblem;
  Transaction: TTestProblemTransaction;
  User: TUser;
begin
  ParseNames(ContestName, ProblemIndex);
  User := (Parent as IUserWebModule).User;
  Contest := ContestManager.GetObject(ContestName) as TContest;
  try
    ContestTransaction := Contest.CreateTestTransaction(User);
    try
      Problem := ContestTransaction.Problems[ProblemIndex];
      try
        Transaction := Problem.CreateTestTransaction(User);
        try
          DoHandleTransaction(Transaction);
        finally
          FreeAndNil(Transaction);
        end;
      finally
        FreeAndNil(Problem);
      end;
    finally
      FreeAndNil(ContestTransaction);
    end;
  finally
    FreeAndNil(Contest);
  end;
end;

{ TSolveDownloadWebModule }

function TSolveDownloadWebModule.DoCreatePage: THtmlPage;
begin
  Result := nil; // no need in page in a download module!
end;

procedure TSolveDownloadWebModule.AfterConstruction;
begin
  inherited AfterConstruction;
  Handlers.Add(TSolveDownloadHandler.Create);
end;

{ TSolveContestProblemModule }

function TSolveContestProblemModule.ProblemIndex: integer;
begin
  Result := StrToInt(Request.QueryFields.Values['problem']) - 1;
end;

function TSolveContestProblemModule.DoCreatePage: THtmlPage;
begin
  Result := TSolveContestProblemPage.Create;
end;

procedure TSolveContestProblemModule.DoPageAfterConstruction(APage: THtmlPage);
begin
  inherited DoPageAfterConstruction(APage);
  (APage as TSolveContestProblemPage).ProblemIndex := Self.ProblemIndex;
end;

procedure TSolveContestProblemModule.DoSessionCreated;
begin
  inherited DoSessionCreated;
  if Transaction <> nil then
    FHandler := TProblemTestPostHandler.Create(Transaction.Problems[ProblemIndex], Self);
end;

procedure TSolveContestProblemModule.DoAfterRequest;
begin
  FreeAndNil(FHandler);
  inherited DoAfterRequest;
end;

procedure TSolveContestProblemModule.DoHandlePost(ARequest: TRequest);
begin
  FHandler.HandlePost(ARequest);
end;

function TSolveContestProblemModule.CanRedirect: boolean;
begin
  Result := FHandler.CanRedirect;
end;

function TSolveContestProblemModule.RedirectLocation: string;
begin
  Result := FHandler.RedirectLocation;
end;

{ TSolveProblemListWebModule }

function TSolveProblemListWebModule.DoCreatePage: THtmlPage;
begin
  Result := TSolveProblemListPage.Create;
end;

{ TSolvePostContestWebModule }

procedure TSolvePostContestWebModule.DoSessionCreated;
begin
  inherited DoSessionCreated;
  FContest := SolveContestFromRequest(Request);
  try
    FTransaction := FContest.CreateTestTransaction(User);
  except
    FTransaction := nil;
    // mute the exception, as RedirectIfNoContestAccessHandler will send redirect
  end;
end;

procedure TSolvePostContestWebModule.DoAfterRequest;
begin
  FreeAndNil(FTransaction);
  FreeAndNil(FContest);
  inherited DoAfterRequest;
end;

function TSolvePostContestWebModule.CanRedirect: boolean;
begin
  Result := True;
end;

function TSolvePostContestWebModule.RedirectLocation: string;
begin
  Result := Request.URI;
end;

function TSolvePostContestWebModule.Contest: TContest;
begin
  Result := FContest;
end;

procedure TSolvePostContestWebModule.AfterConstruction;
begin
  Handlers.Add(TRedirectIfNoContestAccessWebHandler.Create);
  inherited AfterConstruction;
end;

{ TSolveBaseContestWebModule }

procedure TSolveBaseContestWebModule.DoSessionCreated;
begin
  inherited DoSessionCreated;
  FContest := SolveContestFromRequest(Request);
end;

procedure TSolveBaseContestWebModule.DoAfterRequest;
begin
  FreeAndNil(FContest);
  inherited DoAfterRequest;
end;

function TSolveBaseContestWebModule.Contest: TContest;
begin
  Result := FContest;
end;

procedure TSolveBaseContestWebModule.AfterConstruction;
begin
  inherited AfterConstruction;
  Handlers.Add(TRedirectIfNoContestAccessWebHandler.Create);
end;

{ TRedirectIfNoContestAccessWebHandler }

procedure TRedirectIfNoContestAccessWebHandler.HandleRequest(
  ARequest: TRequest; AResponse: TResponse; var Handled: boolean);
var
  User: TUser;
  Contest: TContest;
  Transaction: TTestContestTransaction;
  Redirect: boolean;
begin
  User := (Parent as IUserWebModule).User;
  Contest := (Parent as IContestSolveModule).Contest;
  Redirect := False;
  try
    Transaction := Contest.CreateTestTransaction(User);
    try
      Redirect := not Transaction.CanReadData;
    finally
      FreeAndNil(Transaction);
    end;
  except
    on E: EEditableAction do
      Redirect := True
    else
      raise;
  end;
  if Redirect then
  begin
    AResponse.Location := DocumentRoot + '/solve';
    AResponse.Code := 303;
    Handled := True;
  end;
end;

{ TSolveContestListModule }

function TSolveContestListModule.DoCreatePage: THtmlPage;
begin
  Result := TSolveListPage.Create;
end;

initialization
  RegisterHTTPModule('solve', TSolveContestListModule, True);
  RegisterHTTPModule('solve-contest', TSolveProblemListWebModule, True);
  RegisterHTTPModule('solve-problem', TSolveContestProblemModule, True);
  RegisterHTTPModule('solve-download', TSolveDownloadWebModule, True);
  RegisterHTTPModule('solve-submissions', TSolveSubmissionsWebModule, True);
  RegisterHTTPModule('solve-standings', TSolveStandingsWebModule, True);

end.

