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
unit tswebsolvefeatures;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, tswebfeatures, tswebsolveelements, contests, tswebmanagers, users,
  editableobjects, tswebpagesbase, contestproblems, userpages, webstrconsts,
  tswebproblemfeatures, tswebsubmissionelements, htmlpages;

type

  { TSolveContestListFeature }

  TSolveContestListFeature = class(TTesterPageFeature)
  public
    procedure Satisfy; override;
  end;

  { TContestUserFeature }

  TContestUserFeature = class(TTesterPageFeature)
  private
    function GetContest: TContest;
    function GetUser: TUser;
  public
    property User: TUser read GetUser;
    property Contest: TContest read GetContest;
  end;

  { TSolveContestBaseFeature }

  TSolveContestBaseFeature = class(TContestUserFeature)
  public
    procedure Satisfy; override;
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

  { TSolveProblemListFeature }

  TSolveProblemListFeature = class(TContestUserFeature)
  public
    procedure Satisfy; override;
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

  { TSolveContestStatusFeature }

  TSolveContestStatusFeature = class(TContestUserFeature)
  public
    procedure Satisfy; override;
  end;

  { TSolveContestProblemFeature }

  TSolveContestProblemFeature = class(TContestUserFeature)
  public
    procedure Satisfy; override;
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

  { TSolveSubmissionsFeature }

  TSolveSubmissionsFeature = class(TContestUserFeature)
  public
    procedure Satisfy; override;
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

  { TSolveStandingsFeature }

  TSolveStandingsFeature = class(TContestUserFeature)
  public
    procedure Satisfy; override;
  end;

implementation

{ TSolveStandingsFeature }

procedure TSolveStandingsFeature.Satisfy;
var
  Transaction: TTestContestTransaction;
  Standings: TSolveStandings;
begin
  Transaction := Contest.CreateTestTransaction(User);
  try
    Standings := TSolveStandings.Create(Parent, Transaction);
    try
      Parent.AddElementPagePart('solveStandings', Standings);
    finally
      FreeAndNil(Standings);
    end;
  finally
    FreeAndNil(Transaction);
  end;
end;

{ TSolveSubmissionsFeature }

procedure TSolveSubmissionsFeature.Satisfy;
var
  Session: TContestProblemSubmissionSession;
  List: TSubmissionItemList;
begin
  Session := ProblemManager.CreateSubmissionSession(User) as TContestProblemSubmissionSession;
  try
    // add submission list
    List := TSubmissionItemList.Create(Parent, Session.ListAvailable(Contest),
      Session, TSubmissionSolveProblemHandler);
    try
      Parent.AddElementPagePart('solveSubmissionList', List);
    finally
      FreeAndNil(List);
    end;
    // add rejudge button (if can rejudge)
    if Session.CanRejudgeSubmission(Contest) then
      Parent.Variables.ItemsAsText['solveRejudgeForm'] := '~#+solveCanRejudgeForm;';
  finally
    FreeAndNil(Session);
  end;
  // load page part
  LoadPagePart('solve', 'solveSubmissions');
end;

procedure TSolveSubmissionsFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TPostDataFeature);
  ADependencies.Add(TProblemRejudgeFormFeature);
end;

{ TSolveContestProblemFeature }

procedure TSolveContestProblemFeature.Satisfy;
var
  Index: integer;
begin
  with Parent.Variables do
  begin
    Index := (Parent as IContestProblemPage).GetProblemIndex + 1;
    ItemsAsText['problemIndex'] := IntToStr(Index);
    ItemsAsText['problemFullIndex'] := Format(SProblemIndexFmt, [Index]);
  end;
  LoadPagePart('solve', 'solveContestProblem');
end;

procedure TSolveContestProblemFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TProblemTestInnerFeature);
end;

{ TContestUserFeature }

function TContestUserFeature.GetContest: TContest;
begin
  Result := (Parent as IContestSolvePage).Contest;
end;

function TContestUserFeature.GetUser: TUser;
begin
  Result := (Parent as TUserPage).User;
end;

{ TSolveContestStatusFeature }

procedure TSolveContestStatusFeature.Satisfy;
var
  Transaction: TTestContestTransaction;
  ContestStatus: string;
  TimeLeft: integer;
begin
  // retrieve contest status
  Transaction := Contest.CreateTestTransaction(User);
  try
    case Transaction.Status of
      csNotStarted: ContestStatus := SStatusNotStarted;
      csRunning: ContestStatus := SStatusRunning;
      csUpsolve: ContestStatus := SStatusUpsolve;
    end;
    TimeLeft := Transaction.SecondsLeft;
  finally
    FreeAndNil(Transaction);
  end;
  // fill variables
  with Parent.Variables do
  begin
    ItemsAsText['solveStatus'] := ContestStatus;
    if TimeLeft >= 0 then
    begin
      ItemsAsText['solveTimeLeft'] := SSolveTimeLeft;
      with Parent as TTesterHtmlPage do
        ItemsAsText['solveTimeLeftTimer'] := GenerateTimer(TimeLeft);
      LoadPagePart('solve', 'solveContestStatusTimer');
    end;
  end;
  // load page part
  LoadPagePart('solve', 'solveContestStatus', 'contestPostHeader');
end;

{ TSolveContestBaseFeature }

procedure TSolveContestBaseFeature.Satisfy;

  function GetContestTitle: string;
  var
    Transaction: TViewContestTransaction;
  begin
    Transaction := Contest.CreateViewTransaction(User);
    try
      Result := Transaction.Title;
    finally
      FreeAndNil(Transaction);
    end;
  end;

var
  PageTitle, ContestTitle: string;
begin
  PageTitle := (Parent as TContentHtmlPage).Title;
  ContestTitle := GetContestTitle;
  with Parent.Variables do
  begin
    ItemsAsText['solveContest'] := Contest.Name;
    ItemsAsText['solveContestTitle'] := ContestTitle;
    ItemsAsText['pageTitle'] := PageTitle;
    ItemsAsText['contentHeaderText'] := PageTitle;
    ItemsAsText['pageHeader'] := ContestTitle;
    ItemsAsText['title'] := '~pageTitle;: ~solveContestTitle;';
  end;
end;

procedure TSolveContestBaseFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TContentHeaderFeature);
  ADependencies.Add(THeaderFeature);
  ADependencies.Add(TSolveContestStatusFeature);
end;

{ TSolveProblemListFeature }

procedure TSolveProblemListFeature.Satisfy;
var
  Transaction: TTestContestTransaction;
  List: TSolveProblemList;
begin
  Transaction := Contest.CreateTestTransaction(User);
  try
    List := TSolveProblemList.Create(Parent, Transaction);
    try
      Parent.AddElementPagePart('solveProblemList', List);
    finally
      FreeAndNil(List);
    end;
  finally
    FreeAndNil(Transaction);
  end;
end;

procedure TSolveProblemListFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TSolveContestBaseFeature);
end;

{ TSolveContestListFeature }

procedure TSolveContestListFeature.Satisfy;
var
  ManagerSession: TEditableManagerSession;
  List: TSolveContestList;
begin
  ManagerSession := ContestManager.CreateManagerSession((Parent as TUserPage).User);
  try
    List := TSolveContestList.Create(Parent, ManagerSession as TContestManagerSession);
    try
      Parent.AddElementPagePart('solveList', List);
    finally
      FreeAndNil(List);
    end;
  finally
    FreeAndNil(ManagerSession);
  end;
end;

end.

