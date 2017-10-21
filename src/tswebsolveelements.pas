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
unit tswebsolveelements;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contests, contestproblems, htmlpreprocess, htmlpages,
  tswebpagesbase, webstrconsts, webstrutils, tswebmanagers, strconsts,
  standings;

const
  SContestAccessTypes: array [TContestAccessType] of string = (
    SContestAccessTypeNone,
    SContestAccessTypeSetter,
    SContestAccessTypeParticipant
  );

type
  {$interfaces CORBA}

  { IContestSolvePage }

  IContestSolvePage = interface
    ['{7C18C5C5-C400-4CEB-866C-6EB91084F07E}']
    function Contest: TContest;
    function ContestName: string;
  end;

  { IContestSolveModule }

  IContestSolveModule = interface
    ['{99858B2B-A77C-447B-9019-97F94E1FFB8C}']
    function Contest: TContest;
  end;

  { IContestProblemPage }

  IContestProblemPage = interface
    ['{5CAB7787-860A-4744-BD9B-7C0E9CE13C48}']
    function GetProblemIndex: integer;
  end;
  {$interfaces COM}

  { TSolveContestListItem }

  TSolveContestListItem = class(TTesterHtmlPageElement)
  private
    FContest: TContest;
    FTransaction: TViewContestTransaction;
  protected
    procedure DoFillVariables; override;
    procedure DoGetSkeleton(Strings: TIndentTaggedStrings); override;
  public
    property Contest: TContest read FContest;
    property Transaction: TViewContestTransaction read FTransaction;
    constructor Create(AParent: THtmlPage; AContest: TContest);
    destructor Destroy; override;
  end;

  { TSolveContestList }

  TSolveContestList = class(TTesterHtmlListedPageElement)
  private
    FManagerSession: TContestManagerSession;
  protected
    procedure DoFillList;
    procedure DoFillVariables; override;
    procedure DoGetSkeleton(Strings: TIndentTaggedStrings); override;
  public
    property ManagerSession: TContestManagerSession read FManagerSession;
    constructor Create(AParent: THtmlPage; ASession: TContestManagerSession);
  end;

  { TSolveProblemListItem }

  TSolveProblemListItem = class(TTesterHtmlPageElement)
  private
    FProblemIndex: integer;
    FTransaction: TTestContestTransaction;
  protected
    procedure DoFillVariables; override;
    procedure DoGetSkeleton(Strings: TIndentTaggedStrings); override;
  public
    property Transaction: TTestContestTransaction read FTransaction;
    property ProblemIndex: integer read FProblemIndex;
    constructor Create(AParent: THtmlPage; ATransaction: TTestContestTransaction;
      AProblemIndex: integer);
  end;

  { TSolveProblemList }

  TSolveProblemList = class(TTesterHtmlListedPageElement)
  private
    FTransaction: TTestContestTransaction;
  protected
    procedure DoFillList;
    procedure DoFillVariables; override;
    procedure DoGetSkeleton(Strings: TIndentTaggedStrings); override;
  public
    property Transaction: TTestContestTransaction read FTransaction;
    constructor Create(AParent: THtmlPage; ATransaction: TTestContestTransaction);
  end;

implementation

{ TSolveProblemList }

procedure TSolveProblemList.DoFillList;
var
  I: integer;
begin
  for I := 0 to Transaction.ProblemCount - 1 do
    List.Add(TSolveProblemListItem.Create(Parent, Transaction, I));
end;

procedure TSolveProblemList.DoFillVariables;
begin
  with Storage do
  begin
    ItemsAsText['solveProblemIndexHeader'] := SSolveProblemIndexHeader;
    ItemsAsText['solveProblemTitleHeader'] := SSolveProblemTitleHeader;
    ItemsAsText['solveProblemScoreHeader'] := SSolveProblemScoreHeader;
  end;
  AddListToVariable('solveProblemListInner');
end;

procedure TSolveProblemList.DoGetSkeleton(Strings: TIndentTaggedStrings);
begin
  Strings.LoadFromFile(TemplateLocation('solve', 'solveProblemList'));
end;

constructor TSolveProblemList.Create(AParent: THtmlPage;
  ATransaction: TTestContestTransaction);
begin
  inherited Create(AParent);
  FTransaction := ATransaction;
  DoFillList;
end;

{ TSolveProblemListItem }

procedure TSolveProblemListItem.DoFillVariables;
var
  Row: TStandingsRow;
  Score: string;
begin
  // get problem score
  Row := Transaction.GetOwnResults;
  if Row = nil then
    Score := SNoAnswer
  else
    Score := Format(SScoreFmt, [Row.ProblemScores[ProblemIndex]]);
  // fill variables
  with Storage do
  begin
    ItemsAsText['solveProblemIndex'] := IntToStr(ProblemIndex + 1);
    ItemsAsText['solveProblemTitle'] := Transaction.ProblemTitles[ProblemIndex];
    ItemsAsText['solveProblemScore'] := Score;
  end;
end;

procedure TSolveProblemListItem.DoGetSkeleton(Strings: TIndentTaggedStrings);
begin
  Strings.LoadFromFile(TemplateLocation('solve', 'solveProblemListItem'));
end;

constructor TSolveProblemListItem.Create(AParent: THtmlPage;
  ATransaction: TTestContestTransaction; AProblemIndex: integer);
begin
  inherited Create(AParent);
  FTransaction := ATransaction;
  FProblemIndex := AProblemIndex;
end;

{ TSolveContestListItem }

procedure TSolveContestListItem.DoFillVariables;
var
  ContestStatus: string;
begin
  // retrieve contest status
  case Transaction.Status of
    csNotStarted: ContestStatus := SStatusNotStarted;
    csRunning: ContestStatus := Format(SStatusRunningFmt, [Parent.GenerateTimer(Transaction.SecondsLeft)]);
    csUpsolve: ContestStatus := SStatusUpsolve;
  end;
  // fill variables
  with Storage do
  begin
    ItemsAsText['solveName'] := Transaction.Contest.Name;
    ItemsAsText['solveTitle'] := Transaction.Title;
    if Transaction.CanAccessContest then
      ItemsAsText['solveNameLink'] := '~+#solveActiveName;'
    else
      ItemsAsText['solveNameLink'] := '~+#solveInactiveName;';
    ItemsAsText['solveStartTime'] := FormatDateTime(SPreferredDateTimeFormat,
      Transaction.StartTime);
    ItemsAsText['solveDuration'] := DurationMinutesToStr(Transaction.DurationMinutes);
    ItemsAsText['solveStatus'] := ContestStatus;
    ItemsAsText['solveAccessType'] := SContestAccessTypes[Transaction.AccessType];
  end;
end;

procedure TSolveContestListItem.DoGetSkeleton(Strings: TIndentTaggedStrings);
begin
  Strings.LoadFromFile(TemplateLocation('solve', 'solveListItem'));
end;

constructor TSolveContestListItem.Create(AParent: THtmlPage; AContest: TContest);
begin
  inherited Create(AParent);
  FContest := AContest;
  FTransaction := AContest.CreateViewTransaction(Parent.User);
end;

destructor TSolveContestListItem.Destroy;
begin
  FreeAndNil(FTransaction);
  FreeAndNil(FContest);
  inherited Destroy;
end;

{ TSolveContestList }

procedure TSolveContestList.DoFillList;
var
  I: integer;
  ContestList: TStringList;
  Contest: TContest;
begin
  ContestList := ManagerSession.ListAvailableContests;
  try
    for I := 0 to ContestList.Count - 1 do
    begin
      Contest := ContestManager.GetObject(ContestList[I]) as TContest;
      List.Add(TSolveContestListItem.Create(Parent, Contest));
    end;
  finally
    FreeAndNil(ContestList);
  end;
end;

procedure TSolveContestList.DoFillVariables;
begin
  with Storage do
  begin
    ItemsAsText['solveNameHeader'] := SSolveNameHeader;
    ItemsAsText['solveStartTimeHeader'] := SSolveStartTimeHeader;
    ItemsAsText['solveDurationHeader'] := SSolveDurationHeader;
    ItemsAsText['solveStatusHeader'] := SSolveStatusHeader;
    ItemsAsText['solveAccessTypeHeader'] := SSolveAccessTypeHeader;
  end;
  AddListToVariable('solveListInner');
end;

procedure TSolveContestList.DoGetSkeleton(Strings: TIndentTaggedStrings);
begin
  Strings.LoadFromFile(TemplateLocation('solve', 'solveList'));
end;

constructor TSolveContestList.Create(AParent: THtmlPage;
  ASession: TContestManagerSession);
begin
  inherited Create(AParent);
  FManagerSession := ASession;
  DoFillList;
end;

end.

