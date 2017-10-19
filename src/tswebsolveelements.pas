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
  tswebpagesbase, webstrconsts, webstrutils, tswebmanagers;

const
  SContestAccessTypes: array [TContestAccessType] of string = (
    SContestAccessTypeNone,
    SContestAccessTypeSetter,
    SContestAccessTypeParticipant
  );

type
  {$interfaces CORBA}

  { IContestPage }

  IContestPage = interface
    ['{7C18C5C5-C400-4CEB-866C-6EB91084F07E}']
    function Contest: TContest;
  end;
  {$interfaces COM}

  { TSolveContestListItem }

  TSolveContestListItem = class(TTesterHtmlPageElement)
  private
    FContest: TContest;
    FTransaction: TContestViewTransaction;
  protected
    procedure DoFillVariables; override;
    procedure DoGetSkeleton(Strings: TIndentTaggedStrings); override;
  public
    property Contest: TContest read FContest;
    property Transaction: TContestViewTransaction read FTransaction;
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

implementation

{ TSolveContestListItem }

procedure TSolveContestListItem.DoFillVariables;
var
  ContestStatus: string;
begin
  // retrieve contest status
  case Transaction.Status of
    csNotStarted: ContestStatus := SStatusNotStarted;
    csRunning: ContestStatus := Format(SStatusRunning, [DurationSecondsToStr(Transaction.SecondsLeft)]);
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

