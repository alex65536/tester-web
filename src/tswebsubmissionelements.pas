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
unit tswebsubmissionelements;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, submissionlanguages, tswebpagesbase, htmlpages, htmlpreprocess,
  submissions, submissioninfo, strverdicts, webstrconsts, strconsts, testresults;

type

  {$interfaces CORBA}

  { IViewSubmissionPage }

  IViewSubmissionPage = interface
    ['{DD46AD42-06E6-45B5-8957-C991ABFB8ED7}']
    function Submission: TViewSubmission;
    function SubmissionSession: TProblemSubmissionSession;
  end;
  {$interfaces COM}

  { TSubmissionLanguageItem }

  TSubmissionLanguageItem = class(TTesterHtmlPageElement)
  private
    FLanguage: TSubmissionLanguage;
  protected
    procedure DoFillVariables; override;
    procedure DoGetSkeleton(Strings: TIndentTaggedStrings); override;
  public
    property Language: TSubmissionLanguage read FLanguage;
    constructor Create(AParent: THtmlPage; ALanguage: TSubmissionLanguage);
  end;

  { TSubmissionLanguageItemList }

  TSubmissionLanguageItemList = class(TTesterHtmlListedPageElement)
  protected
    procedure DoFillVariables; override;
    procedure DoGetSkeleton(Strings: TIndentTaggedStrings); override;
  public
    constructor Create(AParent: THtmlPage);
  end;

  { TSubmissionItem }

  TSubmissionItem = class(TTesterHtmlPageElement)
  private
    FSubmission: TViewSubmission;
  protected
    procedure DoFillVariables; override;
    procedure DoGetSkeleton(Strings: TIndentTaggedStrings); override;
  public
    property Submission: TViewSubmission read FSubmission;
    constructor Create(AParent: THtmlPage; ASubmission: TViewSubmission);
    destructor Destroy; override;
  end;

  { TSubmissionItemList }

  TSubmissionItemList = class(TTesterHtmlListedPageElement)
  private
    FSubmissionList: TIdList;
    FSession: TProblemSubmissionSession;
  protected
    procedure DoFillList;
    procedure DoFillVariables; override;
    procedure DoGetSkeleton(Strings: TIndentTaggedStrings); override;
  public
    property Session: TProblemSubmissionSession read FSession;
    property SubmissionList: TIdList read FSubmissionList;
    constructor Create(AParent: THtmlPage; ASubmissionList: TIdList;
      ASession: TProblemSubmissionSession);
    destructor Destroy; override;
  end;

  { TSubmissionTestItem }

  TSubmissionTestItem = class(TTesterHtmlPageElement)
  private
    FTestResult: TTestResult;
  protected
    procedure DoFillVariables; override;
    procedure DoGetSkeleton(Strings: TIndentTaggedStrings); override;
  public
    property TestResult: TTestResult read FTestResult;
    constructor Create(AParent: THtmlPage; ATestResult: TTestResult);
  end;

  { TSubmissionTestItemList }

  TSubmissionTestItemList = class(TTesterHtmlListedPageElement)
  private
    FResults: TTestedProblem;
  protected
    procedure FillList;
    procedure DoFillVariables; override;
    procedure DoGetSkeleton(Strings: TIndentTaggedStrings); override;
  public
    property Results: TTestedProblem read FResults;
    constructor Create(AParent: THtmlPage; AResults: TTestedProblem);
  end;

implementation

{ TSubmissionTestItemList }

procedure TSubmissionTestItemList.FillList;
var
  I: integer;
begin
  for I := 0 to FResults.TestResultsCount - 1 do
    List.Add(TSubmissionTestItem.Create(Parent, FResults.TestResults[I]));
end;

procedure TSubmissionTestItemList.DoFillVariables;
begin
  with Storage do
  begin
    ItemsAsText['testIdHeader'] := STestIdHeader;
    ItemsAsText['testVerdictHeader'] := STestVerdictHeader;
    ItemsAsText['testTimeHeader'] := STestTimeHeader;
    ItemsAsText['testMemoryHeader'] := STestMemoryHeader;
    ItemsAsText['testScoreHeader'] := STestScoreHeader;
  end;
  AddListToVariable('submissionTestTableItems');
end;

procedure TSubmissionTestItemList.DoGetSkeleton(Strings: TIndentTaggedStrings);
begin
  LoadTemplateStrings(Strings, 'submissions', 'submissionTestTable');
end;

constructor TSubmissionTestItemList.Create(AParent: THtmlPage;
  AResults: TTestedProblem);
begin
  inherited Create(AParent);
  FResults := AResults;
  FillList;
end;

{ TSubmissionTestItem }

procedure TSubmissionTestItem.DoFillVariables;
begin
  with Storage do
  begin
    ItemsAsText['testId'] := IntToStr(TestResult.Index + 1);
    ItemsAsText['testVerdict'] := STestVerdicts[TestResult.Verdict];
    ItemsAsText['testVerdictKind'] := TestVerdictToStr(TestResult.Verdict);
    ItemsAsText['testTime'] := ProblemTimeToStrEx(TestResult.Time);
    ItemsAsText['testMemory'] := ProblemMemoryToStrEx(TestResult.Memory);
    ItemsAsText['testScore'] := Format(SScoreFmt, [TestResult.Score]);
  end;
end;

procedure TSubmissionTestItem.DoGetSkeleton(Strings: TIndentTaggedStrings);
begin
  LoadTemplateStrings(Strings, 'submissions', 'submissionTestTableItem');
end;

constructor TSubmissionTestItem.Create(AParent: THtmlPage;
  ATestResult: TTestResult);
begin
  inherited Create(AParent);
  FTestResult := ATestResult;
end;

{ TSubmissionItemList }

procedure TSubmissionItemList.DoFillList;
var
  AID: integer;
begin
  SubmissionList.Sort(True);
  for AID in SubmissionList do
    List.Add(TSubmissionItem.Create(Parent, Session.GetSubmission(AID)));
end;

procedure TSubmissionItemList.DoFillVariables;
begin
  with Storage do
  begin
    ItemsAsText['submissionHeaderId'] := SSubmissionHeaderId;
    ItemsAsText['submissionHeaderSubmitTime'] := SSubmissionHeaderSubmitTime;
    ItemsAsText['submissionHeaderAuthor'] := SSubmissionHeaderAuthor;
    ItemsAsText['submissionHeaderLanguage'] := SSubmissionHeaderLanguage;
    ItemsAsText['submissionHeaderVerdict'] := SSubmissionHeaderVerdict;
    ItemsAsText['submissionHeaderTest'] := SSubmissionHeaderTest;
    ItemsAsText['submissionHeaderTime'] := SSubmissionHeaderTime;
    ItemsAsText['submissionHeaderMemory'] := SSubmissionHeaderMemory;
    ItemsAsText['submissionHeaderScore'] := SSubmissionHeaderScore;
  end;
  AddListToVariable('submissionListItems');
end;

procedure TSubmissionItemList.DoGetSkeleton(Strings: TIndentTaggedStrings);
begin
  LoadTemplateStrings(Strings, 'problem', 'problemSubmissionList');
end;

constructor TSubmissionItemList.Create(AParent: THtmlPage;
  ASubmissionList: TIdList; ASession: TProblemSubmissionSession);
begin
  inherited Create(AParent);
  FSubmissionList := ASubmissionList;
  FSession := ASession;
  DoFillList;
end;

destructor TSubmissionItemList.Destroy;
begin
  FreeAndNil(FSubmissionList);
  inherited Destroy;
end;

{ TSubmissionItem }

procedure TSubmissionItem.DoFillVariables;

  function TestIdToStr(ATestNumber: integer): string;
  begin
    if ATestNumber = -1 then
      Result := '-'
    else
      Result := IntToStr(ATestNumber + 1);
  end;

begin
  with Storage do
  begin
    ItemsAsText['submissionId'] := IntToStr(Submission.ID);
    ItemsAsText['submissionSubmitTime'] := FormatDateTime(SPreferredDateTimeFormat, Submission.SubmitTime);
    ItemsAsText['submissionAuthorLink'] := Parent.GenerateUserLink(Submission.OwnerName);
    ItemsAsText['submissionLanguage'] := LanguageFullCompilerNames(Submission.Language);
    ItemsAsText['submissionVerdict'] := VerdictKindToStr(Submission.VerdictKind);
    ItemsAsText['submissionVerdictKind'] := Submission.VerdictKind;
    ItemsAsText['submissionTest'] := TestIdToStr(Submission.TestCase);
    ItemsAsText['submissionTime'] := ProblemTimeToStr(Submission.Time);
    ItemsAsText['submissionMemory'] := ProblemMemoryToStr(Submission.Memory);
    ItemsAsText['submissionScore'] := Format(SScoreFmt, [Submission.Score]);
  end;
end;

procedure TSubmissionItem.DoGetSkeleton(Strings: TIndentTaggedStrings);
begin
  LoadTemplateStrings(Strings, 'problem', 'problemSubmissionListItem');
end;

constructor TSubmissionItem.Create(AParent: THtmlPage; ASubmission: TViewSubmission);
begin
  inherited Create(AParent);
  FSubmission := ASubmission;
end;

destructor TSubmissionItem.Destroy;
begin
  FreeAndNil(FSubmission);
  inherited Destroy;
end;

{ TSubmissionLanguageItemList }

procedure TSubmissionLanguageItemList.DoFillVariables;
begin
  AddListToVariable('problemLanguageItems');
end;

procedure TSubmissionLanguageItemList.DoGetSkeleton(Strings: TIndentTaggedStrings);
begin
  LoadTemplateStrings(Strings, 'problem', 'problemLanguageList');
end;

constructor TSubmissionLanguageItemList.Create(AParent: THtmlPage);
var
  L: TSubmissionLanguage;
begin
  inherited Create(AParent);
  for L in TSubmissionLanguage do
    List.Add(TSubmissionLanguageItem.Create(AParent, L));
end;

{ TSubmissionLanguageItem }

procedure TSubmissionLanguageItem.DoFillVariables;
begin
  Storage.ItemsAsText['problemLanguageOptionName'] := LanguageToStr(Language);
  Storage.ItemsAsText['problemLanguageOptionText'] := LanguageFullCompilerNames(Language);
end;

procedure TSubmissionLanguageItem.DoGetSkeleton(Strings: TIndentTaggedStrings);
begin
  LoadTemplateStrings(Strings, 'problem', 'problemLanguageOption');
end;

constructor TSubmissionLanguageItem.Create(AParent: THtmlPage; ALanguage: TSubmissionLanguage);
begin
  inherited Create(AParent);
  FLanguage := ALanguage;
end;

end.

