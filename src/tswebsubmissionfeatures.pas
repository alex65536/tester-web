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
unit tswebsubmissionfeatures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, submissions, tswebeditablefeatures, editableobjects,
  tswebfeatures, htmlpages, webstrconsts, tswebsubmissionelements, tswebutils,
  tswebmanagers, submissioninfo, strverdicts, LazFileUtils, submissionlanguages;

const
  LanguageBrushNames: array [TSubmissionLanguage] of string =
    ('delphi', 'delphi', 'cpp', 'cpp', 'cpp');

type

  { TSubmissionTransactionPageFeature }

  TSubmissionTransactionPageFeature = class(TEditableTransactionPageFeature)
  private
    FSession: TProblemSubmissionSession;
    function GetTransaction: TTestProblemTransaction;
  protected
    property Transaction: TTestProblemTransaction read GetTransaction;
    property Session: TProblemSubmissionSession read FSession;
    function CreateTransaction: TEditableTransaction; override;
    function CreateSubmissionSession: TProblemSubmissionSession; virtual;
    procedure BeforeSatisfy; override;
    procedure AfterSatisfy; override;
  end;

  { TSubmitPageFeature }

  TSubmitPageFeature = class(TSubmissionTransactionPageFeature)
  protected
    procedure InternalSatisfy; override;
  public
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

  { TSubmitFooterPageFeature }

  TSubmitFooterPageFeature = class(TSubmissionTransactionPageFeature)
  protected
    procedure InternalSatisfy; override;
  public
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

  { TSubmissionViewBasePageFeature }

  TSubmissionViewBasePageFeature = class(TTesterPageFeature)
  public
    function Submission: TViewSubmission;
    function Session: TProblemSubmissionSession;
  end;

  { TSubmissionEmptyInnerPageFeature }

  TSubmissionEmptyInnerPageFeature = class(TTesterPageFeature)
  public
    procedure Satisfy; override;
  end;

  { TSubmissionCompileInnerPageFeature }

  TSubmissionCompileInnerPageFeature = class(TSubmissionViewBasePageFeature)
  public
    procedure Satisfy; override;
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

  { TSubmissionTestInnerPageFeature }

  TSubmissionTestInnerPageFeature = class(TSubmissionViewBasePageFeature)
  public
    procedure Satisfy; override;
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

  { TSubmissionSourceInnerPageFeature }

  TSubmissionSourceInnerPageFeature = class(TSubmissionViewBasePageFeature)
  public
    procedure Satisfy; override;
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

  { TSubmissionViewPageFeature }

  TSubmissionViewPageFeature = class(TSubmissionViewBasePageFeature)
  public
    procedure Satisfy; override;
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

  { TSyntaxHighlighterJsPageFeature }

  TSyntaxHighlighterJsPageFeature = class(TTesterPageFeature)
  public
    procedure Satisfy; override;
  end;

implementation

uses
  tswebproblemfeatures;

{ TSyntaxHighlighterJsPageFeature }

procedure TSyntaxHighlighterJsPageFeature.Satisfy;
begin
  LoadPagePart('', 'syntaxHighlighterJsFiles');
end;

{ TSubmissionViewPageFeature }

procedure TSubmissionViewPageFeature.Satisfy;
var
  IdList: TIdList;
  BriefTable: TSubmissionItemList;
begin
  // fill brief table
  IdList := TIdList.Create;
  IdList.Add(Submission.ID);
  BriefTable := TSubmissionItemList.Create(Parent, IdList, Session, nil);
  try
    Parent.AddElementPagePart('submissionBriefTable', BriefTable);
  finally
    FreeAndNil(BriefTable);
  end;
  // fill variables
  with Parent.Variables do
  begin
    ItemsAsText['refreshDuration'] := '2';
    ItemsAsText['submissionCompileHeader'] := SSubmissionCompileHeader;
    ItemsAsText['submissionTestHeader'] := SSubmissionTestHeader;
    ItemsAsText['submissionSourceHeader'] := SSubmissionSourceHeader;
    // add rejudge button (if we can rejudge)
    if Session.CanRejudgeSubmission(Submission.ID) then
      ItemsAsText['problemCanRejudgeForm'] := '~+#problemRejudgeForm;';
  end;
  // add refresh header (if not finished)
  if not Submission.Finished then
    LoadPagePart('', 'refreshHeader');
  // load page part
  LoadPagePart('submissions', 'submissionView');
end;

procedure TSubmissionViewPageFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TSubmissionCompileInnerPageFeature);
  ADependencies.Add(TSubmissionTestInnerPageFeature);
  ADependencies.Add(TSubmissionSourceInnerPageFeature);
  ADependencies.Add(TSyntaxHighlighterJsPageFeature);
  ADependencies.Add(TProblemRejudgeFormFeature);
end;

{ TSubmissionSourceInnerPageFeature }

procedure TSubmissionSourceInnerPageFeature.Satisfy;
begin
  if not FileExistsUTF8(Submission.FileName) then
  begin
    Parent.Variables.ItemsAsText['submissionTestInner'] := '~#+submissionEmptyInner;';
    Exit;
  end;
  with Parent.Variables do
  begin
    SetFromFile('sourceCode', Submission.FileName);
    ItemsAsText['sourceBrushName'] := LanguageBrushNames[Submission.Language];
  end;
  LoadPagePart('submissions', 'submissionSourceInner');
end;

procedure TSubmissionSourceInnerPageFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TSubmissionEmptyInnerPageFeature);
end;

{ TSubmissionEmptyInnerPageFeature }

procedure TSubmissionEmptyInnerPageFeature.Satisfy;
begin
  with Parent.Variables do
  begin
    ItemsAsText['submissionEmptySection'] := SSubmissionEmptySection;
  end;
  LoadPagePart('submissions', 'submissionEmptyInner');
end;

{ TSubmissionTestInnerPageFeature }

procedure TSubmissionTestInnerPageFeature.Satisfy;
var
  List: TSubmissionTestItemList;
begin
  if Submission.Results = nil then
  begin
    Parent.Variables.ItemsAsText['submissionTestInner'] := '~#+submissionEmptyInner;';
    Exit;
  end;
  List := TSubmissionTestItemList.Create(Parent, Submission.Results);
  try
    Parent.AddElementPagePart('submissionTestTable', List);
  finally
    FreeAndNil(List);
  end;
  LoadPagePart('submissions', 'submissionTestInner');
end;

procedure TSubmissionTestInnerPageFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TSubmissionEmptyInnerPageFeature);
end;

{ TSubmissionCompileInnerPageFeature }

procedure TSubmissionCompileInnerPageFeature.Satisfy;
begin
  if Submission.Results = nil then
  begin
    Parent.Variables.ItemsAsText['submissionCompileInner'] := '~#+submissionEmptyInner;';
    Exit;
  end;
  with Parent.Variables, Submission.Results do
  begin
    ItemsAsText['compileVerdictCaption'] := SCompileVerdictCaption;
    ItemsAsText['compileVerdictKind'] := CompilerVerdictToStr(CompileVerdict);
    ItemsAsText['compileVerdict'] := SCompilerVerdicts[CompileVerdict];
    ItemsAsText['compileOutputCaption'] := SCompileOutputCaption;
    ItemsAsText['compileOutput'] := CompilerOutput;
  end;
  LoadPagePart('submissions', 'submissionCompileInner');
end;

procedure TSubmissionCompileInnerPageFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TSubmissionEmptyInnerPageFeature);
end;

{ TSubmissionViewBasePageFeature }

function TSubmissionViewBasePageFeature.Submission: TViewSubmission;
begin
  Result := (Parent as IViewSubmissionPage).Submission;
end;

function TSubmissionViewBasePageFeature.Session: TProblemSubmissionSession;
begin
  Result := (Parent as IViewSubmissionPage).SubmissionSession;
end;

{ TSubmitFooterPageFeature }

procedure TSubmitFooterPageFeature.InternalSatisfy;
var
  SubmissionIds: TIdList;
  List: TSubmissionItemList;
begin
  SubmissionIds := Session.ListByOwner(EditableObject as TTestableProblem);
  List := TSubmissionItemList.Create(Parent, SubmissionIds, Session, nil);
  try
    Parent.AddElementPagePart('problemSubmissionList', List);
  finally
    FreeAndNil(List);
  end;
  LoadPagePart('problem', 'problemSubmitFooter', 'contentFooterInner');
end;

procedure TSubmitFooterPageFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TContentFooterFeature);
  ADependencies.Add(TSubmitPageFeature);
end;

{ TSubmitPageFeature }

procedure TSubmitPageFeature.InternalSatisfy;
var
  List: TSubmissionLanguageItemList;
begin
  // we don't add this feature it we're not allowed to test!
  if not Transaction.CanTestProblem then
    Exit;
  // fill variables
  with Parent.Variables do
  begin
    ItemsAsText['problemSubmitSolution'] := SProblemSubmitSolution;
    ItemsAsText['problemLanguage'] := SProblemLanguage;
    ItemsAsText['problemFile'] := SProblemFile;
    ItemsAsText['problemSubmitText'] := SProblemSubmitText;
    ItemsAsText['problemSubmitMaxSize'] := IntToStr(Transaction.MaxSrcLimit);
  end;
  // load language list
  List := TSubmissionLanguageItemList.Create(Parent);
  try
    Parent.AddElementPagePart('problemLanguageList', List);
  finally
    FreeAndNil(List);
  end;
  LoadPagePart('problem', 'problemSubmit');
end;

procedure TSubmitPageFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TPostDataFeature);
end;

{ TSubmissionTransactionPageFeature }

function TSubmissionTransactionPageFeature.GetTransaction: TTestProblemTransaction;
begin
  Result := (inherited Transaction) as TTestProblemTransaction;
end;

function TSubmissionTransactionPageFeature.CreateTransaction: TEditableTransaction;
begin
  Result := (EditableObject as TTestableProblem).CreateTestTransaction(User);
end;

function TSubmissionTransactionPageFeature.CreateSubmissionSession: TProblemSubmissionSession;
begin
  Result := ProblemManager.CreateSubmissionSession(User);
end;

procedure TSubmissionTransactionPageFeature.BeforeSatisfy;
begin
  inherited BeforeSatisfy;
  FSession := CreateSubmissionSession;
end;

procedure TSubmissionTransactionPageFeature.AfterSatisfy;
begin
  FreeAndNil(FSession);
  inherited AfterSatisfy;
end;

end.

