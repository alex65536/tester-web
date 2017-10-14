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
unit tswebcontestfeatures;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, tswebfeatures, tswebeditablefeatures, htmlpages,
  webstrconsts, contests, tswebcontestelements, tswebdatetimefeatures,
  serverconfig;

const
  SContestScoringPolicies: array [TContestScoringPolicy] of string = (
    SScoringPolicyMaxScore,
    SScoringPolicyLastScore
  );

  SYesNo: array [boolean] of string = (SNo, SYes);

type

  { TContestBaseFeature }

  TContestBaseFeature = class(TTesterPageFeature)
  public
    procedure Satisfy; override;
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

  { TContestCreateFormFeature }

  TContestCreateFormFeature = class(TTesterPageFeature)
  public
    procedure Satisfy; override;
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

  { TContestEditViewBaseFeature }

  TContestEditViewBaseFeature = class(TTesterPageFeature)
  public
    procedure Satisfy; override;
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

  { TContestManageAccessFeature }

  TContestManageAccessFeature = class(TTesterPageFeature)
  public
    procedure Satisfy; override;
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

  { TContestEditFeature }

  TContestEditFeature = class(TEditableTransactionPageFeature)
  protected
    procedure InternalSatisfy; override;
  public
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

  { TContestViewFeature }

  TContestViewFeature = class(TEditableTransactionPageFeature)
  protected
    procedure InternalSatisfy; override;
  public
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

  { TContestParticipantFeature }

  TContestParticipantFeature = class(TEditableObjectFeature)
  private
    FSession: TContestParticipantSession;
  protected
    property Session: TContestParticipantSession read FSession;
    procedure BeforeSatisfy; override;
    procedure InternalSatisfy; override;
    procedure AfterSatisfy; override;
  public
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

  { TContestProblemsFeature }

  TContestProblemsFeature = class(TEditableTransactionPageFeature)
  protected
    procedure InternalSatisfy; override;
  public
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

  { TContestParticipantButtonFeature }

  TContestParticipantButtonFeature = class(TEditableNavButtonFeature)
  protected
    function Enabled: boolean; override;
    function PagePartDir: string; override;
    function PagePartName: string; override;
    procedure InternalSatisfy; override;
  end;

  { TContestProblemsBtnFeature }

  TContestProblemsBtnFeature = class(TEditableNavButtonFeature)
  protected
    function Enabled: boolean; override;
    function PagePartDir: string; override;
    function PagePartName: string; override;
    procedure InternalSatisfy; override;
  end;

  { TContestButtonsFeature }

  TContestButtonsFeature = class(TTesterPageFeature)
  public
    procedure Satisfy; override;
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

  { TContestSettingsFeature }

  TContestSettingsFeature = class(TTesterPageFeature)
  public
    procedure Satisfy; override;
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

implementation

{ TContestProblemsFeature }

procedure TContestProblemsFeature.InternalSatisfy;
var
  List: TContestProblemList;
begin
  // set variables
  with Parent.Variables do
  begin
    ItemsAsText['contentHeaderText'] := SContestProblemsText;
  end;
  // load "add problem" form (if necessary)
  if Transaction.CanWriteData then
  begin
    with Parent.Variables do
    begin
      ItemsAsText['contestAddProblem'] := SContestAddProblem;
      ItemsAsText['contestAddProblemPrompt'] := SContestAddProblemPrompt;
      ItemsAsText['contestAddProblemBtn'] := SContestAddProblemBtn;
    end;
    LoadPagePart('contest', 'contestProblemAdd');
  end;
  // load problem list
  List := TContestProblemList.Create(Parent, Transaction as TContestTransaction);
  try
    Parent.AddElementPagePart('contestProblemList', List);
  finally
    FreeAndNil(List);
  end;
  // load page template
  LoadPagePart('contest', 'contestProblems', 'content');
end;

procedure TContestProblemsFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TPostDataFeature);
  ADependencies.Add(TContentFeature);
  ADependencies.Add(TContestButtonsFeature);
  ADependencies.Add(TEditablePageTitleFeature);
end;

{ TContestProblemsBtnFeature }

function TContestProblemsBtnFeature.Enabled: boolean;
begin
  Result := True;
end;

function TContestProblemsBtnFeature.PagePartDir: string;
begin
  Result := 'contest';
end;

function TContestProblemsBtnFeature.PagePartName: string;
begin
  Result := 'contestProblemsBtn';
end;

procedure TContestProblemsBtnFeature.InternalSatisfy;
begin
  inherited InternalSatisfy;
  Parent.Variables.ItemsAsText['editableReserved2Btn'] := '~+#contestProblemsBtn;';
end;

{ TContestParticipantFeature }

procedure TContestParticipantFeature.BeforeSatisfy;
begin
  inherited BeforeSatisfy;
  FSession := (EditableObject as TContest).CreateParticipantSession(User);
end;

procedure TContestParticipantFeature.InternalSatisfy;
var
  Table: TContestParticipantTable;
begin
  // add variables
  with Parent.Variables do
  begin
    ItemsAsText['objectAddUser'] := SObjectAddUser;
    ItemsAsText['objectAddUserPrompt'] := SObjectAddUserPrompt;
    ItemsAsText['objectAddUserBtn'] := SObjectAddUserBtn;
    ItemsAsText['contentHeaderText'] := SContestParticipantsText;
  end;
  // add "add user" form
  if Session.CanAddParticipant then
    LoadPagePart('editable', 'editableAccessAdd', 'contestParticipantAdd');
  // add participant table
  Table := TContestParticipantTable.Create(Parent, Session);
  try
    Parent.AddElementPagePart('contestParticipantsTable', Table);
  finally
    FreeAndNil(Table);
  end;
  // load page
  LoadPagePart('contest', 'contestParticipants', 'content');
end;

procedure TContestParticipantFeature.AfterSatisfy;
begin
  FreeAndNil(FSession);
  inherited AfterSatisfy;
end;

procedure TContestParticipantFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TPostDataFeature);
  ADependencies.Add(TContentFeature);
  ADependencies.Add(TContestButtonsFeature);
  ADependencies.Add(TEditablePageTitleFeature);
end;

{ TContestParticipantButtonFeature }

function TContestParticipantButtonFeature.Enabled: boolean;
begin
  Result := True;
end;

function TContestParticipantButtonFeature.PagePartDir: string;
begin
  Result := 'contest';
end;

function TContestParticipantButtonFeature.PagePartName: string;
begin
  Result := 'contestParticipantsBtn';
end;

procedure TContestParticipantButtonFeature.InternalSatisfy;
begin
  inherited InternalSatisfy;
  Parent.Variables.ItemsAsText['editableReserved1Btn'] := '~+#contestParticipantsBtn;';
end;

{ TContestSettingsFeature }

procedure TContestSettingsFeature.Satisfy;
begin
  with Parent.Variables do
  begin
    ItemsAsText['editableCloneNamePrompt'] := SContestCloneNamePrompt;
    ItemsAsText['editableCloneObject'] := SContestCloneObject;
    ItemsAsText['editableDeleteObject'] := SContestDeleteObject;
  end;
end;

procedure TContestSettingsFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TContestButtonsFeature);
  ADependencies.Add(TEditableSettingsFeature);
end;

{ TContestButtonsFeature }

procedure TContestButtonsFeature.Satisfy;
begin
  with Parent.Variables do
  begin
    ItemsAsText['contestParticipantsText'] := SContestParticipantsText;
    ItemsAsText['contestProblemsText'] := SContestProblemsText;
  end;
end;

procedure TContestButtonsFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TContestParticipantButtonFeature);
  ADependencies.Add(TContestProblemsBtnFeature);
  ADependencies.Add(TEditableButtonsFeature);
end;

{ TContestViewFeature }

procedure TContestViewFeature.InternalSatisfy;
var
  ContestTransaction: TContestTransaction;
begin
  ContestTransaction := Transaction as TContestTransaction;
  with Parent.Variables do
  begin
    ItemsAsText['contestStartDateValue'] := FormatDateTime(SPreferredDateFormat,
      ContestTransaction.StartTime);
    ItemsAsText['contestStartTimeValue'] := FormatDateTime(SPreferredTimeFormat,
      ContestTransaction.StartTime);
   ItemsAsText['contestDurationValue'] := IntToStr(ContestTransaction.DurationMinutes);
   ItemsAsText['contestScoringPolicyValue'] :=
     SContestScoringPolicies[ContestTransaction.ScoringPolicy];
   ItemsAsText['contestAllowUpsolvingValue'] := SYesNo[ContestTransaction.AllowUpsolving];
  end;
  LoadPagePart('contest', 'contestView', 'objectViewContent');
end;

procedure TContestViewFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TContestEditViewBaseFeature);
  ADependencies.Add(TEditableViewFeature);
  ADependencies.Add(TContestButtonsFeature);
end;

{ TContestEditFeature }

procedure TContestEditFeature.InternalSatisfy;
var
  ContestTransaction: TContestTransaction;
begin
  ContestTransaction := Transaction as TContestTransaction;
  with Parent.Variables do
  begin
    ItemsAsText['objectEditSubmit'] := SContestEditSubmit;
    ItemsAsText['contestDurationMax'] := IntToStr(Config.Contest_MaxDurationMinutes);
    ItemsAsText['contestDurationValue'] := IntToStr(ContestTransaction.DurationMinutes);
    ItemsAsText['spMaxScoreStr'] := SScoringPolicyMaxScore;
    ItemsAsText['spLastScoreStr'] := SScoringPolicyLastScore;
    ItemsAsText[ScoringPolicyToStr(ContestTransaction.ScoringPolicy) + 'Sel'] :=
      ' selected';
    if ContestTransaction.AllowUpsolving then
      ItemsAsText['allowUpsolvingChecked'] := ' checked';
  end;
  LoadPagePart('contest', 'contestEdit', 'objectEditContent');
end;

procedure TContestEditFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TContestEditViewBaseFeature);
  ADependencies.Add(TEditableEditFeature);
  ADependencies.Add(TContestButtonsFeature);
  ADependencies.Add(TDateEditFeature);
  ADependencies.Add(TTimeEditFeature);
end;

{ TContestManageAccessFeature }

procedure TContestManageAccessFeature.Satisfy;
begin
  // do nothing
end;

procedure TContestManageAccessFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TContestButtonsFeature);
  ADependencies.Add(TEditableManageAccessFeature);
end;

{ TContestEditViewBaseFeature }

procedure TContestEditViewBaseFeature.Satisfy;
begin
  with Parent.Variables do
  begin
    ItemsAsText['contestStartDateKey'] := SContestStartDateKey;
    ItemsAsText['contestStartTimeKey'] := SContestStartTimeKey;
    ItemsAsText['contestDurationKey'] := SContestDurationKey;
    ItemsAsText['contestScoringPolicyKey'] := SContestScoringPolicyKey;
    ItemsAsText['contestAllowUpsolvingKey'] := SContestAllowUpsolvingKey;
    ItemsAsText['durationMinutes'] := SDurationMinutes;
  end;
end;

procedure TContestEditViewBaseFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TEditableEditViewBaseFeature);
end;

{ TContestCreateFormFeature }

procedure TContestCreateFormFeature.Satisfy;
begin
  with Parent.Variables do
  begin
    ItemsAsText['editableCreateNamePrompt'] := SContestCreateNamePrompt;
    ItemsAsText['editableCreateTitlePrompt'] := SContestCreateTitlePrompt;
    ItemsAsText['editableCreatePrompt'] := SContestCreatePrompt;
  end;
end;

procedure TContestCreateFormFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TEditableCreateFormFeature);
end;

{ TContestBaseFeature }

procedure TContestBaseFeature.Satisfy;
begin
  with Parent.Variables do
  begin
    ItemsAsText['editableNewRef'] := 'contest-new';
    ItemsAsText['editableNodeDeleteRef'] := 'contest-delete';
    ItemsAsText['editableViewRef'] := 'contest-view';
    ItemsAsText['editableEditRef'] := 'contest-edit';
    ItemsAsText['editableAccessRef'] := 'contest-access';
    ItemsAsText['editableSettingsRef'] := 'contest-settings';
    ItemsAsText['contestParticipantsRef'] := 'contest-participants';
    ItemsAsText['contestProblemsRef'] := 'contest-problems';
  end;
end;

procedure TContestBaseFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TEditableBaseFeature);
end;

end.

