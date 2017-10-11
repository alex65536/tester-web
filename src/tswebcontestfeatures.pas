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
  webstrconsts, contests, tswebcontestelements;

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

  { TContestParticipantButtonFeature }

  TContestParticipantButtonFeature = class(TEditableNavButtonFeature)
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
  end;
end;

procedure TContestButtonsFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TContestParticipantButtonFeature);
  ADependencies.Add(TEditableButtonsFeature);
end;

{ TContestViewFeature }

procedure TContestViewFeature.InternalSatisfy;
begin
  // do nothing
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
begin
  with Parent.Variables do
  begin
    ItemsAsText['objectEditSubmit'] := SContestEditSubmit;
  end;
end;

procedure TContestEditFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TContestEditViewBaseFeature);
  ADependencies.Add(TEditableEditFeature);
  ADependencies.Add(TContestButtonsFeature);
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
  // do nothing
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
  end;
end;

procedure TContestBaseFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TEditableBaseFeature);
end;

end.

