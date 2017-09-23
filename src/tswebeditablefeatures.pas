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
unit tswebeditablefeatures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, editableobjects, htmlpages, tswebfeatures, webstrconsts,
  userpages, tswebeditableelements;

type

  { TEditableObjectFeature }

  TEditableObjectFeature = class(TTesterPageFeature)
  private
    FEditableObject: TEditableObject;
  protected
    procedure InternalSatisfy; virtual; abstract;
    procedure BeforeSatisfy; virtual;
    procedure AfterSatisfy; virtual;
  public
    function User: TEditorUser;
    property EditableObject: TEditableObject read FEditableObject;
    procedure Satisfy; override;
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

  { TEditableTransactionPageFeature }

  TEditableTransactionPageFeature = class(TEditableObjectFeature)
  private
    FTransaction: TEditableTransaction;
  protected
    procedure BeforeSatisfy; override;
    procedure AfterSatisfy; override;
  public
    property Transaction: TEditableTransaction read FTransaction;
  end;

  { TEditableBaseFeature }

  TEditableBaseFeature = class(TTesterPageFeature)
  public
    procedure Satisfy; override;
  end;

  { TEditableObjectBaseFeature }

  TEditableObjectBaseFeature = class(TEditableObjectFeature)
  protected
    procedure InternalSatisfy; override;
  end;

  { TEditablePageTitleFeature }

  TEditablePageTitleFeature = class(TEditableTransactionPageFeature)
  protected
    procedure InternalSatisfy; override;
  public
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

  { TEditableObjListFeature }

  TEditableObjListFeature = class(TTesterPageFeature)
  public
    procedure Satisfy; override;
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

  { TEditableCreateFormFeature }

  TEditableCreateFormFeature = class(TTesterPageFeature)
  public
    procedure Satisfy; override;
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

  { TEditableNavButtonBaseFeature }

  TEditableNavButtonBaseFeature = class(TTesterPageFeature)
  public
    procedure Satisfy; override;
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

  { TEditableNavButtonFeature }

  TEditableNavButtonFeature = class(TEditableObjectFeature)
  protected
    function Enabled: boolean; virtual; abstract;
    function PagePartName: string; virtual; abstract;
    function BtnInnerName: string; virtual;
    procedure InternalSatisfy; override;
  public
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

  { TEditableViewButtonFeature }

  TEditableViewButtonFeature = class(TEditableNavButtonFeature)
  protected
    function Enabled: boolean; override;
    function PagePartName: string; override;
  end;

  { TEditableEditButtonFeature }

  TEditableEditButtonFeature = class(TEditableNavButtonFeature)
  protected
    function Enabled: boolean; override;
    function PagePartName: string; override;
  end;

  { TEditableAccessButtonFeature }

  TEditableAccessButtonFeature = class(TEditableNavButtonFeature)
  protected
    function Enabled: boolean; override;
    function PagePartName: string; override;
  end;

  { TEditableButtonsFeature }

  TEditableButtonsFeature = class(TTesterPageFeature)
  public
    procedure Satisfy; override;
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

  { TEditableManageAccessFeature }

  TEditableManageAccessFeature = class(TEditableObjectFeature)
  protected
    procedure InternalSatisfy; override;
  public
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

  { TEditableEditViewBaseFeature }

  TEditableEditViewBaseFeature = class(TEditableTransactionPageFeature)
  protected
    procedure InternalSatisfy; override;
  public
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

  { TEditableViewFeature }

  TEditableViewFeature = class(TTesterPageFeature)
  public
    procedure Satisfy; override;
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

  { TEditableEditFeature }

  TEditableEditFeature = class(TTesterPageFeature)
  public
    procedure Satisfy; override;
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

implementation

{ TEditableEditFeature }

procedure TEditableEditFeature.Satisfy;
begin
  with Parent.Variables do
  begin
    ItemsAsText['contentHeaderText'] := SEditableEditText;
    ItemsAsText['objectEditSubmit'] := SObjectEditSubmit;
  end;
  LoadPagePart('editable', 'editableEdit', 'content');
end;

procedure TEditableEditFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TEditableEditViewBaseFeature);
  ADependencies.Add(TPostDataFeature);
end;

{ TEditableViewFeature }

procedure TEditableViewFeature.Satisfy;
begin
  Parent.Variables.ItemsAsText['contentHeaderText'] := SEditableViewText;
  LoadPagePart('editable', 'editableView', 'content');
end;

procedure TEditableViewFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TEditableEditViewBaseFeature);
end;

{ TEditableEditViewBaseFeature }

procedure TEditableEditViewBaseFeature.InternalSatisfy;
begin
  with Parent.Variables do
  begin
    ItemsAsText['objectTitleKey'] := SObjectTitleKey;
    ItemsAsText['objectTitleValue'] := Transaction.Title;
    ItemsAsText['objectTitlePrompt'] := SObjectTitlePrompt;
  end;
end;

procedure TEditableEditViewBaseFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TEditableBaseFeature);
  ADependencies.Add(TEditableButtonsFeature);
  ADependencies.Add(TContentFeature);
  ADependencies.Add(TEditablePageTitleFeature);
end;

{ TEditableManageAccessFeature }

procedure TEditableManageAccessFeature.InternalSatisfy;
var
  Session: TEditableObjectAccessSession;
  List: TEditableAccessNodeList;
begin
  Session := EditableObject.CreateAccessSession(User);
  try
    // title
    Parent.Variables.ItemsAsText['contentHeaderText'] := SEditableAccessText;
    // "add user" panel
    if Session.CanAddUser then
    begin
      with Parent.Variables do
      begin
        ItemsAsText['objectAddUser'] := SObjectAddUser;
        ItemsAsText['objectAddUserPrompt'] := SObjectAddUserPrompt;
        ItemsAsText['objectAddUserBtn'] := SObjectAddUserBtn;
      end;
      LoadPagePart('editable', 'editableAccessAdd');
    end;
    // access table
    List := TEditableAccessNodeList.Create(Parent, Session);
    try
      Parent.AddElementPagePart('editableAccessTable', List)
    finally
      FreeAndNil(List);
    end;
    // load page content
    LoadPagePart('editable', 'editableAccess', 'content');
  finally
    FreeAndNil(Session);
  end;
end;

procedure TEditableManageAccessFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TEditableBaseFeature);
  ADependencies.Add(TContentFeature);
  ADependencies.Add(TPostDataFeature);
  ADependencies.Add(TEditableButtonsFeature);
  ADependencies.Add(TEditablePageTitleFeature);
end;

{ TEditableButtonsFeature }

procedure TEditableButtonsFeature.Satisfy;
begin
  with Parent.Variables do
  begin
    ItemsAsText['editableViewText'] := SEditableViewText;
    ItemsAsText['editableEditText'] := SEditableEditText;
    ItemsAsText['editableAccessText'] := SEditableAccessText;
  end;
end;

procedure TEditableButtonsFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TEditableViewButtonFeature);
  ADependencies.Add(TEditableEditButtonFeature);
  ADependencies.Add(TEditableAccessButtonFeature);
end;

{ TEditableAccessButtonFeature }

function TEditableAccessButtonFeature.Enabled: boolean;
begin
  Result := EditableObject.GetAccessRights(User) in AccessCanReadSet;
end;

function TEditableAccessButtonFeature.PagePartName: string;
begin
  Result := 'editableAccessBtn';
end;

{ TEditableEditButtonFeature }

function TEditableEditButtonFeature.Enabled: boolean;
begin
  Result := EditableObject.GetAccessRights(User) in AccessCanWriteSet;
end;

function TEditableEditButtonFeature.PagePartName: string;
begin
  Result := 'editableEditBtn';
end;

{ TEditableViewButtonFeature }

function TEditableViewButtonFeature.Enabled: boolean;
begin
  Result := EditableObject.GetAccessRights(User) in AccessCanReadSet;
end;

function TEditableViewButtonFeature.PagePartName: string;
begin
  Result := 'editableViewBtn';
end;

{ TEditableNavButtonFeature }

function TEditableNavButtonFeature.BtnInnerName: string;
begin
  Result := PagePartName + 'Inner';
end;

procedure TEditableNavButtonFeature.InternalSatisfy;
var
  BtnInnerVar: string;
begin
  if Enabled then
    BtnInnerVar := '~+#editableLinkEnabled;'
  else
    BtnInnerVar := '~+#editableLinkDisabled;';
  Parent.Variables.ItemsAsText[BtnInnerName] := BtnInnerVar;
  LoadPagePart('editable', PagePartName);
end;

procedure TEditableNavButtonFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TEditableNavButtonBaseFeature);
end;

{ TEditableNavButtonBaseFeature }

procedure TEditableNavButtonBaseFeature.Satisfy;
begin
  LoadPagePart('editable', 'editableLinkEnabled');
  LoadPagePart('editable', 'editableLinkDisabled');
end;

procedure TEditableNavButtonBaseFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TEditableObjectBaseFeature);
end;

{ TEditableCreateFormFeature }

procedure TEditableCreateFormFeature.Satisfy;
begin
  with Parent.Variables do
  begin
    ItemsAsText['editableCreateName'] := SEditableCreateName;
    ItemsAsText['editableCreateNamePrompt'] := SEditableCreateNamePrompt;
    ItemsAsText['editableCreateTitle'] := SEditableCreateTitle;
    ItemsAsText['editableCreateTitlePrompt'] := SEditableCreateTitlePrompt;
    ItemsAsText['editableCreateSubmit'] := SEditableCreateSubmit;
    ItemsAsText['editableCreatePrompt'] := SEditableCreatePrompt;
  end;
  LoadPagePart('editable', 'editableCreateForm', 'content');
end;

procedure TEditableCreateFormFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TEditableBaseFeature);
  ADependencies.Add(TContentFeature);
  ADependencies.Add(TPostDataFeature);
end;

{ TEditableObjListFeature }

procedure TEditableObjListFeature.Satisfy;
var
  List: TEditableObjList;
begin
  // load header names
  with Parent.Variables do
  begin
    ItemsAsText['editableListName'] := SEditableListName;
    ItemsAsText['editableListTitle'] := SEditableListTitle;
    ItemsAsText['editableListAuthor'] := SEditableListAuthor;
    ItemsAsText['editableListAccess'] := SEditableListAccess;
    ItemsAsText['editableListLastModified'] := SEditableListLastModified;
    ItemsAsText['editableListDelete'] := SEditableListDelete;
  end;
  // load list
  List := TEditableObjList.Create(Parent, (Parent as IEditablePage).Manager);
  try
    Parent.AddElementPagePart('editableObjListTable', List)
  finally
    FreeAndNil(List);
  end;
  // load content
  LoadPagePart('editable', 'editableObjList', 'content');
end;

procedure TEditableObjListFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TEditableBaseFeature);
  ADependencies.Add(TContentFeature);
end;

{ TEditablePageTitleFeature }

procedure TEditablePageTitleFeature.InternalSatisfy;
begin
  with Parent.Variables do
  begin
    ItemsAsText['pageHeader'] := Transaction.Title;
    ItemsAsText['title'] := '~pageHeader;: ~contentHeaderText;';
  end;
end;

procedure TEditablePageTitleFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(THeaderFeature);
  ADependencies.Add(TContentHeaderFeature);
end;

{ TEditableObjectBaseFeature }

procedure TEditableObjectBaseFeature.InternalSatisfy;
begin
  with Parent.Variables do
  begin
    ItemsAsText['objectName'] := EditableObject.Name;
  end;
end;

{ TEditableBaseFeature }

procedure TEditableBaseFeature.Satisfy;
begin
  with Parent.Variables do
  begin
    ItemsAsText['editableDelete'] := SEditableDelete;
    ItemsAsText['editableNewText'] := SEditableNewText;
  end;
end;

{ TEditableTransactionPageFeature }

procedure TEditableTransactionPageFeature.BeforeSatisfy;
begin
  inherited BeforeSatisfy;
  FTransaction := EditableObject.CreateTransaction(User);
end;

procedure TEditableTransactionPageFeature.AfterSatisfy;
begin
  FreeAndNil(FTransaction);
  inherited AfterSatisfy;
end;

{ TEditableObjectFeature }

function TEditableObjectFeature.User: TEditorUser;
begin
  Result := (Parent as TUserPage).User as TEditorUser;
end;

procedure TEditableObjectFeature.BeforeSatisfy;
begin
  FEditableObject := (Parent as IEditablePage).EditableObject;
end;

procedure TEditableObjectFeature.AfterSatisfy;
begin
  FreeAndNil(FEditableObject);
end;

procedure TEditableObjectFeature.Satisfy;
begin
  BeforeSatisfy;
  try
    InternalSatisfy;
  finally
    AfterSatisfy;
  end;
end;

procedure TEditableObjectFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TEditableBaseFeature);
end;

end.

