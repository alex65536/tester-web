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
unit tswebeditableelements;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, editableobjects, htmlpages, tswebpagesbase, htmlpreprocess,
  users, webstrconsts;

type
  {$interfaces CORBA}

  { IEditablePage }

  IEditablePage = interface
    ['{DC40AF42-023F-4781-B994-71228705D8A9}']
    function Manager: TEditableManager;
    function EditableObject: TEditableObject;
  end;
  {$interfaces COM}

  { TEditableObjListNode }

  TEditableObjListNode = class(TTesterHtmlPageElement)
  private
    FEditableObject: TEditableObject;
    FManagerSession: TEditableManagerSession;
    FTransaction: TEditableTransaction;
  protected
    property ManagerSession: TEditableManagerSession read FManagerSession;
    property Transaction: TEditableTransaction read FTransaction;
    procedure DoFillVariables; override;
    procedure DoGetSkeleton(Strings: TIndentTaggedStrings); override;
  public
    property EditableObject: TEditableObject read FEditableObject;
    constructor Create(AParent: THtmlPage; AObject: TEditableObject);
    destructor Destroy; override;
  end;

  { TEditableObjList }

  TEditableObjList = class(TTesterHtmlListedPageElement)
  private
    FManager: TEditableManager;
  protected
    procedure FillList;
    procedure DoFillVariables; override;
    procedure DoGetSkeleton(Strings: TIndentTaggedStrings); override;
  public
    property Manager: TEditableManager read FManager;
    constructor Create(AParent: THtmlPage; AManager: TEditableManager);
  end;

  { TEditableAccessRightsOption }

  TEditableAccessRightsOption = class(TTesterHtmlPageElement)
  private
    FRights: TEditableAccessRights;
    FSelected: boolean;
    FSession: TEditableObjectAccessSession;
  protected
    property Session: TEditableObjectAccessSession read FSession;
    procedure DoFillVariables; override;
    procedure DoGetSkeleton(Strings: TIndentTaggedStrings); override;
  public
    property Rights: TEditableAccessRights read FRights;
    property Selected: boolean read FSelected;
    constructor Create(AParent: THtmlPage; ASession: TEditableObjectAccessSession;
      ARights: TEditableAccessRights; ASelected: boolean);
  end;

  { TEditableAccessNode }

  TEditableAccessNode = class(TTesterHtmlListedPageElement)
  private
    FIndex: integer;
    FSession: TEditableObjectAccessSession;
    FTarget: TUserInfo;
  protected
    property Session: TEditableObjectAccessSession read FSession;
    property Target: TUserInfo read FTarget;
    procedure FillList;
    procedure DoFillVariables; override;
    procedure DoGetSkeleton(Strings: TIndentTaggedStrings); override;
  public
    property Index: integer read FIndex;
    constructor Create(AParent: THtmlPage; ASession: TEditableObjectAccessSession;
      ATarget: TUserInfo; AIndex: integer);
    destructor Destroy; override;
  end;

  { TEditableAccessNodeList }

  TEditableAccessNodeList = class(TTesterHtmlListedPageElement)
  private
    FSession: TEditableObjectAccessSession;
  protected
    property Session: TEditableObjectAccessSession read FSession;
    procedure FillList;
    procedure DoFillVariables; override;
    procedure DoGetSkeleton(Strings: TIndentTaggedStrings); override;
  public
    constructor Create(AParent: THtmlPage; ASession: TEditableObjectAccessSession);
  end;

implementation

{ TEditableAccessNodeList }

procedure TEditableAccessNodeList.FillList;
var
  I: integer;
  AccessUsers: TStringList;
begin
  List.Clear;
  AccessUsers := Session.EditableObject.ListUsers;
  try
    for I := 0 to AccessUsers.Count - 1 do
      List.Add(TEditableAccessNode.Create(Parent, Session,
        UserManager.GetUserInfo(AccessUsers[I]), I + 1));
  finally
    FreeAndNil(AccessUsers);
  end;
end;

procedure TEditableAccessNodeList.DoFillVariables;
begin
  with Parent.Variables do
  begin
    ItemsAsText['editableAccessId'] := SEditableAccessId;
	  ItemsAsText['editableAccessUsername'] := SEditableAccessUsername;
	  ItemsAsText['editableAccessRights'] := SEditableAccessRights;
	  ItemsAsText['editableAccessDelete'] := SEditableAccessDelete;
  end;
  FillList;
  AddListToVariable('editableAccessNodes');
  List.Clear;
end;

procedure TEditableAccessNodeList.DoGetSkeleton(Strings: TIndentTaggedStrings);
begin
  Strings.LoadFromFile(TemplateLocation('editable', 'editableAccessTable'));
end;

constructor TEditableAccessNodeList.Create(AParent: THtmlPage;
  ASession: TEditableObjectAccessSession);
begin
  inherited Create(AParent);
  FSession := ASession;
end;

{ TEditableAccessNode }

procedure TEditableAccessNode.FillList;
var
  R: TEditableAccessRights;
  TargetRights: TEditableAccessRights;
begin
  List.Clear;
  TargetRights := Session.EditableObject.GetAccessRights(Target);
  for R in TEditableAccessRights do
    if Session.CanGrantAccessRights(Target, R) then
      List.Add(TEditableAccessRightsOption.Create(Parent, Session, R, R = TargetRights));
end;

procedure TEditableAccessNode.DoFillVariables;
var
  DeleteBtnLocation: string;
  TargetRights: TEditableAccessRights;
begin
  with Storage do
  begin
    ItemsAsText['objectNodeNum'] := IntToStr(Index);
    ItemsAsText['objectNodeUser'] := Parent.GenerateUserLink(Target);
    ItemsAsText['objectNodeAccessChange'] := SObjectNodeAccessChange;
    ItemsAsText['objectNodeUsername'] := Target.Username;
    // fill "edit rights" column
    FillList;
    if List.Count > 1 then
    begin
      AddListToVariable('objectNodeEditRightsOptions');
      SetFromFile('objectNodeRightsEdit', TemplateLocation('editable', 'editableEditRights'));
    end
    else
    begin
      TargetRights := Session.EditableObject.GetAccessRights(Target);
      ItemsAsText['objectUserRights'] := SAccessRightsNames[TargetRights];
      SetFromFile('objectNodeRightsEdit', TemplateLocation('editable', 'editableViewRights'));
    end;
    List.Clear;
    // fill "delete" column
    if Session.CanDeleteUser(Target) then
      DeleteBtnLocation := 'editableDeleteUserEnabled'
    else
      DeleteBtnLocation := 'editableDeleteUserDisabled';
    SetFromFile('objectNodeDelete', TemplateLocation('editable', DeleteBtnLocation));
  end;
end;

procedure TEditableAccessNode.DoGetSkeleton(Strings: TIndentTaggedStrings);
begin
  Strings.LoadFromFile(TemplateLocation('editable', 'editableAccessNode'));
end;

constructor TEditableAccessNode.Create(AParent: THtmlPage;
  ASession: TEditableObjectAccessSession; ATarget: TUserInfo; AIndex: integer);
begin
  inherited Create(AParent);
  FSession := ASession;
  FTarget := ATarget;
  FIndex := AIndex;
end;

destructor TEditableAccessNode.Destroy;
begin
  FreeAndNil(FTarget);
  inherited Destroy;
end;

{ TEditableAccessRightsOption }

procedure TEditableAccessRightsOption.DoFillVariables;
begin
  with Storage do
  begin
    ItemsAsText['rightsFullName'] := AccessRightsToStr(Rights);
    ItemsAsText['rightsName'] := SAccessRightsNames[Rights];
    if Selected then
      ItemsAsText['rightsSelected'] := ' selected'
    else
      ItemsAsText['rightsSelected'] := '';
  end;
end;

procedure TEditableAccessRightsOption.DoGetSkeleton(Strings: TIndentTaggedStrings);
begin
  Strings.LoadFromFile(TemplateLocation('editable', 'editableEditRightsOption'));
end;

constructor TEditableAccessRightsOption.Create(AParent: THtmlPage;
  ASession: TEditableObjectAccessSession; ARights: TEditableAccessRights;
  ASelected: boolean);
begin
  inherited Create(AParent);
  FSession := ASession;
  FRights := ARights;
  FSelected := ASelected;
end;

{ TEditableObjList }

procedure TEditableObjList.FillList;
var
  ObjList: TStringList;
  ObjName: string;
begin
  List.Clear;
  ObjList := FManager.ListAvailableObjects(Parent.User as TEditorUser);
  try
    for ObjName in ObjList do
      List.Add(TEditableObjListNode.Create(Parent, FManager.GetObject(ObjName)))
  finally
    FreeAndNil(ObjList);
  end;
end;

procedure TEditableObjList.DoFillVariables;
begin
  FillList;
  AddListToVariable('editableObjListNodes');
  List.Clear;
end;

procedure TEditableObjList.DoGetSkeleton(Strings: TIndentTaggedStrings);
begin
  Strings.LoadFromFile(TemplateLocation('editable', 'editableObjListTable'));
end;

constructor TEditableObjList.Create(AParent: THtmlPage;
  AManager: TEditableManager);
begin
  inherited Create(AParent);
  FManager := AManager;
end;

{ TEditableObjListNode }

procedure TEditableObjListNode.DoFillVariables;
var
  DeleteNodeName: string;
begin
  with Storage, FEditableObject do
  begin
    ItemsAsText['objectNodeName'] := Name;
    ItemsAsText['objectNodeTitle'] := FTransaction.Title;
    ItemsAsText['objectNodeOwner'] := Parent.GenerateUserLink(GetObjectAuthorName);
    ItemsAsText['objectNodeRights'] := SAccessRightsNames[GetAccessRights(Parent.User as TEditorUser)];
    ItemsAsText['objectNodeLastModified'] := FormatDateTime(SPreferredDateTimeFormat,
      FTransaction.LastModifyTime);
    if FManagerSession.CanDeleteObject(Name) then
      DeleteNodeName := 'editableADeleteEnabled'
    else
      DeleteNodeName := 'editableADeleteDisabled';
    SetFromFile('objectNodeDelete', TemplateLocation('editable', DeleteNodeName));
  end;
end;

procedure TEditableObjListNode.DoGetSkeleton(Strings: TIndentTaggedStrings);
begin
  Strings.LoadFromFile(TemplateLocation('editable', 'editableObjListNode'));
end;

constructor TEditableObjListNode.Create(AParent: THtmlPage;
  AObject: TEditableObject);
begin
  inherited Create(AParent);
  FEditableObject := AObject;
  FManagerSession := FEditableObject.Manager.CreateManagerSession(Parent.User as TEditorUser);
  FTransaction := FEditableObject.CreateTransaction(Parent.User as TEditorUser);
end;

destructor TEditableObjListNode.Destroy;
begin
  FreeAndNil(FTransaction);
  FreeAndNil(FManagerSession);
  FreeAndNil(FEditableObject);
  inherited Destroy;
end;

end.

