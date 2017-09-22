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
  Classes, SysUtils, tswebpagesbase, editableobjects, htmlpages, htmlpreprocess,
  tswebfeatures, webstrconsts;

type
  {$interfaces CORBA}
  IEditablePage = interface
    ['{DC40AF42-023F-4781-B994-71228705D8A9}']
    function Manager: TEditableManager;
  end;
  {$interfaces COM}

  { TEditableBaseFeature }

  TEditableBaseFeature = class(TTesterPageFeature)
  public
    procedure Satisfy; override;
  end;

  { TEditableObjListNode }

  TEditableObjListNode = class(TTesterHtmlPageElement)
  private
    FEditableObject: TEditableObject;
    FManagerSession: TEditableManagerSession;
  protected
    procedure DoFillVariables; override;
    procedure DoGetSkeleton(Strings: TIndentTaggedStrings); override;
  public
    property EditableObject: TEditableObject read FEditableObject;
    constructor Create(AParent: THtmlPage; AObject: TEditableObject);
    destructor Destroy; override;
  end;

  { TEditableObjList }

  TEditableObjList = class(TTesterHtmlPageElement)
  private
    FManager: TEditableManager;
  protected
    procedure FillItems(AItems: TIndentTaggedStrings);
    procedure DoFillVariables; override;
    procedure DoGetSkeleton(Strings: TIndentTaggedStrings); override;
  public
    property Manager: TEditableManager read FManager;
    constructor Create(AParent: THtmlPage; AManager: TEditableManager);
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

implementation

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
end;

procedure TEditableCreateFormFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TEditableBaseFeature);
  ADependencies.Add(TPostDataFeature);
  ADependencies.Add(TSessionTokenFeature);
end;

{ TEditableObjListFeature }

procedure TEditableObjListFeature.Satisfy;
var
  List: TEditableObjList;
  Strings: TIndentTaggedStrings;
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
    Strings := TIndentTaggedStrings.Create;
    try
      List.GetContents(Strings);
      Parent.PageParts.SetItemAsStrings('editableObjListTable', Strings);
      LoadPagePart('editable', 'editableObjList');
    finally
      FreeAndNil(Strings);
    end;
  finally
    FreeAndNil(List);
  end;
end;

procedure TEditableObjListFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TEditableBaseFeature);
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

{ TEditableObjListNode }

procedure TEditableObjListNode.DoFillVariables;
var
  DeleteNodeName: string;
begin
  with Storage, FEditableObject do
  begin
    ItemsAsText['objectNodeName'] := Name;
    ItemsAsText['objectNodeOwner'] := Parent.GenerateUserLink(GetObjectAuthorName);
    ItemsAsText['objectNodeRights'] := SAccessRightsNames[GetAccessRights(Parent.User as TEditorUser)];
    ItemsAsText['objectNodeLastModified'] := 'TODO!'; // TODO : add objectNodeLastModified !!!
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
end;

destructor TEditableObjListNode.Destroy;
begin
  FreeAndNil(FManagerSession);
  FreeAndNil(FEditableObject);
  inherited Destroy;
end;

{ TEditableObjList }

procedure TEditableObjList.FillItems(AItems: TIndentTaggedStrings);
var
  ObjList: TStringList;
  ObjName: string;
  ObjContent: TIndentTaggedStrings;
begin
  AItems.Clear;
  ObjList := FManager.ListAvailableObjects(Parent.User as TEditorUser);
  try
    ObjContent := TIndentTaggedStrings.Create;
    try
      for ObjName in ObjList do
        with TEditableObjListNode.Create(Parent, FManager.GetObject(ObjName)) do
          try
            GetContents(ObjContent);
            AItems.AppendIndentedLines(ObjContent, True);
            ObjContent.Clear;
          finally
            Free;
          end;
    finally
      FreeAndNil(ObjContent);
    end;
  finally
    FreeAndNil(ObjList);
  end;
end;

procedure TEditableObjList.DoFillVariables;
var
  Strings: TIndentTaggedStrings;
begin
  Strings := TIndentTaggedStrings.Create;
  try
    FillItems(Strings);
    Storage.SetItemAsStrings('editableObjListNodes', Strings);
  finally
    FreeAndNil(Strings);
  end;
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

end.

