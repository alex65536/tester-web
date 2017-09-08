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
unit navbars;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, htmlpreprocess, htmlpages, serverconfig, fgl;

type
  TNavBar = class;

  { TNavBarElement }

  TNavBarElement = class
  private
    FActive: boolean;
    FStorage: TVariableStorage;
    FCaption: string;
    FLink: string;
    FParent: TNavBar;
  protected
    procedure DoFillVariables; virtual;
    procedure DoGetSkeleton(Strings: TIndentTaggedStrings); virtual; abstract;
  public
    property Storage: TVariableStorage read FStorage;
    property Parent: TNavBar read FParent;
    property Caption: string read FCaption write FCaption;
    property Link: string read FLink write FLink;
    property Active: boolean read FActive write FActive;
    procedure GetContents(Strings: TIndentTaggedStrings);
    constructor Create(AParent: TNavBar);
    destructor Destroy; override;
  end;

  TNavBarElementList = specialize TFPGObjectList<TNavBarElement>;

  { TNavBar }

  TNavBar = class
  private
    FActiveCaption: string;
    FStorage: TVariableStorage;
    FElements: TNavBarElementList;
    FParent: THtmlPage;
  protected
    property Storage: TVariableStorage read FStorage;
    procedure BuildElementsList(Strings: TIndentTaggedStrings);
    procedure CreateElements; virtual; abstract;
    procedure DoFillVariables; virtual;
    procedure DoGetSkeleton(Strings: TIndentTaggedStrings); virtual; abstract;
    function DoCreateElement(AParent: TNavBar): TNavBarElement; virtual; abstract;
  public
    property ActiveCaption: string read FActiveCaption write FActiveCaption;
    property Parent: THtmlPage read FParent;
    property Elements: TNavBarElementList read FElements;
    procedure GetContents(Strings: TIndentTaggedStrings);
    function CreateElement(const ACaption, ALink: string): TNavBarElement;
    constructor Create(AParent: THtmlPage);
    destructor Destroy; override;
  end;

  { IPageNavBar }

  IPageNavBar = interface
    ['{3952DAAF-61C6-4077-A84C-A09305B3FC4F}']
    function GetNavBar: TNavBar;
    property NavBar: TNavBar read GetNavBar;
  end;

implementation

{ TNavBarElement }

procedure TNavBarElement.DoFillVariables;
begin
  with FStorage do
  begin
    ItemsAsText['navTitle'] := Caption;
    ItemsAsText['navRef'] := Link;
    if Active then
      ItemsAsText['navIsActive'] := '~#+navActive;';
  end;
end;

procedure TNavBarElement.GetContents(Strings: TIndentTaggedStrings);
var
  Source: TIndentTaggedStrings;
begin
  Source := TIndentTaggedStrings.Create;
  try
    DoGetSkeleton(Source);
    Parent.Parent.Preprocessor.Preprocess(Source, Strings);
  finally
    FreeAndNil(Source);
  end;
end;

constructor TNavBarElement.Create(AParent: TNavBar);
begin
  FParent := AParent;
  with Parent.Parent.Preprocessor do
  begin
    FStorage := TTreeVariableStorage.Create(Storages);
    Storages.Add(FStorage);
  end;
end;

destructor TNavBarElement.Destroy;
begin
  FreeAndNil(FStorage);
  inherited Destroy;
end;

{ TNavBar }

procedure TNavBar.BuildElementsList(Strings: TIndentTaggedStrings);
var
  I: integer;
  Item: TIndentTaggedStrings;
begin
  for I := 0 to FElements.Count - 1 do
  begin
    Item := TIndentTaggedStrings.Create;
    try
      FElements[I].GetContents(Item);
      Strings.AppendIndentedLines(Item, True);
    finally
      FreeAndNil(Item);
    end;
  end;
end;

procedure TNavBar.DoFillVariables;
var
  ListInner: TIndentTaggedStrings;
begin
  ListInner := TIndentTaggedStrings.Create;
  try
    BuildElementsList(ListInner);
    FStorage.SetItemAsStrings('navItems', ListInner);
  finally
    FreeAndNil(ListInner);
  end;
end;

procedure TNavBar.GetContents(Strings: TIndentTaggedStrings);
var
  Source: TIndentTaggedStrings;
begin
  Source := TIndentTaggedStrings.Create;
  try
    DoGetSkeleton(Source);
    Parent.Preprocessor.Preprocess(Source, Strings);
  finally
    FreeAndNil(Source);
  end;
end;

function TNavBar.CreateElement(const ACaption, ALink: string): TNavBarElement;
begin
  Result := DoCreateElement(Self);
  try
    Result.Caption := ACaption;
    Result.Link := ALink;
    Result.Active := ACaption = ActiveCaption;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

constructor TNavBar.Create(AParent: THtmlPage);
begin
  FParent := AParent;
  FElements := TNavBarElementList.Create(True);
  with Parent.Preprocessor do
  begin
    FStorage := TTreeVariableStorage.Create(Storages);
    Storages.Add(FStorage);
  end;
end;

destructor TNavBar.Destroy;
begin
  FreeAndNil(FElements);
  FreeAndNil(FStorage);
  inherited Destroy;
end;

end.
