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

  TNavBarElement = class(THtmlPageElement)
  private
    FActive: boolean;
    FCaption: string;
    FLink: string;
  protected
    procedure DoFillVariables; override;
  public
    property Caption: string read FCaption write FCaption;
    property Link: string read FLink write FLink;
    property Active: boolean read FActive write FActive;
  end;

  TNavBarElementList = specialize TFPGObjectList<TNavBarElement>;

  { TNavBar }

  TNavBar = class(THtmlPageElement)
  private
    FActiveCaption: string;
    FElements: TNavBarElementList;
  protected
    procedure BuildElementsList(Strings: TIndentTaggedStrings);
    procedure DoCreateElements; virtual; abstract;
    procedure DoFillVariables; override;
    function DoCreateElement(AParent: THtmlPage): TNavBarElement; virtual; abstract;
  public
    property ActiveCaption: string read FActiveCaption write FActiveCaption;
    property Elements: TNavBarElementList read FElements;
    function CreateElement(const ACaption, ALink: string): TNavBarElement;
    function AddElement(const ACaption, ALink: string): TNavBarElement;
    constructor Create(AParent: THtmlPage);
    destructor Destroy; override;
  end;

  {$interfaces CORBA}
  IPageNavBar = interface
    ['{3952DAAF-61C6-4077-A84C-A09305B3FC4F}']
    function GetNavBar: TNavBar;
    property NavBar: TNavBar read GetNavBar;
  end;
  {$interfaces COM}

implementation

{ TNavBarElement }

procedure TNavBarElement.DoFillVariables;
begin
  with Storage do
  begin
    ItemsAsText['navTitle'] := Caption;
    ItemsAsText['navRef'] := Link;
    if Active then
      ItemsAsText['navIsActive'] := '~#+navActive;';
  end;
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
      with FElements[I] do
      begin
        Active := Self.ActiveCaption = Caption;
        GetContents(Item);
        Clear;
      end;
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
  FElements.Clear;
  try
    DoCreateElements;
    ListInner := TIndentTaggedStrings.Create;
    try
      BuildElementsList(ListInner);
      Storage.SetItemAsStrings('navItems', ListInner);
    finally
      FreeAndNil(ListInner);
    end;
  finally
    FElements.Clear;
  end;
end;

function TNavBar.CreateElement(const ACaption, ALink: string): TNavBarElement;
begin
  Result := DoCreateElement(Parent);
  try
    Result.Caption := ACaption;
    Result.Link := ALink;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TNavBar.AddElement(const ACaption, ALink: string): TNavBarElement;
begin
  Elements.Add(CreateElement(ACaption, ALink));
  Result := Elements.Last;
end;

constructor TNavBar.Create(AParent: THtmlPage);
begin
  inherited Create(AParent);
  FElements := TNavBarElementList.Create(True);
end;

destructor TNavBar.Destroy;
begin
  FreeAndNil(FElements);
  inherited Destroy;
end;

end.
