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
unit tswebeditablepages;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, tswebpages, tswebmodules, tswebeditableelements, editableobjects,
  tswebeditablemodules, tswebeditablefeatures, tswebpagesbase, navbars,
  htmlpreprocess;

type

  { TEditableHtmlPage }

  TEditableHtmlPage = class(TDefaultHtmlPage, IEditablePage)
  protected
    function HasEditableObject: boolean; virtual;
    function EditableObject: TEditableObject;
    function DoGetManager: TEditableManager; virtual; abstract;
    function CreateNavBar: TNavBar; override;
    procedure AddFeatures; override;
    procedure DoGetInnerContents(Strings: TIndentTaggedStrings); override;
  public
    function Manager: TEditableManager;
  end;

  { TEditablePostHtmlPage }

  TEditablePostHtmlPage = class(TEditableHtmlPage, IPostHtmlPage)
  private
    FError: string;
    FSuccess: string;
  public
    function GetError: string;
    function GetSuccess: string;
    procedure SetError(AValue: string);
    procedure SetSuccess(AValue: string);
  end;

implementation

{ TEditablePostHtmlPage }

function TEditablePostHtmlPage.GetError: string;
begin
  Result := FError;
end;

function TEditablePostHtmlPage.GetSuccess: string;
begin
  Result := FSuccess;
end;

procedure TEditablePostHtmlPage.SetError(AValue: string);
begin
  FError := AValue;
end;

procedure TEditablePostHtmlPage.SetSuccess(AValue: string);
begin
  FSuccess := AValue;
end;

{ TEditableHtmlPage }

function TEditableHtmlPage.HasEditableObject: boolean;
begin
  Result := True;
end;

function TEditableHtmlPage.EditableObject: TEditableObject;
begin
  if HasEditableObject then
    Result := EditableObjectFromRequest(Request, Manager)
  else
    Result := nil;
end;

function TEditableHtmlPage.CreateNavBar: TNavBar;
begin
  Result := TDefaultNavBar.Create(Self);
end;

procedure TEditableHtmlPage.AddFeatures;
begin
  AddFeature(TEditableBaseFeature);
  inherited AddFeatures;
end;

procedure TEditableHtmlPage.DoGetInnerContents(Strings: TIndentTaggedStrings);
begin
  // do nothing, not necessary
  Strings.Text := '';
end;

function TEditableHtmlPage.Manager: TEditableManager;
begin
  Result := DoGetManager;
end;

end.

