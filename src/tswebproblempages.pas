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
unit tswebproblempages;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, tswebmodules, tswebeditablefeatures, tswebeditablemodules,
  webstrconsts, problems, editableobjects, navbars, tswebpagesbase, tswebpages,
  htmlpreprocess, tswebeditableelements, tswebproblemfeatures, tswebmanagers;

type

  { TProblemHtmlPage }

  TProblemHtmlPage = class(TDefaultHtmlPage, IEditablePage)
  protected
    function HasEditableObject: boolean; virtual;
    function EditableObject: TEditableObject;
    function Manager: TEditableManager;
    function CreateNavBar: TNavBar; override;
    procedure AddFeatures; override;
    procedure DoGetInnerContents(Strings: TIndentTaggedStrings); override;
  end;

  { TProblemPostHtmlPage }

  TProblemPostHtmlPage = class(TProblemHtmlPage, IPostHtmlPage)
  private
    FError: string;
    FSuccess: string;
  public
    function GetError: string;
    function GetSuccess: string;
    procedure SetError(AValue: string);
    procedure SetSuccess(AValue: string);
  end;

  { TProblemListPage }

  TProblemListPage = class(TProblemHtmlPage)
  protected
    procedure AddFeatures; override;
    function HasEditableObject: boolean; override;
  public
    procedure AfterConstruction; override;
  end;

  { TProblemCreateNewPage }

  TProblemCreateNewPage = class(TProblemPostHtmlPage)
  protected
    procedure AddFeatures; override;
    function HasEditableObject: boolean; override;
  public
    procedure AfterConstruction; override;
  end;

  { TProblemAccessPage }

  TProblemAccessPage = class(TProblemPostHtmlPage)
  protected
    procedure AddFeatures; override;
  end;

  { TProblemViewPage }

  TProblemViewPage = class(TProblemHtmlPage)
  protected
    procedure AddFeatures; override;
  end;

  { TProblemEditPage }

  TProblemEditPage = class(TProblemPostHtmlPage)
  protected
    procedure AddFeatures; override;
  end;

implementation

{ TProblemEditPage }

procedure TProblemEditPage.AddFeatures;
begin
  inherited AddFeatures;
  AddFeature(TProblemEditFeature);
end;

{ TProblemViewPage }

procedure TProblemViewPage.AddFeatures;
begin
  inherited AddFeatures;
  AddFeature(TProblemViewFeature);
end;

{ TProblemAccessPage }

procedure TProblemAccessPage.AddFeatures;
begin
  inherited AddFeatures;
  AddFeature(TEditableManageAccessFeature);
end;

{ TProblemCreateNewPage }

procedure TProblemCreateNewPage.AddFeatures;
begin
  inherited AddFeatures;
  AddFeature(TProblemCreateFormFeature);
end;

function TProblemCreateNewPage.HasEditableObject: boolean;
begin
  Result := False;
end;

procedure TProblemCreateNewPage.AfterConstruction;
begin
  inherited AfterConstruction;
  Title := SProblemCreateNew;
end;

{ TProblemListPage }

procedure TProblemListPage.AddFeatures;
begin
  inherited AddFeatures;
  AddFeature(TEditableObjListFeature);
end;

function TProblemListPage.HasEditableObject: boolean;
begin
  Result := False;
end;

procedure TProblemListPage.AfterConstruction;
begin
  inherited AfterConstruction;
  Title := SProblemList;
end;

{ TProblemPostHtmlPage }

function TProblemPostHtmlPage.GetError: string;
begin
  Result := FError;
end;

function TProblemPostHtmlPage.GetSuccess: string;
begin
  Result := FSuccess;
end;

procedure TProblemPostHtmlPage.SetError(AValue: string);
begin
  FError := AValue;
end;

procedure TProblemPostHtmlPage.SetSuccess(AValue: string);
begin
  FSuccess := AValue;
end;

{ TProblemHtmlPage }

function TProblemHtmlPage.HasEditableObject: boolean;
begin
  Result := True;
end;

function TProblemHtmlPage.EditableObject: TEditableObject;
begin
  if HasEditableObject then
    Result := EditableObjectFromRequest(Request, Manager)
  else
    Result := nil;
end;

function TProblemHtmlPage.Manager: TEditableManager;
begin
  Result := ProblemManager;
end;

function TProblemHtmlPage.CreateNavBar: TNavBar;
begin
  Result := TDefaultNavBar.Create(Self);
end;

procedure TProblemHtmlPage.AddFeatures;
begin
  AddFeature(TProblemBaseFeature);
  inherited AddFeatures;
end;

procedure TProblemHtmlPage.DoGetInnerContents(Strings: TIndentTaggedStrings);
begin
  // do nothing, not necessary
  Strings.Text := '';
end;

end.

