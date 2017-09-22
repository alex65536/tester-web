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

{$mode objfpc}{$H+}{$macro on}

interface

uses
  Classes, SysUtils, htmlpages, tswebeditablefeatures, tswebpagesbase,
  htmlpreprocess, webstrconsts, tswebpages, editableobjects, navbars;

type

  { TEditablePageHook }

  TEditablePageHook = class
  private
    FParent: TTesterHtmlPage;
  public
    property Parent: TTesterHtmlPage read FParent;
    procedure AddFeaturesPre; virtual;
    procedure AddFeaturesPost; virtual;
    function CreateNavBar: TNavBar; virtual;
    function Manager: TEditableManager; virtual;
    constructor Create(AParent: TTesterHtmlPage); virtual;
  end;

  TEditablePageHookClass = class of TEditablePageHook;

  // some hacks with macros for not repeating the code twice for diffent page types
  // interfaces of hookable pages

  {$define HOOKABLE_PAGE_CLASS :=
  { THookableDefaultHtmlPage }

  THookableDefaultHtmlPage }
  {$define HOOKABLE_PAGE_BASE := TDefaultHtmlPage }
  {$I editablehookpages_h.inc }

  {$define HOOKABLE_PAGE_CLASS := THookablePostHtmlPage }
  {$define HOOKABLE_PAGE_BASE := TPostHtmlPage }
  {$I editablehookpages_h.inc }

  {$undef HOOKABLE_PAGE_CLASS}
  {$undef HOOKABLE_PAGE_BASE}

  { TEditableObjListPage }

  TEditableObjListPage = class(THookableDefaultHtmlPage)
  protected
    procedure AddFeatures; override;
    procedure DoGetInnerContents(Strings: TIndentTaggedStrings); override;
  public
    procedure AfterConstruction; override;
  end;

  { TEditableCreateFormPage }

  TEditableCreateFormPage = class(THookablePostHtmlPage)
  protected
    procedure AddFeatures; override;
    procedure DoGetInnerContents(Strings: TIndentTaggedStrings); override;
  public
    procedure AfterConstruction; override;
  end;

implementation

// some hacks with macros for not repeating the code twice for diffent page types
// implementations of hookable pages

{$define HOOKABLE_PAGE_CLASS := THookableDefaultHtmlPage }
{$I editablehookpages.inc }

{$define HOOKABLE_PAGE_CLASS := THookablePostHtmlPage }
{$I editablehookpages.inc }

{$undef HOOKABLE_PAGE_CLASS}

{ TEditablePageHook }

procedure TEditablePageHook.AddFeaturesPre;
begin
  // do nothing
end;

procedure TEditablePageHook.AddFeaturesPost;
begin
  // do nothing
end;

function TEditablePageHook.CreateNavBar: TNavBar;
begin
  // do nothing
  Result := nil;
end;

function TEditablePageHook.Manager: TEditableManager;
begin
  // do nothing
  Result := nil;
end;

constructor TEditablePageHook.Create(AParent: TTesterHtmlPage);
begin
  FParent := AParent;
end;

{ TEditableCreateFormPage }

procedure TEditableCreateFormPage.AddFeatures;
begin
  inherited AddFeatures;
  AddFeature(TEditableCreateFormFeature);
end;

procedure TEditableCreateFormPage.DoGetInnerContents(Strings: TIndentTaggedStrings);
begin
  Strings.Text := '~+#editableCreateForm;';
end;

procedure TEditableCreateFormPage.AfterConstruction;
begin
  inherited AfterConstruction;
  Title := SEditableCreateNew;
end;

{ TEditableObjListPage }

procedure TEditableObjListPage.AddFeatures;
begin
  inherited AddFeatures;
  AddFeature(TEditableObjListFeature);
end;

procedure TEditableObjListPage.DoGetInnerContents(Strings: TIndentTaggedStrings);
begin
  Strings.Text := '~+#editableObjList;';
end;

procedure TEditableObjListPage.AfterConstruction;
begin
  inherited AfterConstruction;
  Title := SEditableList;
end;

end.

