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
unit tswebproblems;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tswebmodules, tswebeditablefeatures, tswebeditablemodules,
  tswebeditablepages, webstrconsts, fphttp, htmlpages, problems, editableobjects,
  navbars, tswebfeatures, tswebpagesbase;

type

  { TProblemBaseFeature }

  TProblemBaseFeature = class(TTesterPageFeature)
  public
    procedure Satisfy; override;
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

  { TProblemPageHook }

  TProblemPageHook = class(TEditablePageHook)
  private type
    TFakeTesterHtmlPage = class(TTesterHtmlPage)
    public
      procedure AddFeature(AClass: THtmlPageFeatureClass);
    end;
  public
    procedure AddFeaturesPre; override;
    function CreateNavBar: TNavBar; override;
    function Manager: TEditableManager; override;
  end;

  { TProblemListPage }

  TProblemListPage = class(TEditableObjListPage, IEditablePage)
  protected
    function HookClass: TEditablePageHookClass; override;
  public
    procedure AfterConstruction; override;
  end;

  { TProblemCreateNewPage }

  TProblemCreateNewPage = class(TEditableCreateFormPage, IEditablePage)
  protected
    function HookClass: TEditablePageHookClass; override;
  public
    procedure AfterConstruction; override;
  end;

  { TProblemListPageModule }

  TProblemListPageModule = class(TEditableHtmlPageWebModule)
  protected
    function DoCreatePage: THtmlPage; override;
  end;

  { TProblemCreateNewModule }

  TProblemCreateNewModule = class(TEditableCreateNewWebModule)
  protected
    function Manager: TEditableManager; override;
    function DoCreatePage: THtmlPage; override;
    function CanRedirect: boolean; override;
    function RedirectLocation: string; override;
  end;

implementation

{ TProblemPageHook.TFakeTesterHtmlPage }

procedure TProblemPageHook.TFakeTesterHtmlPage.AddFeature(AClass: THtmlPageFeatureClass);
begin
  inherited;
end;

{ TProblemPageHook }

procedure TProblemPageHook.AddFeaturesPre;
begin
  TFakeTesterHtmlPage(Parent).AddFeature(TProblemBaseFeature);
end;

function TProblemPageHook.CreateNavBar: TNavBar;
begin
  Result := TDefaultNavBar.Create(Parent);
end;

function TProblemPageHook.Manager: TEditableManager;
begin
  Result := ProblemManager;
end;

{ TProblemCreateNewModule }

function TProblemCreateNewModule.Manager: TEditableManager;
begin
  Result := ProblemManager;
end;

function TProblemCreateNewModule.DoCreatePage: THtmlPage;
begin
  Result := TProblemCreateNewPage.Create;
end;

function TProblemCreateNewModule.CanRedirect: boolean;
begin
  Result := True;
end;

function TProblemCreateNewModule.RedirectLocation: string;
begin
  Result := DocumentRoot + '/problems';
end;

{ TProblemCreateNewPage }

function TProblemCreateNewPage.HookClass: TEditablePageHookClass;
begin
  Result := TProblemPageHook;
end;

procedure TProblemCreateNewPage.AfterConstruction;
begin
  inherited AfterConstruction;
  Title := SProblemCreateNew;
end;

{ TProblemBaseFeature }

procedure TProblemBaseFeature.Satisfy;
begin
  with Parent.Variables do
  begin
    ItemsAsText['editableNewRef'] := 'problem-new';
    ItemsAsText['editableNodeDeleteRef'] := 'problem-delete';
    ItemsAsText['editableViewRef'] := 'problem-view';
  end;
end;

procedure TProblemBaseFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TEditableBaseFeature);
end;

{ TProblemListPageModule }

function TProblemListPageModule.DoCreatePage: THtmlPage;
begin
  Result := TProblemListPage.Create;
end;

{ TProblemListPage }

function TProblemListPage.HookClass: TEditablePageHookClass;
begin
  Result := TProblemPageHook;
end;

procedure TProblemListPage.AfterConstruction;
begin
  inherited AfterConstruction;
  Title := SProblemList;
end;

initialization
  RegisterHTTPModule('problems', TProblemListPageModule, True);
  RegisterHTTPModule('problem-new', TProblemCreateNewModule, True);

end.

