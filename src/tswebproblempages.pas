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
  SysUtils, tswebeditablefeatures, webstrconsts, editableobjects, tswebmanagers,
  tswebproblemfeatures, tswebeditablepages;

type

  { TProblemHtmlPage }

  TProblemHtmlPage = class(TEditableHtmlPage)
  protected
    function DoGetManager: TEditableManager; override;
    procedure AddFeatures; override;
  end;

  { TProblemPostHtmlPage }

  TProblemPostHtmlPage = class(TEditablePostHtmlPage)
  protected
    function DoGetManager: TEditableManager; override;
    procedure AddFeatures; override;
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

  { TProblemTestPage }

  TProblemTestPage = class(TProblemPostHtmlPage)
  protected
    procedure AddFeatures; override;
  end;

  { TProblemSubmissionsPage }

  TProblemSubmissionsPage = class(TProblemPostHtmlPage)
  protected
    procedure AddFeatures; override;
  end;

  { TProblemSettingsPage }

  TProblemSettingsPage = class(TProblemPostHtmlPage)
  protected
    procedure AddFeatures; override;
  end;

implementation

{ TProblemSettingsPage }

procedure TProblemSettingsPage.AddFeatures;
begin
  inherited AddFeatures;
  AddFeature(TProblemSettingsFeature);
end;

{ TProblemSubmissionsPage }

procedure TProblemSubmissionsPage.AddFeatures;
begin
  inherited AddFeatures;
  AddFeature(TProblemSubmissionsFeature);
end;

{ TProblemTestPage }

procedure TProblemTestPage.AddFeatures;
begin
  inherited AddFeatures;
  AddFeature(TProblemTestFeature);
end;

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
  AddFeature(TProblemManageAccessFeature);
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

function TProblemPostHtmlPage.DoGetManager: TEditableManager;
begin
  Result := ProblemManager;
end;

procedure TProblemPostHtmlPage.AddFeatures;
begin
  AddFeature(TProblemBaseFeature);
  inherited AddFeatures;
end;

{ TProblemHtmlPage }

function TProblemHtmlPage.DoGetManager: TEditableManager;
begin
  Result := ProblemManager;
end;

procedure TProblemHtmlPage.AddFeatures;
begin
  AddFeature(TProblemBaseFeature);
  inherited AddFeatures;
end;

end.

