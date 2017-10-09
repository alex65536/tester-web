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
unit tswebcontestpages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tswebeditablefeatures, webstrconsts, editableobjects,
  tswebcontestfeatures, tswebeditablepages, tswebmanagers;

type

  { TContestHtmlPage }

  TContestHtmlPage = class(TEditableHtmlPage)
  protected
    function DoGetManager: TEditableManager; override;
    procedure AddFeatures; override;
  end;

  { TContestPostHtmlPage }

  TContestPostHtmlPage = class(TEditablePostHtmlPage)
  protected
    function DoGetManager: TEditableManager; override;
    procedure AddFeatures; override;
  end;

  { TContestListPage }

  TContestListPage = class(TContestHtmlPage)
  protected
    procedure AddFeatures; override;
    function HasEditableObject: boolean; override;
  public
    procedure AfterConstruction; override;
  end;

  { TContestCreateNewPage }

  TContestCreateNewPage = class(TContestPostHtmlPage)
  protected
    procedure AddFeatures; override;
    function HasEditableObject: boolean; override;
  public
    procedure AfterConstruction; override;
  end;

  { TContestAccessPage }

  TContestAccessPage = class(TContestPostHtmlPage)
  protected
    procedure AddFeatures; override;
  end;

  { TContestViewPage }

  TContestViewPage = class(TContestHtmlPage)
  protected
    procedure AddFeatures; override;
  end;

  { TContestEditPage }

  TContestEditPage = class(TContestPostHtmlPage)
  protected
    procedure AddFeatures; override;
  end;

  { TContestSettingsPage }

  TContestSettingsPage = class(TContestPostHtmlPage)
  protected
    procedure AddFeatures; override;
  end;

implementation

{ TContestSettingsPage }

procedure TContestSettingsPage.AddFeatures;
begin
  inherited AddFeatures;
  AddFeature(TContestSettingsFeature);
end;

{ TContestEditPage }

procedure TContestEditPage.AddFeatures;
begin
  inherited AddFeatures;
  AddFeature(TContestEditFeature);
end;

{ TContestViewPage }

procedure TContestViewPage.AddFeatures;
begin
  inherited AddFeatures;
  AddFeature(TContestViewFeature);
end;

{ TContestAccessPage }

procedure TContestAccessPage.AddFeatures;
begin
  inherited AddFeatures;
  AddFeature(TContestManageAccessFeature);
end;

{ TContestCreateNewPage }

procedure TContestCreateNewPage.AddFeatures;
begin
  inherited AddFeatures;
  AddFeature(TContestCreateFormFeature);
end;

function TContestCreateNewPage.HasEditableObject: boolean;
begin
  Result := False;
end;

procedure TContestCreateNewPage.AfterConstruction;
begin
  inherited AfterConstruction;
  Title := SContestCreateNew;
end;

{ TContestListPage }

procedure TContestListPage.AddFeatures;
begin
  inherited AddFeatures;
  AddFeature(TEditableObjListFeature);
end;

function TContestListPage.HasEditableObject: boolean;
begin
  Result := False;
end;

procedure TContestListPage.AfterConstruction;
begin
  inherited AfterConstruction;
  Title := SContestList;
end;

{ TContestPostHtmlPage }

function TContestPostHtmlPage.DoGetManager: TEditableManager;
begin
  Result := ContestManager;
end;

procedure TContestPostHtmlPage.AddFeatures;
begin
  AddFeature(TContestBaseFeature);
  inherited AddFeatures;
end;

{ TContestHtmlPage }

function TContestHtmlPage.DoGetManager: TEditableManager;
begin
  Result := ContestManager;
end;

procedure TContestHtmlPage.AddFeatures;
begin
  AddFeature(TContestBaseFeature);
  inherited AddFeatures;
end;

end.

