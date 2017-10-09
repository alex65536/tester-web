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
unit tswebcontestmodules;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tswebpagesbase, tswebmodules, tswebeditablemodules,
  fphttp, tswebcontestpages, tswebmanagers, editableobjects, htmlpages;

type

  { TContestModuleHook }

  TContestModuleHook = class(TEditableModuleHook)
  public
    function Manager: TEditableManager; override;
    function ObjectsRoot: string; override;
  end;

  { TContestObjListWebModule }

  TContestObjListWebModule = class(TEditableObjListWebModule)
  protected
    function DoCreatePage: THtmlPage; override;
    function HookClass: TEditableModuleHookClass; override;
  end;

  { TContestCreateNewWebModule }

  TContestCreateNewWebModule = class(TEditableCreateNewWebModule)
  protected
    function DoCreatePage: THtmlPage; override;
    function HookClass: TEditableModuleHookClass; override;
  end;

  { TContestDeleteWebModule }

  TContestDeleteWebModule = class(TEditableDeleteWebModule)
  protected
    function DoCreatePage: THtmlPage; override;
    function HookClass: TEditableModuleHookClass; override;
  end;

  { TContestAccessWebModule }

  TContestAccessWebModule = class(TEditableAccessWebModule)
  protected
    function DoCreatePage: THtmlPage; override;
    function HookClass: TEditableModuleHookClass; override;
  end;

  { TContestViewWebModule }

  TContestViewWebModule = class(TEditableViewWebModule)
  protected
    function DoCreatePage: THtmlPage; override;
    function HookClass: TEditableModuleHookClass; override;
  end;

  { TContestEditWebModule }

  TContestEditWebModule = class(TEditableEditWebModule)
  protected
    procedure DoInsideEdit(ATransaction: TEditableTransaction); override;
    function DoCreatePage: THtmlPage; override;
    function HookClass: TEditableModuleHookClass; override;
  end;

  { TContestSettingsWebModule }

  TContestSettingsWebModule = class(TEditableSettingsWebModule)
  protected
    function DoCreatePage: THtmlPage; override;
    function HookClass: TEditableModuleHookClass; override;
  end;

implementation

{ TContestSettingsWebModule }

function TContestSettingsWebModule.DoCreatePage: THtmlPage;
begin
  Result := TContestSettingsPage.Create;
end;

function TContestSettingsWebModule.HookClass: TEditableModuleHookClass;
begin
  Result := TContestModuleHook;
end;

{ TContestEditWebModule }

procedure TContestEditWebModule.DoInsideEdit(ATransaction: TEditableTransaction);
begin
  inherited DoInsideEdit(ATransaction);
  // TODO : Implement DoInsideEdit !!!
end;

function TContestEditWebModule.DoCreatePage: THtmlPage;
begin
  Result := TContestEditPage.Create;
end;

function TContestEditWebModule.HookClass: TEditableModuleHookClass;
begin
  Result := TContestModuleHook;
end;

{ TContestViewWebModule }

function TContestViewWebModule.DoCreatePage: THtmlPage;
begin
  Result := TContestViewPage.Create;
end;

function TContestViewWebModule.HookClass: TEditableModuleHookClass;
begin
  Result := TContestModuleHook;
end;

{ TContestAccessWebModule }

function TContestAccessWebModule.DoCreatePage: THtmlPage;
begin
  Result := TContestAccessPage.Create;
end;

function TContestAccessWebModule.HookClass: TEditableModuleHookClass;
begin
  Result := TContestModuleHook;
end;

{ TContestDeleteWebModule }

function TContestDeleteWebModule.DoCreatePage: THtmlPage;
begin
  Result := TNavConfirmPasswordPage.Create;
end;

function TContestDeleteWebModule.HookClass: TEditableModuleHookClass;
begin
  Result := TContestModuleHook;
end;

{ TContestCreateNewWebModule }

function TContestCreateNewWebModule.DoCreatePage: THtmlPage;
begin
  Result := TContestCreateNewPage.Create;
end;

function TContestCreateNewWebModule.HookClass: TEditableModuleHookClass;
begin
  Result := TContestModuleHook;
end;

{ TContestObjListWebModule }

function TContestObjListWebModule.DoCreatePage: THtmlPage;
begin
  Result := TContestListPage.Create;
end;

function TContestObjListWebModule.HookClass: TEditableModuleHookClass;
begin
  Result := TContestModuleHook;
end;

{ TContestModuleHook }

function TContestModuleHook.Manager: TEditableManager;
begin
  Result := ContestManager;
end;

function TContestModuleHook.ObjectsRoot: string;
begin
  Result := DocumentRoot + '/contests';
end;

initialization
  RegisterHTTPModule('contests', TContestObjListWebModule, True);
  RegisterHTTPModule('contest-new', TContestCreateNewWebModule, True);
  RegisterHTTPModule('contest-delete', TContestDeleteWebModule, True);
  RegisterHTTPModule('contest-access', TContestAccessWebModule, True);
  RegisterHTTPModule('contest-view', TContestViewWebModule, True);
  RegisterHTTPModule('contest-edit', TContestEditWebModule, True);
  RegisterHTTPModule('contest-settings', TContestSettingsWebModule, True);

end.

