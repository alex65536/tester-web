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
unit tswebproblemmodules;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, tswebmodules, tswebeditablemodules, fphttp, htmlpages, problems,
  editableobjects, tswebpagesbase, tswebproblempages, webstrconsts, HTTPDefs;

type

  { TProblemModuleHook }

  TProblemModuleHook = class(TEditableModuleHook)
  public
    function Manager: TEditableManager; override;
    function ObjectsRoot: string; override;
  end;

  { TProblemObjListWebModule }

  TProblemObjListWebModule = class(TEditableObjListWebModule)
  protected
    function DoCreatePage: THtmlPage; override;
    function HookClass: TEditableModuleHookClass; override;
  end;

  { TProblemCreateNewWebModule }

  TProblemCreateNewWebModule = class(TEditableCreateNewWebModule)
  protected
    function DoCreatePage: THtmlPage; override;
    function HookClass: TEditableModuleHookClass; override;
  end;

  { TProblemDeleteWebModule }

  TProblemDeleteWebModule = class(TEditableDeleteWebModule)
  protected
    function DoCreatePage: THtmlPage; override;
    function HookClass: TEditableModuleHookClass; override;
  end;

  { TProblemAccessWebModule }

  TProblemAccessWebModule = class(TEditableAccessWebModule)
  protected
    function DoCreatePage: THtmlPage; override;
    function HookClass: TEditableModuleHookClass; override;
  end;

  { TProblemViewWebModule }

  TProblemViewWebModule = class(TEditableViewWebModule)
  protected
    function DoCreatePage: THtmlPage; override;
    function HookClass: TEditableModuleHookClass; override;
  end;

  { TProblemEditWebModule }

  TProblemEditWebModule = class(TEditableEditWebModule)
  protected
    procedure DoInsideEdit(ATransaction: TEditableTransaction); override;
    function DoCreatePage: THtmlPage; override;
    function HookClass: TEditableModuleHookClass; override;
  end;

implementation

{ TProblemEditWebModule }

procedure TProblemEditWebModule.DoInsideEdit(ATransaction: TEditableTransaction);
var
  ProblemTransaction: TProblemTransaction;
  I: integer;

  procedure ControlExtension(AFile: TUploadedFile; const MustExt, ErrMsg: string);
  var
    HaveExt: string;
  begin
    HaveExt := LowerCase(ExtractFileExt(AFile.FileName));
    if MustExt <> HaveExt then
      raise EEditableValidate.CreateFmt(ErrMsg, [MustExt]);
  end;

begin
  inherited DoInsideEdit(ATransaction);
  ProblemTransaction := ATransaction as TProblemTransaction;
  with Request.ContentFields do
  begin
    ProblemTransaction.MaxSrcLimit := StrToInt(Values['max-src-limit']);
    ProblemTransaction.StatementsType := StrToStatementsType(Values['statements-type']);
  end;
  with Request.Files do
    for I := 0 to Count - 1 do
    begin
      if Files[I].FieldName = 'archive' then
      begin
        ControlExtension(Files[I], '.zip', SArchiveExtensionExpected);
        ProblemTransaction.ArchiveFileName := Files[I].LocalFileName
      end
      else if Files[I].FieldName = 'statements' then
      begin
        ControlExtension(Files[I], SFileTypesByExt[ProblemTransaction.StatementsType],
          SStatementsExtensionExpected);
        ProblemTransaction.StatementsFileName := Files[I].LocalFileName;
      end;
    end;
end;

function TProblemEditWebModule.DoCreatePage: THtmlPage;
begin
  Result := TProblemEditPage.Create;
end;

function TProblemEditWebModule.HookClass: TEditableModuleHookClass;
begin
  Result := TProblemModuleHook;
end;

{ TProblemViewWebModule }

function TProblemViewWebModule.DoCreatePage: THtmlPage;
begin
  Result := TProblemViewPage.Create;
end;

function TProblemViewWebModule.HookClass: TEditableModuleHookClass;
begin
  Result := TProblemModuleHook;
end;

{ TProblemAccessWebModule }

function TProblemAccessWebModule.DoCreatePage: THtmlPage;
begin
  Result := TProblemAccessPage.Create;
end;

function TProblemAccessWebModule.HookClass: TEditableModuleHookClass;
begin
  Result := TProblemModuleHook;
end;

{ TProblemDeleteWebModule }

function TProblemDeleteWebModule.DoCreatePage: THtmlPage;
begin
  Result := TNavConfirmPasswordPage.Create;
end;

function TProblemDeleteWebModule.HookClass: TEditableModuleHookClass;
begin
  Result := TProblemModuleHook;
end;

{ TProblemCreateNewWebModule }

function TProblemCreateNewWebModule.DoCreatePage: THtmlPage;
begin
  Result := TProblemCreateNewPage.Create;
end;

function TProblemCreateNewWebModule.HookClass: TEditableModuleHookClass;
begin
  Result := TProblemModuleHook;
end;

{ TProblemObjListWebModule }

function TProblemObjListWebModule.DoCreatePage: THtmlPage;
begin
  Result := TProblemListPage.Create;
end;

function TProblemObjListWebModule.HookClass: TEditableModuleHookClass;
begin
  Result := TProblemModuleHook;
end;

{ TProblemModuleHook }

function TProblemModuleHook.Manager: TEditableManager;
begin
  Result := ProblemManager;
end;

function TProblemModuleHook.ObjectsRoot: string;
begin
  Result := DocumentRoot + '/problems';
end;

initialization
  RegisterHTTPModule('problems', TProblemObjListWebModule, True);
  RegisterHTTPModule('problem-new', TProblemCreateNewWebModule, True);
  RegisterHTTPModule('problem-delete', TProblemDeleteWebModule, True);
  RegisterHTTPModule('problem-access', TProblemAccessWebModule, True);
  RegisterHTTPModule('problem-view', TProblemViewWebModule, True);
  RegisterHTTPModule('problem-edit', TProblemEditWebModule, True);

end.

