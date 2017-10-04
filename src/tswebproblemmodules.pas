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
  editableobjects, tswebpagesbase, tswebproblempages, webstrconsts, HTTPDefs,
  webmodules, downloadhandlers, tsmiscwebmodules, tswebmanagers, submissions,
  submissionlanguages;

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

  { TProblemDownloadHandler }

  TProblemDownloadHandler = class(TDownloadModuleHandler)
  protected
    procedure InternalHandleDownload; override;
  end;

  { TProblemDownloadWebModule }

  TProblemDownloadWebModule = class(TEditablePageWebModule)
  protected
    function Inside: boolean; override;
    function DoCreatePage: THtmlPage; override;
    function HookClass: TEditableModuleHookClass; override;
  public
    procedure AfterConstruction; override;
  end;

  { TProblemTestBaseWebModule }

  TProblemTestBaseWebModule = class(TEditablePostWebModule)
  protected
    function GetTestableProblem: TTestableProblem; virtual; abstract;
    function Inside: boolean; override;
    function HookClass: TEditableModuleHookClass; override;
    procedure DoHandlePost(ARequest: TRequest); override;
    function CanRedirect: boolean; override;
    function RedirectLocation: string; override;
  end;

  { TProblemTestWebModule }

  TProblemTestWebModule = class(TProblemTestBaseWebModule)
  protected
    function GetTestableProblem: TTestableProblem; override;
    function DoCreatePage: THtmlPage; override;
  end;

  { TProblemSubmissionsWebModule }

  TProblemSubmissionsWebModule = class(TEditableObjectPostWebModule)
  protected
    function Inside: boolean; override;
    function HookClass: TEditableModuleHookClass; override;
    function DoCreatePage: THtmlPage; override;
    function CanRedirect: boolean; override;
    function RedirectLocation: string; override;
    procedure DoInsideHandlePost(ARequest: TRequest); override;
  end;

implementation

{ TProblemSubmissionsWebModule }

function TProblemSubmissionsWebModule.Inside: boolean;
begin
  Result := True;
end;

function TProblemSubmissionsWebModule.HookClass: TEditableModuleHookClass;
begin
  Result := TProblemModuleHook;
end;

function TProblemSubmissionsWebModule.DoCreatePage: THtmlPage;
begin
  Result := TProblemSubmissionsPage.Create;
end;

function TProblemSubmissionsWebModule.CanRedirect: boolean;
begin
  Result := True;
end;

function TProblemSubmissionsWebModule.RedirectLocation: string;
begin
  Result := Request.URI;
end;

procedure TProblemSubmissionsWebModule.DoInsideHandlePost(ARequest: TRequest);
var
  TestProblem: TTestableProblem;
  TestTransaction: TTestProblemTransaction;
begin
  if ARequest.ContentFields.Values['query'] = 'rejudge' then
  begin
    TestProblem := EditableObject as TTestableProblem;
    TestTransaction := TestProblem.CreateTestTransaction(User);
    try
      SubmissionManager.RejudgeSubmissions(TestTransaction, TestProblem);
    finally
      FreeAndNil(TestTransaction);
    end;
  end;
end;

{ TProblemTestWebModule }

function TProblemTestWebModule.GetTestableProblem: TTestableProblem;
begin
  Result := Hook.EditableObject as TTestableProblem;
end;

function TProblemTestWebModule.DoCreatePage: THtmlPage;
begin
  Result := TProblemTestPage.Create;
end;

{ TProblemTestBaseWebModule }

function TProblemTestBaseWebModule.Inside: boolean;
begin
  Result := True;
end;

function TProblemTestBaseWebModule.HookClass: TEditableModuleHookClass;
begin
  Result := TProblemModuleHook;
end;

procedure TProblemTestBaseWebModule.DoHandlePost(ARequest: TRequest);
var
  Language: TSubmissionLanguage;
  FileName: string;
  I: integer;
  TestProblem: TTestableProblem;
  TestTransaction: TTestProblemTransaction;
begin
  Language := StrToLanguage(ARequest.ContentFields.Values['sol-lang']);
  FileName := '';
  // find the solution file
  with ARequest.Files do
    for I := 0 to Count - 1 do
      if Files[I].FieldName = 'sol-file' then
      begin
        if LowerCase(ExtractFileExt(Files[I].FileName)) <> LanguageExts[Language] then
          raise ESubmissionValidate.CreateFmt(SSubmissionExtensionExpected, [LanguageExts[Language]]);
        FileName := Files[I].LocalFileName;
      end;
  // if solution file was not found - raise an error
  if FileName = '' then
    raise ESubmissionValidate.Create(SNoSubmissionFile);
  // create and run the submission
  TestProblem := GetTestableProblem;
  try
    TestTransaction := TestProblem.CreateTestTransaction(User);
    try
      SubmissionManager.CreateSubmission(TestTransaction, Language, FileName);
    finally
      FreeAndNil(TestTransaction);
    end;
  finally
    FreeAndNil(TestProblem);
  end;
end;

function TProblemTestBaseWebModule.CanRedirect: boolean;
begin
  Result := True;
end;

function TProblemTestBaseWebModule.RedirectLocation: string;
begin
  Result := Request.URI;
end;

{ TProblemDownloadWebModule }

function TProblemDownloadWebModule.Inside: boolean;
begin
  Result := False;
end;

function TProblemDownloadWebModule.DoCreatePage: THtmlPage;
begin
  Result := nil; // no need for page in a download module!
end;

function TProblemDownloadWebModule.HookClass: TEditableModuleHookClass;
begin
  Result := TProblemModuleHook;
end;

procedure TProblemDownloadWebModule.AfterConstruction;
begin
  inherited AfterConstruction;
  Handlers.Add(TProblemDownloadHandler.Create);
end;

{ TProblemDownloadHandler }

procedure TProblemDownloadHandler.InternalHandleDownload;
var
  ProblemName: string;
  Problem: TProblem;
  User: TEditorUser;
begin
  ProblemName := ChangeFileExt(FileName, '');
  Problem := ProblemManager.GetObject(ProblemName) as TProblem;
  try
    User := (Parent as TUserWebModule).User as TEditorUser;
    with Problem.CreateTransaction(User) as TProblemTransaction do
      try
        if (FilePath = 'statements') and (FileExt = SFileTypesByExt[StatementsType]) then
        begin
          DiskFileName := StatementsFileName;
          FileMimeType := SFileTypesByMime[StatementsType];
        end
        else if (FilePath = 'archives') and (FileExt = SArchiveExt) then
        begin
          DiskFileName := ArchiveFileName;
          FileMimeType := SArchiveMime;
        end;
      finally
        Free;
      end;
  finally
    FreeAndNil(Problem);
  end;
end;

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
  RegisterHTTPModule('problem-download', TProblemDownloadWebModule, True);
  RegisterHTTPModule('problem-test', TProblemTestWebModule, True);
  RegisterHTTPModule('problem-submissions', TProblemSubmissionsWebModule, True);

end.

