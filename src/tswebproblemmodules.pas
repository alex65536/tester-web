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
  downloadhandlers, tsmiscwebmodules, tswebmanagers, submissionlanguages,
  submissions;

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
    procedure DoHandleTransaction(ATransaction: TBaseProblemTransaction);
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

  { TProblemTestPostHandler }

  TProblemTestPostHandler = class
  private
    FID: integer;
    FModule: TPostUserWebModule;
    FProblem: TTestableProblem;
  public
    property Problem: TTestableProblem read FProblem;
    property Module: TPostUserWebModule read FModule;
    function CanRedirect: boolean; virtual;
    function RedirectLocation: string; virtual;
    procedure HandlePost(ARequest: TRequest); virtual;
    constructor Create(AProblem: TTestableProblem; AModule: TPostUserWebModule);
    destructor Destroy; override;
  end;

  { TProblemTestWebModule }

  TProblemTestWebModule = class(TEditablePostWebModule)
  private
    FHandler: TProblemTestPostHandler;
  protected
    function Inside: boolean; override;
    function HookClass: TEditableModuleHookClass; override;
    function DoCreatePage: THtmlPage; override;
    procedure DoHandlePost(ARequest: TRequest); override;
    function CanRedirect: boolean; override;
    function RedirectLocation: string; override;
    procedure DoBeforeRequest; override;
    procedure DoAfterRequest; override;
  end;

  { TProblemSubmissionsWebModule }

  TProblemSubmissionsWebModule = class(TEditableObjectPostWebModule)
  protected
    function NeedAccessRights: TEditableAccessRightsSet; override;
    function Inside: boolean; override;
    function HookClass: TEditableModuleHookClass; override;
    function DoCreatePage: THtmlPage; override;
    function CanRedirect: boolean; override;
    function RedirectLocation: string; override;
    procedure DoHandlePost(ARequest: TRequest); override;
  end;

  { TProblemSettingsWebModule }

  TProblemSettingsWebModule = class(TEditableSettingsWebModule)
  protected
    function DoCreatePage: THtmlPage; override;
    function HookClass: TEditableModuleHookClass; override;
  end;

implementation

{ TProblemSettingsWebModule }

function TProblemSettingsWebModule.DoCreatePage: THtmlPage;
begin
  Result := TProblemSettingsPage.Create;
end;

function TProblemSettingsWebModule.HookClass: TEditableModuleHookClass;
begin
  Result := TProblemModuleHook;
end;

{ TProblemSubmissionsWebModule }

function TProblemSubmissionsWebModule.NeedAccessRights: TEditableAccessRightsSet;
begin
  Result := AccessCanWriteSet;
end;

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

procedure TProblemSubmissionsWebModule.DoHandlePost(ARequest: TRequest);
var
  TestProblem: TTestableProblem;
  SubmissionSession: TProblemSubmissionSession;
begin
  if ARequest.ContentFields.Values['query'] = 'rejudge' then
  begin
    TestProblem := EditableObject as TTestableProblem;
    SubmissionSession := ProblemManager.CreateSubmissionSession(User);
    try
      SubmissionSession.RejudgeSubmissions(TestProblem);
    finally
      FreeAndNil(SubmissionSession);
    end;
  end;
end;

{ TProblemTestWebModule }

function TProblemTestWebModule.Inside: boolean;
begin
  Result := True;
end;

function TProblemTestWebModule.HookClass: TEditableModuleHookClass;
begin
  Result := TProblemModuleHook;
end;

function TProblemTestWebModule.DoCreatePage: THtmlPage;
begin
  Result := TProblemTestPage.Create;
end;

procedure TProblemTestWebModule.DoHandlePost(ARequest: TRequest);
begin
  FHandler.HandlePost(ARequest);
end;

function TProblemTestWebModule.CanRedirect: boolean;
begin
  Result := FHandler.CanRedirect;
end;

function TProblemTestWebModule.RedirectLocation: string;
begin
  Result := FHandler.RedirectLocation;
end;

procedure TProblemTestWebModule.DoBeforeRequest;
begin
  inherited DoBeforeRequest;
  FHandler := TProblemTestPostHandler.Create(Hook.EditableObject as TTestableProblem, Self);
end;

procedure TProblemTestWebModule.DoAfterRequest;
begin
  FreeAndNil(FHandler);
  inherited DoAfterRequest;
end;

{ TProblemTestPostHandler }

procedure TProblemTestPostHandler.HandlePost(ARequest: TRequest);
var
  Language: TSubmissionLanguage;
  FileName: string;
  I: integer;
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
  TestTransaction := FProblem.CreateTestTransaction(Module.User);
  try
    FID := TestTransaction.CreateSubmission(Language, FileName);
  finally
    FreeAndNil(TestTransaction);
  end;
end;

function TProblemTestPostHandler.CanRedirect: boolean;
begin
  Result := FID >= 0;
end;

function TProblemTestPostHandler.RedirectLocation: string;
begin
  Result := Format('%s/submissions?id=%d', [DocumentRoot, FID]);
end;

constructor TProblemTestPostHandler.Create(AProblem: TTestableProblem;
  AModule: TPostUserWebModule);
begin
  inherited Create;
  FProblem := AProblem;
  FModule := AModule;
end;

destructor TProblemTestPostHandler.Destroy;
begin
  FreeAndNil(FProblem);
  inherited Destroy;
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

procedure TProblemDownloadHandler.DoHandleTransaction(ATransaction: TBaseProblemTransaction);

  procedure HandleStatements;
  begin
    with ATransaction do
      if FileExt = SFileTypesByExt[StatementsType] then
      begin
        DiskFileName := StatementsFileName;
        FileMimeType := SFileTypesByMime[StatementsType];
      end;
  end;

  procedure HandleArchives;
  begin
    if not (ATransaction is TProblemTransaction) then
      raise EEditableAccessDenied.Create(SAccessDenied);
    with ATransaction as TProblemTransaction do
      if FileExt = SArchiveExt then
      begin
        DiskFileName := ArchiveFileName;
        FileMimeType := SArchiveMime;
      end;
  end;

begin
  if FilePath = 'statements' then
    HandleStatements
  else if FilePath = 'archives' then
    HandleArchives;
end;

procedure TProblemDownloadHandler.InternalHandleDownload;
var
  ProblemName: string;
  Problem: TProblem;
  User: TEditorUser;
  Transaction: TProblemTransaction;
begin
  ProblemName := ChangeFileExt(FileName, '');
  Problem := ProblemManager.GetObject(ProblemName) as TProblem;
  try
    User := (Parent as TUserWebModule).User as TEditorUser;
    Transaction := Problem.CreateTransaction(User) as TProblemTransaction;
    try
      DoHandleTransaction(Transaction);
    finally
      FreeAndNil(Transaction);
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
  RegisterHTTPModule('problem-settings', TProblemSettingsWebModule, True);

end.

