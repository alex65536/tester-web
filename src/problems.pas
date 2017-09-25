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
unit problems;

{$mode objfpc}{$H+}{$M+}

interface

uses
  Classes, SysUtils, editableobjects, datastorages, webstrconsts, TypInfo,
  tswebdirectories, filemanager, archivemanager, FileUtil, LazFileUtils,
  serverconfig;

type
  TProblemStatementsType = (stNone, stHtml, stPdf);

const
  SFileTypesByExt: array [TProblemStatementsType] of string = (
    '',
    '.html',
    '.pdf'
    );

  SFileTypesByName: array [TProblemStatementsType] of string = (
    '',
    SProblemStatementsHtml,
    SProblemStatementsPdf
    );

  SFileTypesByMime: array [TProblemStatementsType] of string = (
    '',
    'text/html',
    'application/pdf'
    );

  SArchiveMime = 'application/zip';
  SArchiveExt = '.zip';

type
  TProblem = class;

  { TProblemAccessSession }

  TProblemAccessSession = class(TEditableObjectAccessSession)
  protected
    {%H-}constructor Create(AManager: TEditableManager; AUser: TEditorUser;
      AObject: TEditableObject);
  end;

  { TProblemTransaction }

  TProblemTransaction = class(TEditableTransaction)
  private
    FArchiveFileName: string;
    FMaxSrcLimit: integer;
    FStatementsFileName: string;
    FStatementsType: TProblemStatementsType;
    function GetProblem: TProblem;
  protected
    procedure DoCommit; override;
    procedure DoReload; override;
    {%H-}constructor Create(AManager: TEditableManager; AUser: TEditorUser;
      AObject: TEditableObject);
  public
    property Problem: TProblem read GetProblem;
    property StatementsType: TProblemStatementsType read FStatementsType write
      FStatementsType;
    property StatementsFileName: string read FStatementsFileName write
      FStatementsFileName;
    property ArchiveFileName: string read FArchiveFileName write FArchiveFileName;
    property MaxSrcLimit: integer read FMaxSrcLimit write FMaxSrcLimit;
    procedure Validate; override;
  end;

  { TProblemManagerSession }

  TProblemManagerSession = class(TEditableManagerSession)
  protected
    {%H-}constructor Create(AManager: TEditableManager; AUser: TEditorUser);
  end;

  { TProblem }

  TProblem = class(TEditableObject)
  private
    function GetFileName(const Dir, Ext: string; MustExist: boolean): string;
  protected
    {%H-}constructor Create(const AName: string; AManager: TEditableManager);
    function ArchiveFileName(MustExist: boolean): string;
    function UnpackedFileName(MustExist: boolean): string;
    function StatementsFileName(MustExist: boolean): string;
    function StatementsFileType: TProblemStatementsType;
    procedure WaitForFiles;
    procedure HandleSelfDeletion; override;
  public
    function CreateAccessSession(AUser: TEditorUser): TEditableObjectAccessSession;
      override;
    function CreateTransaction(AUser: TEditorUser): TEditableTransaction;
      override;
  end;

  TProblemManager = class(TEditableManager)
  protected
    function ObjectTypeName: string; override;
    function CreateStorage: TAbstractDataStorage; override;
    function CreateObject(const AName: string): TEditableObject; override;
  public
    function CreateManagerSession(AUser: TEditorUser): TEditableManagerSession;
      override;
  end;

function ProblemManager: TProblemManager;

function StatementsTypeToStr(AType: TProblemStatementsType): string;
function StrToStatementsType(const S: string): TProblemStatementsType;

implementation

var
  FManager: TProblemManager = nil;

function ProblemManager: TProblemManager;
begin
  if FManager = nil then
    FManager := TProblemManager.Create;
  Result := FManager;
end;

function StatementsTypeToStr(AType: TProblemStatementsType): string;
begin
  Result := GetEnumName(TypeInfo(TProblemStatementsType), Ord(AType));
end;

function StrToStatementsType(const S: string): TProblemStatementsType;
var
  T: TProblemStatementsType;
begin
  for T in TProblemStatementsType do
    if StatementsTypeToStr(T) = S then
      Exit(T);
  raise EConvertError.Create(SNoSuchStatementsType);
end;

{ TProblemManager }

function TProblemManager.ObjectTypeName: string;
begin
  Result := SProblemTypeName;
end;

function TProblemManager.CreateStorage: TAbstractDataStorage;
begin
  Result := TXmlDataStorage.Create('problems');
end;

function TProblemManager.CreateObject(const AName: string): TEditableObject;
begin
  Result := TProblem.Create(AName, Self);
end;

function TProblemManager.CreateManagerSession(AUser: TEditorUser): TEditableManagerSession;
begin
  Result := TProblemManagerSession.Create(Self, AUser);
end;

{ TProblem }

constructor TProblem.Create(const AName: string; AManager: TEditableManager);
begin
  inherited Create(AName, AManager);
end;

function TProblem.GetFileName(const Dir, Ext: string; MustExist: boolean): string;
var
  Path: string;
begin
  Path := AppendPathDelim(ExpandInternalDirLocation(Dir));
  Result := Path + Format('problem%d%s', [ID, Ext]);
  if MustExist and (not FileExistsUTF8(Result)) then
    Result := '';
end;

function TProblem.ArchiveFileName(MustExist: boolean): string;
begin
  Result := GetFileName('archives', SArchiveExt, MustExist);
end;

function TProblem.UnpackedFileName(MustExist: boolean): string;
begin
  Result := GetFileName('problems', '', MustExist);
end;

function TProblem.StatementsFileName(MustExist: boolean): string;
var
  FileType: TProblemStatementsType;
begin
  FileType := StatementsFileType;
  if FileType = stNone then
    Result := ''
  else
    Result := GetFileName('statements', SFileTypesByExt[FileType], MustExist);
end;

function TProblem.StatementsFileType: TProblemStatementsType;
var
  DefaultValue: string;
begin
  DefaultValue := StatementsTypeToStr(stNone);
  Result := StrToStatementsType(Storage.ReadString(FullKeyName('statementType'),
    DefaultValue));
end;

procedure TProblem.WaitForFiles;
begin
  // This will be used later, when the problem testing will be added
end;

procedure TProblem.HandleSelfDeletion;
var
  Success: boolean;
begin
  inherited HandleSelfDeletion;
  WaitForFiles;
  Success := True;
  Success := Success and TryDeleteFile(StatementsFileName(True));
  Success := Success and TryDeleteFile(ArchiveFileName(True));
  Success := Success and TryDeleteDir(UnpackedFileName(True));
  if not Success then
    raise EEditableAction.CreateFmt(SErrorsWhileDeleting, [Name]);
end;

function TProblem.CreateAccessSession(AUser: TEditorUser): TEditableObjectAccessSession;
begin
  Result := TProblemAccessSession.Create(Manager, AUser, Self);
end;

function TProblem.CreateTransaction(AUser: TEditorUser): TEditableTransaction;
begin
  Result := TProblemTransaction.Create(Manager, AUser, Self);
end;

{ TProblemManagerSession }

constructor TProblemManagerSession.Create(AManager: TEditableManager;
  AUser: TEditorUser);
begin
  inherited Create(AManager, AUser);
end;

{ TProblemAccessSession }

constructor TProblemAccessSession.Create(AManager: TEditableManager;
  AUser: TEditorUser; AObject: TEditableObject);
begin
  inherited Create(AManager, AUser, AObject);
end;

{ TProblemTransaction }

function TProblemTransaction.GetProblem: TProblem;
begin
  Result := EditableObject as TProblem;
end;

procedure TProblemTransaction.DoCommit;
begin
  inherited DoCommit;
  try
    // archive
    if FArchiveFileName <> Problem.ArchiveFileName(True) then
    begin
      TryDeleteFile(Problem.ArchiveFileName(True), True);
      UnpackArchive(FArchiveFileName, Problem.UnpackedFileName(False), True);
      MoveReplaceFile(FArchiveFileName, Problem.ArchiveFileName(False));
    end;
    // statements
    if FStatementsFileName <> Problem.StatementsFileName(True) then
    begin
      TryDeleteFile(Problem.StatementsFileName(True), True);
      Storage.WriteString(FullKeyName('statementType'), StatementsTypeToStr(FStatementsType));
      MoveReplaceFile(FStatementsFileName, Problem.StatementsFileName(False));
    end;
    // max submission limit
    Storage.WriteInteger(FullKeyName('maxSrc'), FMaxSrcLimit);
  except
    on E: EFileManager do
      raise EEditableAction.Create(E.Message)
    else
      raise;
  end;
end;

procedure TProblemTransaction.DoReload;
begin
  inherited DoReload;
  FArchiveFileName := Problem.ArchiveFileName(True);
  FStatementsFileName := Problem.StatementsFileName(True);
  FStatementsType := Problem.StatementsFileType;
  FMaxSrcLimit := Storage.ReadInteger(FullKeyName('maxSrc'), Config.Files_DefaultSrcSize);
end;

constructor TProblemTransaction.Create(AManager: TEditableManager;
  AUser: TEditorUser; AObject: TEditableObject);
begin
  inherited Create(AManager, AUser, AObject);
end;

procedure TProblemTransaction.Validate;
begin
  inherited Validate;
  try
    // archive
    if FArchiveFileName <> Problem.ArchiveFileName(True) then
      ValidateArchive(FArchiveFileName);
    // statements file
    if FStatementsFileName <> Problem.StatementsFileName(True) then
      ValidateFileSize(FStatementsFileName, Config.Files_MaxStatementsSize,
        SStatementsTooBig);
    // max submisson limit
    if (FMaxSrcLimit < 1) or (FMaxSrcLimit > Config.Files_MaxSrcSize) then
      raise EEditableValidate.CreateFmt(SMaxSrcSize, [1, Config.Files_MaxSrcSize]);
  except
    on E: EFileManager do
      raise EEditableValidate.Create(E.Message)
    else
      raise;
  end;
end;

finalization
  FreeAndNil(FManager);

end.
