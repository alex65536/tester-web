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

// This is a sketch of problems unit, actually nothing is implemented.
// This is done to test front-end EditableObjects part
// TODO : Write problems part!!!

{$mode objfpc}{$H+}{$M+}

interface

uses
  Classes, SysUtils, editableobjects, datastorages, webstrconsts, TypInfo,
  tswebdirectories;

type
  TProblemStatementsType = (stHtml, stPdf);

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
  protected
    procedure DoCommit; override;
    procedure DoReload; override;
    {%H-}constructor Create(AManager: TEditableManager; AUser: TEditorUser;
      AObject: TEditableObject);
  public
    property StatementsType: TProblemStatementsType read FStatementsType write
      FStatementsType;
    property StatementsFileName: string read FStatementsFileName write
      FStatementsFileName;
    property ArchiveFileName: string read FArchiveFileName write FArchiveFileName;
    property MaxSrcLimit: integer read FMaxSrcLimit write FMaxSrcLimit;
  end;

  { TProblemManagerSession }

  TProblemManagerSession = class(TEditableManagerSession)
  protected
    {%H-}constructor Create(AManager: TEditableManager; AUser: TEditorUser);
  end;

  { TProblem }

  TProblem = class(TEditableObject)
  protected
    {%H-}constructor Create(const AName: string; AManager: TEditableManager);
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
  Result := GetEnumName(TypeInfo(TEditableAccessRights), Ord(AType));
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

procedure TProblem.WaitForFiles;
begin
  // This will be used later, when the problem testing will be added
end;

procedure TProblem.HandleSelfDeletion;
begin
  inherited HandleSelfDeletion;
  WaitForFiles;
  // TODO : Write it!!!
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

procedure TProblemTransaction.DoCommit;
begin
  inherited DoCommit;
  // TODO : implement DoCommit!
end;

procedure TProblemTransaction.DoReload;
begin
  inherited DoReload;
  // TODO : implement DoReload!
end;

constructor TProblemTransaction.Create(AManager: TEditableManager;
  AUser: TEditorUser; AObject: TEditableObject);
begin
  inherited Create(AManager, AUser, AObject);
end;

finalization
  FreeAndNil(FManager);

end.
