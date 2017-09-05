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
unit datastorages;

{$mode objfpc}{$H+}{$B-}

interface

uses
  Classes, SysUtils, IniFiles, LazFileUtils, Laz2_DOM, Laz2_XMLRead,
  Laz2_XMLWrite, AvgLvlTree;

type

  { TAbstractDataStorage }

  TAbstractDataStorage = class(TPersistent)
  private
    FStoragePath: string;
  public
    property StoragePath: string read FStoragePath;
    function GetRootElements: TStringList; virtual; abstract;
    function VariableExists(const Path: string): boolean; virtual; abstract;
    procedure DeleteVariable(const Path: string); virtual;
    procedure DeletePath(const Path: string); virtual;
    function ReadInteger(const Path: string; Default: integer): integer;
      virtual; abstract;
    function ReadInt64(const Path: string; Default: int64): int64; virtual; abstract;
    function ReadString(const Path: string; const Default: string): string;
      virtual; abstract;
    function ReadBool(const Path: string; Default: boolean): boolean; virtual; abstract;
    function ReadFloat(const Path: string; Default: double): double; virtual; abstract;
    procedure WriteInteger(const Path: string; Value: integer); virtual;
    procedure WriteInt64(const Path: string; Value: int64); virtual;
    procedure WriteString(const Path: string; const Value: string); virtual;
    procedure WriteBool(const Path: string; Value: boolean); virtual;
    procedure WriteFloat(const Path: string; Value: double); virtual;
    procedure Commit; virtual; abstract;
    procedure Reload; virtual;
    constructor Create(const AStoragePath: string);
  end;

  { IStorageUser }

  IStorageUser = interface
  ['{D891B260-67E5-46F0-A3E7-D190F2410B4A}']
    procedure ReloadFromStorage(AStorage: TAbstractDataStorage);
    procedure CommitToStorage(AStorage: TAbstractDataStorage);
  end;

  { TFileDataStorage }

  TFileDataStorage = class(TAbstractDataStorage)
  protected
    function GetFileName: string; virtual;
  end;

  { TIniDataStorage }

  TIniDataStorage = class(TFileDataStorage)
  private const
    GlobalSectionName = '$GLOBAL$';
  private
    FIniFile: TIniFile;
    FStream: TMemoryStream;
    procedure SplitPath(const Path: string; out Section, Ident: string);
    procedure CreateIniFile;
    procedure LoadStream;
    procedure RemoveSectionIfEmpty(const Section: string);
    procedure RemoveEmptySections;
  protected
    function GetFileName: string; override;
  public
    function GetRootElements: TStringList; override;
    function VariableExists(const Path: string): boolean; override;
    procedure DeleteVariable(const Path: string); override;
    procedure DeletePath(const Path: string); override;
    function ReadInteger(const Path: string; Default: integer): integer; override;
    function ReadInt64(const Path: string; Default: int64): int64; override;
    function ReadString(const Path: string; const Default: string): string; override;
    function ReadBool(const Path: string; Default: boolean): boolean; override;
    function ReadFloat(const Path: string; Default: double): double; override;
    procedure WriteInteger(const Path: string; Value: integer); override;
    procedure WriteInt64(const Path: string; Value: int64); override;
    procedure WriteString(const Path: string; const Value: string); override;
    procedure WriteBool(const Path: string; Value: boolean); override;
    procedure WriteFloat(const Path: string; Value: double); override;
    procedure Commit; override;
    procedure Reload; override;
    constructor Create(const AStoragePath: string);
    destructor Destroy; override;
  end;

  { TXmlDataStorage }

  TXmlDataStorage = class(TFileDataStorage)
  protected const
    ValueAttr = 'value';
    RootNodeName = 'data';
  private
    FDocument: TXMLDocument;
    function FindNode(const Path: string): TDOMElement;
    procedure CreateDocument;
    function RootElement: TDOMElement;
    procedure LoadDocument;
  protected
    function GetFileName: string; override;
  public
    function GetRootElements: TStringList; override;
    function VariableExists(const Path: string): boolean; override;
    procedure DeleteVariable(const Path: string); override;
    procedure DeletePath(const Path: string); override;
    function ReadInteger(const Path: string; Default: integer): integer; override;
    function ReadInt64(const Path: string; Default: int64): int64; override;
    function ReadString(const Path: string; const Default: string): string; override;
    function ReadBool(const Path: string; Default: boolean): boolean; override;
    function ReadFloat(const Path: string; Default: double): double; override;
    procedure WriteInteger(const Path: string; Value: integer); override;
    procedure WriteInt64(const Path: string; Value: int64); override;
    procedure WriteString(const Path: string; const Value: string); override;
    procedure WriteBool(const Path: string; Value: boolean); override;
    procedure WriteFloat(const Path: string; Value: double); override;
    procedure Commit; override;
    procedure Reload; override;
    constructor Create(const AStoragePath: string);
    destructor Destroy; override;
  end;

implementation

{ TFileDataStorage }

function TFileDataStorage.GetFileName: string;
begin
  Result := AppendPathDelim(GetAppConfigDirUTF8(False, True)) + StoragePath + '.conf';
end;

{ TXmlDataStorage }

function TXmlDataStorage.GetFileName: string;
begin
  Result := ChangeFileExt(inherited GetFileName, '.xml');
end;

function TXmlDataStorage.GetRootElements: TStringList;
var
  Elem: TDOMNode;
begin
  Result := TStringList.Create;
  try
    Elem := RootElement.FirstChild;
    while Elem <> nil do
    begin
      Result.Add(Elem.NodeName);
      Elem := Elem.NextSibling;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TXmlDataStorage.FindNode(const Path: string): TDOMElement;
var
  Elem: TDOMNode;
  S: string;
begin
  Result := nil;
  Elem := RootElement;
  for S in Path.Split(['.']) do
  begin
    Elem := Elem.FindNode(S);
    if Elem = nil then
      Exit;
  end;
  if not (Elem is TDOMElement) then
    Exit;
  Result := Elem as TDOMElement;
end;

procedure TXmlDataStorage.CreateDocument;
begin
  FDocument := TXMLDocument.Create;
  FDocument.AppendChild(FDocument.CreateElement(RootNodeName));
end;

function TXmlDataStorage.RootElement: TDOMElement;
begin
  Result := FDocument.FindNode(RootNodeName) as TDOMElement;
end;

procedure TXmlDataStorage.LoadDocument;
var
  FileName: string;
begin
  FileName := GetFileName;
  if FileExistsUTF8(FileName) then
    ReadXMLFile(FDocument, FileName)
  else
    CreateDocument;
end;

function TXmlDataStorage.VariableExists(const Path: string): boolean;
var
  Elem: TDOMElement;
begin
  Elem := FindNode(Path);
  Result := (Elem <> nil) and Elem.HasAttribute(ValueAttr);
end;

procedure TXmlDataStorage.DeleteVariable(const Path: string);
var
  Elem: TDOMElement;
begin
  Elem := FindNode(Path);
  if Elem = nil then
    Exit;
  if Elem.HasChildNodes then
  begin
    if Elem.HasAttribute(ValueAttr) then
      Elem.RemoveAttribute(ValueAttr);
  end
  else
  begin
    Elem := Elem.ParentNode.RemoveChild(Elem) as TDOMElement;
    FreeAndNil(Elem);
  end;
  inherited DeleteVariable(Path);
end;

procedure TXmlDataStorage.DeletePath(const Path: string);
var
  Elem: TDOMElement;
begin
  Elem := FindNode(Path);
  if Elem = nil then
    Exit;
  Elem := Elem.ParentNode.RemoveChild(Elem) as TDOMElement;
  FreeAndNil(Elem);
  inherited DeletePath(Path);
end;

function TXmlDataStorage.ReadInteger(const Path: string; Default: integer): integer;
begin
  Result := StrToInt(ReadString(Path, IntToStr(Default)));
end;

function TXmlDataStorage.ReadInt64(const Path: string; Default: int64): int64;
begin
  Result := StrToInt64(ReadString(Path, IntToStr(Default)));
end;

function TXmlDataStorage.ReadString(const Path: string; const Default: string): string;
var
  Elem: TDOMElement;
begin
  Elem := FindNode(Path);
  if (Elem = nil) or (not Elem.HasAttribute(ValueAttr)) then
    Result := Default
  else
    Result := Elem.GetAttribute(ValueAttr);
end;

function TXmlDataStorage.ReadBool(const Path: string; Default: boolean): boolean;
begin
  Result := StrToBool(ReadString(Path, BoolToStr(Default, True)));
end;

function TXmlDataStorage.ReadFloat(const Path: string; Default: double): double;
begin
  Result := StrToFloat(ReadString(Path, FloatToStr(Default)));
end;

procedure TXmlDataStorage.WriteInteger(const Path: string; Value: integer);
begin
  WriteString(Path, IntToStr(Value));
  inherited WriteInteger(Path, Value);
end;

procedure TXmlDataStorage.WriteInt64(const Path: string; Value: int64);
begin
  WriteString(Path, IntToStr(Value));
  inherited WriteInt64(Path, Value);
end;

procedure TXmlDataStorage.WriteString(const Path: string; const Value: string);
var
  Elem, Son: TDOMNode;
  S: string;
begin
  Elem := RootElement;
  for S in Path.Split(['.']) do
  begin
    Son := Elem.FindNode(S);
    if Son = nil then
    begin
      Son := FDocument.CreateElement(S);
      Elem.AppendChild(Son);
    end;
    Elem := Son;
  end;
  (Elem as TDOMElement).SetAttribute(ValueAttr, Value);
  inherited WriteString(Path, Value);
end;

procedure TXmlDataStorage.WriteBool(const Path: string; Value: boolean);
begin
  WriteString(Path, BoolToStr(Value, True));
  inherited WriteBool(Path, Value);
end;

procedure TXmlDataStorage.WriteFloat(const Path: string; Value: double);
begin
  WriteString(Path, FloatToStr(Value));
  inherited WriteFloat(Path, Value);
end;

procedure TXmlDataStorage.Commit;
begin
  WriteXMLFile(FDocument, GetFileName);
end;

procedure TXmlDataStorage.Reload;
begin
  FreeAndNil(FDocument);
  LoadDocument;
  inherited Reload;
end;

constructor TXmlDataStorage.Create(const AStoragePath: string);
begin
  inherited Create(AStoragePath);
  CreateDocument;
end;

destructor TXmlDataStorage.Destroy;
begin
  FreeAndNil(FDocument);
  inherited Destroy;
end;

{ TIniDataStorage }

procedure TIniDataStorage.SplitPath(const Path: string; out Section, Ident: string);
var
  P: integer;
begin
  P := Pos('.', Path);
  if P = 0 then
  begin
    Section := GlobalSectionName;
    Ident := Path;
  end
  else
  begin
    Section := Copy(Path, 1, P - 1);
    Ident := Copy(Path, P + 1, Length(Path) - P);
  end;
end;

procedure TIniDataStorage.CreateIniFile;
begin
  FIniFile := TIniFile.Create(FStream);
  FIniFile.CacheUpdates := True;
end;

procedure TIniDataStorage.LoadStream;
var
  FileName: string;
begin
  FileName := GetFileName;
  if FileExistsUTF8(FileName) then
    FStream.LoadFromFile(FileName)
  else
    FStream.Clear;
  FStream.Position := 0;
end;

procedure TIniDataStorage.RemoveSectionIfEmpty(const Section: string);
var
  StrList: TStringList;
begin
  // have nothing more effective than this :(
  StrList := TStringList.Create;
  try
    FIniFile.ReadSection(Section, StrList);
    if StrList.Count = 0 then
      FIniFile.EraseSection(Section);
  finally
    FreeAndNil(StrList);
  end;
end;

procedure TIniDataStorage.RemoveEmptySections;
var
  Sections: TStringList;
  Section: string;
begin
  Sections := TStringList.Create;
  try
    FIniFile.ReadSections(Sections);
    for Section in Sections do
      RemoveSectionIfEmpty(Section);
  finally
    FreeAndNil(Sections);
  end;
end;

function TIniDataStorage.GetFileName: string;
begin
  Result := ChangeFileExt(inherited GetFileName, '.ini');
end;

function TIniDataStorage.GetRootElements: TStringList;
var
  TempList: TStringList;
  S: string;
  Map: TStringToPointerTree;
  It: PStringToPointerItem;
begin
  RemoveEmptySections;
  Result := TStringList.Create;
  try
    TempList := TStringList.Create;
    try
      Map := TStringToPointerTree.Create(True);
      try
        // get from section names
        FIniFile.ReadSections(TempList);
        for S in TempList do
          if S <> GlobalSectionName then
            Map.Values[S] := Pointer(42);
        // get from global section
        if FIniFile.SectionExists(GlobalSectionName) then
        begin
          FIniFile.ReadSection(GlobalSectionName, TempList);
          for S in TempList do
            Map.Values[S] := Pointer(42);
        end;
        // push to result from map
        for It in Map do
          Result.Add(It^.Name);
      finally
        FreeAndNil(Map);
      end;
    finally
      FreeAndNil(TempList);
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TIniDataStorage.VariableExists(const Path: string): boolean;
var
  Section, Ident: string;
begin
  SplitPath(Path, Section, Ident);
  Result := FIniFile.ValueExists(Section, Ident);
end;

procedure TIniDataStorage.DeleteVariable(const Path: string);
var
  Section, Ident: string;
begin
  SplitPath(Path, Section, Ident);
  FIniFile.DeleteKey(Section, Ident);
  inherited DeleteVariable(Path);
end;

procedure TIniDataStorage.DeletePath(const Path: string);
var
  Section, Ident: string;
  P: integer;
  Idents: TStringList;
  S: string;
begin
  // delete the key
  SplitPath(Path, Section, Ident);
  if FIniFile.ValueExists(Section, Ident) then
    FIniFile.DeleteKey(Section, Ident);
  // retreive the subsection and ident beginning
  P := Pos('.', Path);
  if P = 0 then
  begin
    Section := Path;
    Ident := '';
  end
  else
    Ident := Ident + '.';
  // find and delete the Idents
  Idents := TStringList.Create;
  try
    FIniFile.ReadSection(Section, Idents);
    for S in Idents do
    begin
      if (Length(S) >= Length(Ident)) and (Copy(S, 1, Length(Ident)) = Ident) then
        FIniFile.DeleteKey(Section, S);
    end;
    RemoveSectionIfEmpty(Section);
  finally
    FreeAndNil(Idents);
    inherited DeletePath(Path);
  end;
end;

function TIniDataStorage.ReadInteger(const Path: string; Default: integer): integer;
var
  Section, Ident: string;
begin
  SplitPath(Path, Section, Ident);
  Result := FIniFile.ReadInteger(Section, Ident, Default);
end;

function TIniDataStorage.ReadInt64(const Path: string; Default: int64): int64;
var
  Section, Ident: string;
begin
  SplitPath(Path, Section, Ident);
  Result := FIniFile.ReadInt64(Section, Ident, Default);
end;

function TIniDataStorage.ReadString(const Path: string; const Default: string): string;
var
  Section, Ident: string;
begin
  SplitPath(Path, Section, Ident);
  Result := FIniFile.ReadString(Section, Ident, Default);
end;

function TIniDataStorage.ReadBool(const Path: string; Default: boolean): boolean;
var
  Section, Ident: string;
begin
  SplitPath(Path, Section, Ident);
  Result := FIniFile.ReadBool(Section, Ident, Default);
end;

function TIniDataStorage.ReadFloat(const Path: string; Default: double): double;
var
  Section, Ident: string;
begin
  SplitPath(Path, Section, Ident);
  Result := FIniFile.ReadFloat(Section, Ident, Default);
end;

procedure TIniDataStorage.WriteInteger(const Path: string; Value: integer);
var
  Section, Ident: string;
begin
  SplitPath(Path, Section, Ident);
  FIniFile.WriteInteger(Section, Ident, Value);
  inherited WriteInteger(Path, Value);
end;

procedure TIniDataStorage.WriteInt64(const Path: string; Value: int64);
var
  Section, Ident: string;
begin
  SplitPath(Path, Section, Ident);
  FIniFile.WriteInt64(Section, Ident, Value);
  inherited WriteInt64(Path, Value);
end;

procedure TIniDataStorage.WriteString(const Path: string; const Value: string);
var
  Section, Ident: string;
begin
  SplitPath(Path, Section, Ident);
  FIniFile.WriteString(Section, Ident, Value);
  inherited WriteString(Path, Value);
end;

procedure TIniDataStorage.WriteBool(const Path: string; Value: boolean);
var
  Section, Ident: string;
begin
  SplitPath(Path, Section, Ident);
  FIniFile.WriteBool(Section, Ident, Value);
  inherited WriteBool(Path, Value);
end;

procedure TIniDataStorage.WriteFloat(const Path: string; Value: double);
var
  Section, Ident: string;
begin
  SplitPath(Path, Section, Ident);
  FIniFile.WriteFloat(Section, Ident, Value);
  inherited WriteFloat(Path, Value);
end;

procedure TIniDataStorage.Commit;
begin
  RemoveEmptySections;
  FIniFile.UpdateFile;
  FStream.SaveToFile(GetFileName);
end;

procedure TIniDataStorage.Reload;
begin
  FreeAndNil(FIniFile);
  LoadStream;
  CreateIniFile;
  inherited Reload;
end;

constructor TIniDataStorage.Create(const AStoragePath: string);
begin
  inherited Create(AStoragePath);
  FStream := TMemoryStream.Create;
  CreateIniFile;
end;

destructor TIniDataStorage.Destroy;
begin
  FreeAndNil(FIniFile);
  FreeAndNil(FStream);
  inherited Destroy;
end;

{ TAbstractDataStorage }

{$Hints Off}

procedure TAbstractDataStorage.DeleteVariable(const Path: string);
begin
  FPONotifyObservers(Self, ooDeleteItem, @Path);
end;

procedure TAbstractDataStorage.DeletePath(const Path: string);
begin
  FPONotifyObservers(Self, ooChange, @Path);
end;

procedure TAbstractDataStorage.WriteInteger(const Path: string; Value: integer);
begin
  FPONotifyObservers(Self, ooChange, @Path);
end;

procedure TAbstractDataStorage.WriteInt64(const Path: string; Value: int64);
begin
  FPONotifyObservers(Self, ooChange, @Path);
end;

procedure TAbstractDataStorage.WriteString(const Path: string; const Value: string);
begin
  FPONotifyObservers(Self, ooChange, @Path);
end;

procedure TAbstractDataStorage.WriteBool(const Path: string; Value: boolean);
begin
  FPONotifyObservers(Self, ooChange, @Path);
end;

procedure TAbstractDataStorage.WriteFloat(const Path: string; Value: double);
begin
  FPONotifyObservers(Self, ooChange, @Path);
end;

procedure TAbstractDataStorage.Reload;
begin
  FPONotifyObservers(Self, ooChange, nil);
end;

constructor TAbstractDataStorage.Create(const AStoragePath: string);
begin
  FStoragePath := AStoragePath;
end;

{$Hints On}

end.
