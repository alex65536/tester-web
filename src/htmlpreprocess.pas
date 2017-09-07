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
unit htmlpreprocess;

{$mode objfpc}{$H+}{$B-}{$inline on}

interface

uses
  Classes, SysUtils, AvgLvlTree, FGL, webstrconsts, escaping;

{
  HTML preprocessor syntax:
    ~[MODIFIERS][VARNAME];

      Put contents of [VARNAME] applying [MODIFIERS]. [MODIFIERS] are the
    following:
      &    Escape the contents as in HTML. This is default behaviour.
      \    Escape the contents as in JavaScript's scripts.
      #    Do not escape the contents at all.
      =    Indent the contents with the indent as in the line it must appear.
         This is default behaviour.
      <    Do not indent the contents.
      >    Indent the contents even ignoring ~!<.
      ?    If variable with the name [VARNAME] was not found, add empty string
         instead of the variable. Default behaviour is to panic when a variable
         was not found.
    MODIFIERS may not conflict or repeat. VARNAME must contain latin letters,
    numbers and underscores, but not starting with number.

    ~:[VARNAME]='[VALUE]'

      Assign [VALUE] to variable [VARNAME]. Variable [VARNAME] is stored in the
    local storage, which has the highest priority (variables from there are
    checked first) and is destroyed after the preprocessing finishes.
      The [VALUE] must be escaped as in JavaScript.

    ~~

      Just puts "~" character.

    ~<!

      This must appear at the beginning of the line. If this is met in the
    variable's content that was preprocessed, the line is inserted without
    indentation.

    ~-!

      Indicates that the line will not be preprocessed. This must appear at the
    beginning of the line.

    ~<-! or ~-<!

      Combines ~<! and ~-!.
}

type
  EHtmlPreprocessSyntaxError = class(Exception);
  ERawTextParse = class(Exception);

  { TIndentTaggedStrings }

  TIndentTaggedStrings = class(TStringList)
  private
    FEnableIndents: boolean;
    FSetRawTextMode: boolean;
    function GetRawText: string;
    procedure SetRawText(AValue: string);
  protected
    procedure InsertItem(Index: integer; const S: string); override; overload;
  public
    property EnableIndents: boolean read FEnableIndents write FEnableIndents;
    function IsIndented(Index: integer): boolean;
    function GetIndentedLine(Index: integer; const AIndent: string = '';
      AlwaysIndent: boolean = False): string;
    function GetIndentedText(const AIndent: string = '';
      AlwaysIndent: boolean = False; IndentFirstLine: boolean = True): string;
    property RawText: string read GetRawText write SetRawText;
    procedure AssignTo(Dest: TPersistent); override;
    constructor Create;
  end;

  { TVariableStorage }

  TVariableStorage = class
  protected
    function GetValues(const Key: string): string; virtual; abstract;
    procedure SetValues(const Key: string; const AValue: string); virtual; abstract;
  public
    procedure Remove(const Key: string); virtual; abstract;
    function Contains(const Key: string): boolean; virtual; abstract;
    function GetItemAsStrings(const Key: string; Strings: TIndentTaggedStrings): boolean;
    procedure SetItemAsStrings(const Key: string; Strings: TIndentTaggedStrings);
    property Values[const Key: string]: string read GetValues write SetValues; default;
  end;

  { TTreeVariableStorage }

  TTreeVariableStorage = class(TVariableStorage)
  private
    FTree: TStringToStringTree;
  protected
    function GetValues(const Key: string): string; override;
    procedure SetValues(const Key: string; const AValue: string); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Remove(const Key: string); override;
    function Contains(const Key: string): boolean; override;
  end;

  { TVariableStorageList }

  TVariableStorageList = class(specialize TFPGObjectList<TVariableStorage>)
  public
    function GetItemAsStrings(const Key: string; Strings: TIndentTaggedStrings): boolean;
  end;

  { THtmlPreprocessor }

  THtmlPreprocessor = class
  private
    FStorages: TVariableStorageList;
    FLocalStorage: TVariableStorage;
    procedure PreprocessLine(const S: string; Target: TIndentTaggedStrings);
  public
    property Storages: TVariableStorageList read FStorages;
    procedure Preprocess(Source: TStrings; Target: TIndentTaggedStrings;
      Clear: boolean = False); overload;
    function Preprocess(Source: TStrings): string; overload;
    procedure PreprocessAndInsert(Source: TStrings; AStorage: TVariableStorage;
      const Key: string);
    procedure PreprocessFile(const FileName: string; Target: TIndentTaggedStrings);
      overload;
    function PreprocessFile(const FileName: string): string; overload;
    procedure PreprocessFileAndInsert(const FileName: string;
      AStorage: TVariableStorage; const Key: string);
    constructor Create;
    destructor Destroy; override;
  end;

implementation

const
  IndentMarker = '>';
  NoIndentMarker = '<';

function GetIndentMarker(EnableIndents: boolean): char; inline;
begin
  if EnableIndents then
    Result := IndentMarker
  else
    Result := NoIndentMarker;
end;

procedure ParseRawLine(const S: string; out EnableIndents: boolean;
  out Line: string); inline;
begin
  if Length(S) = 0 then
    raise ERawTextParse.Create(SNoIndentMarker);
  case S[1] of
    IndentMarker: EnableIndents := True;
    NoIndentMarker: EnableIndents := False
    else
      raise ERawTextParse.Create(SInvalidIndentMarker);
  end;
  Line := Copy(S, 2, Length(S) - 1);
end;

{ TTreeVariableStorage }

function TTreeVariableStorage.GetValues(const Key: string): string;
begin
  Result := FTree[Key];
end;

procedure TTreeVariableStorage.SetValues(const Key: string; const AValue: string);
begin
  FTree[Key] := AValue;
end;

constructor TTreeVariableStorage.Create;
begin
  FTree := TStringToStringTree.Create(True);
end;

destructor TTreeVariableStorage.Destroy;
begin
  FreeAndNil(FTree);
  inherited Destroy;
end;

procedure TTreeVariableStorage.Remove(const Key: string);
begin
 FTree.Remove(Key);
end;

function TTreeVariableStorage.Contains(const Key: string): boolean;
begin
  Result := FTree.Contains(Key);
end;

{ THtmlPreprocessor }

procedure THtmlPreprocessor.PreprocessLine(const S: string;
  Target: TIndentTaggedStrings);

const
  IndentChars = [#0 .. ' '];
var
  Indent: boolean;
  RawText: boolean;
  Line: string;
  WasPos, Pos: integer;
  IndentStr: string;

  function IsVariableNameValid(const VarName: string): boolean;
  const
    BegApprovedSet = ['_', 'a' .. 'z', 'A' .. 'Z'];
    ApprovedSet = BegApprovedSet + ['0' .. '9'];
  var
    I: integer;
  begin
    Result := False;
    if VarName = '' then
      Exit;
    if not (VarName[1] in BegApprovedSet) then
      Exit;
    for I := 2 to Length(VarName) do
      if not (VarName[I] in ApprovedSet) then
        Exit;
    Result := True;
  end;

  procedure ValidateVariableName(const VarName: string);
  begin
    if not IsVariableNameValid(VarName) then
      raise EHtmlPreprocessSyntaxError.CreateFmt(SInvalidVariableName, [VarName]);
  end;

  procedure ParseAndAppendVariable(const Variable: string);
  type
    TIndentStyle = (isIndent, isNoIndent, isVeryIndent, isUnknown);
    TEscapeStyle = (esNone, esHTML, esJavaScript, esUnknown);
  var
    IndentStyle: TIndentStyle;
    EscapeStyle: TEscapeStyle;
    PanicIfNotFound: boolean;
    Pos: integer;
    VarName: string;
    Strings: TIndentTaggedStrings;
    I: integer;
  begin
    // parse variable modifiers
    IndentStyle := isUnknown;
    EscapeStyle := esUnknown;
    PanicIfNotFound := True;
    Pos := 2;
    while Pos < Length(Variable) do
    begin
      if (Variable[Pos] in ['&', '\', '#']) and (EscapeStyle <> esUnknown) then
        raise EHtmlPreprocessSyntaxError.CreateFmt(SConflictingModifiers, [Variable]);
      if (Variable[Pos] in ['<', '=', '>']) and (IndentStyle <> isUnknown) then
        raise EHtmlPreprocessSyntaxError.CreateFmt(SConflictingModifiers, [Variable]);
      if (Variable[Pos] = '?') and (not PanicIfNotFound) then
        raise EHtmlPreprocessSyntaxError.CreateFmt(SConflictingModifiers, [Variable]);
      case Variable[Pos] of
        '&': EscapeStyle := esHTML;
        '\': EscapeStyle := esJavaScript;
        '#': EscapeStyle := esNone;
        '=': IndentStyle := isIndent;
        '<': IndentStyle := isNoIndent;
        '>': IndentStyle := isVeryIndent;
        '?': PanicIfNotFound := False
        else
          Break;
      end;
      Inc(Pos);
    end;
    if IndentStyle = isUnknown then
      IndentStyle := isIndent;
    if EscapeStyle = esUnknown then
      EscapeStyle := esHTML;

    // parse variable name
    VarName := Copy(Variable, Pos, Length(Variable) - Pos);
    ValidateVariableName(VarName);

    // apply everything and append to line
    Strings := TIndentTaggedStrings.Create;
    try
      if not FStorages.GetItemAsStrings(VarName, Strings) then
      begin
        if PanicIfNotFound then
          raise EHtmlPreprocessSyntaxError.CreateFmt(SVariableNotFound, [VarName]);
        Strings.RawText := '';
      end;
      if EscapeStyle = esJavaScript then
      begin
        // we ignore indentation guides, just escape the contents and put it
        Line := Line + JsEscapeString(Strings.Text);
      end
      else
      begin
        if EscapeStyle = esHTML then
        begin
          // escape line-by-line
          for I := 0 to Strings.Count - 1 do
            Strings[I] := HtmlEscapeString(Strings[I]);
        end;
        // append (with indentation)
        case IndentStyle of
          isIndent: Line := Line + Strings.GetIndentedText(IndentStr, False, False);
          isNoIndent: Line := Line + Strings.GetIndentedText('', False, False);
          isVeryIndent: Line := Line + Strings.GetIndentedText(IndentStr, True, False);
        end;
      end;
    finally
      FreeAndNil(Strings);
    end;
  end;

  procedure ParseAssignment(const S: string; var Pos: integer);
  var
    WasPos: integer;
    VarName: string;
    Content: string;
    Strings: TIndentTaggedStrings;
  begin
    // parse variable name
    WasPos := Pos;
    while (Pos <= Length(S)) and (S[Pos] <> '=') do
      Inc(Pos);
    if Pos > Length(S) then
      raise EHtmlPreprocessSyntaxError.Create(SUnterminatedAssignment);
    VarName := Copy(S, WasPos, Pos - WasPos);
    ValidateVariableName(VarName);

    // skip characters between varname and contents
    Inc(Pos); // skip "="
    if (Pos > Length(S)) or (S[Pos] <> '''') then
      raise EHtmlPreprocessSyntaxError.Create(SAssignmentQuoteExpected);
    Inc(Pos); // skip leading "'"

    // parse contents
    WasPos := Pos;
    while (Pos <= Length(S)) and (S[Pos] <> '''') do
    begin
      if S[Pos] = '\' then
        Inc(Pos, 2)
      else
        Inc(Pos);
    end;
    if Pos > Length(S) then
      raise EHtmlPreprocessSyntaxError.Create(SUnterminatedQuotes);
    Content := JsUnescapeString(Copy(S, WasPos, Pos - WasPos));
    Inc(Pos); // skip trailing "'"

    // append variable to the local storage
    Strings := TIndentTaggedStrings.Create;
    try
      Strings.Text := Content;
      FLocalStorage.SetItemAsStrings(VarName, Strings);
    finally
      FreeAndNil(Strings);
    end;
  end;

begin
  // initialize
  Indent := True;
  RawText := False;
  Line := '';
  Pos := 1;

  // parse the beginning of the line
  if (Length(S) >= 3) and (Copy(S, 1, 3) = '~<!') then
  begin
    Indent := False;
    Pos := 4;
  end
  else if (Length(S) >= 3) and (Copy(S, 1, 3) = '~-!') then
  begin
    RawText := True;
    Pos := 4;
  end
  else if (Length(S) >= 4) and ((Copy(S, 1, 4) = '~<-!') or (Copy(S, 1, 4) = '~-<!')) then
  begin
    Indent := False;
    RawText := True;
    Pos := 5;
  end;

  // detect indents
  WasPos := Pos;
  while (Pos <= Length(S)) and (S[Pos] in IndentChars) do
    Inc(Pos);
  IndentStr := Copy(S, WasPos, Pos - WasPos);
  Line := Line + IndentStr;

  // parse the rest of the line
  if RawText then
    // if raw text - we just append the rest of the line
    Line := Line + Copy(S, Pos, Length(S) - Pos + 1)
  else
  begin
    // otherwise, parse it
    while Pos <= Length(S) do
    begin
      if S[Pos] = '~' then
      begin
        if Pos = Length(S) then
          raise EHtmlPreprocessSyntaxError.Create(SUnclosedVariable);
        // "~" character
        if S[Pos + 1] = '~' then
        begin
          Line := Line + '~';
          Inc(Pos, 2); // skip "~:"
          Continue;
        end;
        // ":" assignment
        if S[Pos + 1] = ':' then
        begin
          Inc(Pos, 2);
          ParseAssignment(S, Pos);
          Continue;
        end;
        // otherwise, variable
        WasPos := Pos;
        while (Pos <= Length(S)) and (S[Pos] <> ';') do
          Inc(Pos);
        if Pos > Length(S) then
          raise EHtmlPreprocessSyntaxError.Create(SUnclosedVariable);
        Inc(Pos);
        ParseAndAppendVariable(Copy(S, WasPos, Pos - WasPos));
      end
      else
      begin
        Line := Line + S[Pos];
        Inc(Pos);
      end;
    end;
  end;

  // append contents of line to the target
  Target.EnableIndents := Indent;
  Target.AddText(Line);
end;

procedure THtmlPreprocessor.Preprocess(Source: TStrings;
  Target: TIndentTaggedStrings; Clear: boolean);
var
  I: integer;
begin
  FLocalStorage := TTreeVariableStorage.Create;
  try
    FStorages.Insert(0, FLocalStorage);
    try
      if Clear then
        Target.Clear;
      for I := 0 to Source.Count - 1 do
        PreprocessLine(Source[I], Target);
    finally
      FStorages.Remove(FLocalStorage);
    end;
  finally
    FreeAndNil(FLocalStorage);
  end;
end;

function THtmlPreprocessor.Preprocess(Source: TStrings): string;
var
  Target: TIndentTaggedStrings;
begin
  Target := TIndentTaggedStrings.Create;
  try
    Preprocess(Source, Target);
    Result := Target.Text;
  finally
    FreeAndNil(Target);
  end;
end;

procedure THtmlPreprocessor.PreprocessAndInsert(Source: TStrings;
  AStorage: TVariableStorage; const Key: string);
var
  Target: TIndentTaggedStrings;
begin
  Target := TIndentTaggedStrings.Create;
  try
    Preprocess(Source, Target);
    AStorage.SetItemAsStrings(Key, Target);
  finally
    FreeAndNil(Target);
  end;
end;

procedure THtmlPreprocessor.PreprocessFile(const FileName: string;
  Target: TIndentTaggedStrings);
var
  Source: TStringList;
begin
  Source := TStringList.Create;
  try
    Source.LoadFromFile(FileName);
    Preprocess(Source, Target);
  finally
    FreeAndNil(Source);
  end;
end;

function THtmlPreprocessor.PreprocessFile(const FileName: string): string;
var
  Target: TIndentTaggedStrings;
begin
  Target := TIndentTaggedStrings.Create;
  try
    PreprocessFile(FileName, Target);
    Result := Target.Text;
  finally
    FreeAndNil(Target);
  end;
end;

procedure THtmlPreprocessor.PreprocessFileAndInsert(const FileName: string;
  AStorage: TVariableStorage; const Key: string);
var
  Target: TIndentTaggedStrings;
begin
  Target := TIndentTaggedStrings.Create;
  try
    PreprocessFile(FileName, Target);
    AStorage.SetItemAsStrings(Key, Target);
  finally
    FreeAndNil(Target);
  end;
end;

constructor THtmlPreprocessor.Create;
begin
  FStorages := TVariableStorageList.Create(False);
end;

destructor THtmlPreprocessor.Destroy;
begin
  FreeAndNil(FStorages);
  inherited Destroy;
end;

{ TVariableStorageList }

function TVariableStorageList.GetItemAsStrings(const Key: string;
  Strings: TIndentTaggedStrings): boolean;
var
  I: integer;
begin
  Result := True;
  for I := 0 to Count - 1 do
    if Items[I].GetItemAsStrings(Key, Strings) then
      Exit;
  Result := False;
end;

{ TVariableStorage }

function TVariableStorage.GetItemAsStrings(const Key: string;
  Strings: TIndentTaggedStrings): boolean;
begin
  Result := Contains(Key);
  if Result then
    Strings.RawText := Values[Key];
end;

procedure TVariableStorage.SetItemAsStrings(const Key: string;
  Strings: TIndentTaggedStrings);
begin
  Values[Key] := Strings.RawText;
end;

{ TIndentTaggedStrings }

function TIndentTaggedStrings.GetRawText: string;
var
  I: integer;
begin
  Result := '';
  for I := 0 to Count - 1 do
  begin
    if I <> 0 then
      Result := Result + LineEnding;
    Result := Result + GetIndentMarker(IsIndented(I)) + Strings[I];
  end;
end;

procedure TIndentTaggedStrings.SetRawText(AValue: string);
begin
  FSetRawTextMode := True;
  SetTextStr(AValue);
  FSetRawTextMode := False;
end;

procedure TIndentTaggedStrings.InsertItem(Index: integer; const S: string);
var
  Line: string;
  Indented: boolean;
begin
  if FSetRawTextMode then
  begin
    ParseRawLine(S, Indented, Line);
    InsertItem(Index, Line, TObject(PtrInt(Ord(Indented))));
  end
  else
    InsertItem(Index, S, TObject(PtrInt(Ord(EnableIndents))));
end;

function TIndentTaggedStrings.IsIndented(Index: integer): boolean;
begin
  Result := PtrInt(Objects[Index]) = Ord(True);
end;

function TIndentTaggedStrings.GetIndentedLine(Index: integer;
  const AIndent: string; AlwaysIndent: boolean): string;
begin
  if AlwaysIndent or IsIndented(Index) then
    Result := AIndent + Strings[Index]
  else
    Result := Strings[Index];
end;

function TIndentTaggedStrings.GetIndentedText(const AIndent: string;
  AlwaysIndent: boolean; IndentFirstLine: boolean): string;
var
  I: integer;
begin
  Result := '';
  for I := 0 to Count - 1 do
  begin
    if I <> 0 then
      Result := Result + LineEnding;
    if (I = 0) and (not IndentFirstLine) then
      Result := Result + Strings[I]
    else
      Result := Result + GetIndentedLine(I, AIndent, AlwaysIndent);
  end;
end;

procedure TIndentTaggedStrings.AssignTo(Dest: TPersistent);
begin
  if Dest is TIndentTaggedStrings then
    (Dest as TIndentTaggedStrings).RawText := Self.RawText
  else if Dest is TStrings then
    (Dest as TStrings).Text := Self.Text
  else
    raise EConvertError.CreateFmt(SConvertError, [Dest.ClassName, Self.ClassName]);
end;

constructor TIndentTaggedStrings.Create;
begin
  SkipLastLineBreak := True;
  FEnableIndents := True;
  FSetRawTextMode := False;
end;

end.
