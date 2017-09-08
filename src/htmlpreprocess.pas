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
      +    Preprocess the contents of the variable recursively.
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

    ~X!
      Indicates that the line will not appear in the preprocessed text. This
    must appear at the beginning of the line. Note that assignments are working,
    because the contents of this line are preprocessed, but not added to the
    resulting text.
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
    function BoolToObj(B: boolean): TObject; inline;
    function ObjToBool(O: TObject): boolean; inline;
  protected
    procedure InsertItem(Index: integer; const S: string); override; overload;
  public
    property EnableIndents: boolean read FEnableIndents write FEnableIndents;
    function IsIndented(Index: integer): boolean;
    procedure AppendSingleLineText(const Contents: string; BreakLine: boolean);
    procedure AppendIndentedLines(Contents: TIndentTaggedStrings;
      BreakLine: boolean; const AIndent: string = ''; AlwaysIndent: boolean = False);
    property RawText: string read GetRawText write SetRawText;
    procedure AssignTo(Dest: TPersistent); override;
    constructor Create;
  end;

  { TVariableStorage }

  TVariableStorage = class
  protected
    function GetItemAsText(const Key: string): string;
    procedure SetItemAsText(const Key: string; const Text: string);
    function GetItemAsRawText(const Key: string): string; virtual; abstract;
    procedure SetItemAsRawText(const Key: string; const AValue: string);
      virtual; abstract;
  public
    procedure Clear; virtual; abstract;
    procedure Remove(const Key: string); virtual; abstract;
    function Contains(const Key: string): boolean; virtual; abstract;
    function GetItemAsStrings(const Key: string; Strings: TIndentTaggedStrings): boolean;
    procedure SetItemAsStrings(const Key: string; Strings: TIndentTaggedStrings);
    property ItemsAsText[Key: string]: string read GetItemAsText write SetItemAsText;
    property ItemsAsRawText[Key: string]: string
      read GetItemAsRawText write SetItemAsRawText;
    procedure SetFromFile(const Key, FileName: string);
  end;

  { TTreeVariableStorage }

  TTreeVariableStorage = class(TVariableStorage)
  private
    FTree: TStringToStringTree;
  protected
    function GetItemAsRawText(const Key: string): string; override;
    procedure SetItemAsRawText(const Key: string; const AValue: string); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Remove(const Key: string); override;
    function Contains(const Key: string): boolean; override;
  end;

  { TVariableStorageList }

  TVariableStorageList = class(specialize TFPGObjectList<TVariableStorage>)
  public
    function GetItemAsStrings(const Key: string; Strings: TIndentTaggedStrings): boolean;
  end;

  { TVariableState }

  TVariableState = class
  private
    FTree: TStringToPointerTree;
  public
    procedure Visit(const VarName: string);
    procedure Unvisit(const VarName: string);
    function Visited(const VarName: string): boolean;
    constructor Create;
    destructor Destroy; override;
  end;

  { THtmlPreprocessor }

  THtmlPreprocessor = class
  private
    FStorages: TVariableStorageList;
    procedure PreprocessLine(const S: string; Indent: boolean;
      Target: TIndentTaggedStrings; LocalStorage: TVariableStorage;
      VarState: TVariableState);
    procedure InternalPreprocessor(Source, Target: TIndentTaggedStrings;
      VarState: TVariableState);
    function PreprocessVariable(const VarName: string; Target: TIndentTaggedStrings;
      VarState: TVariableState): boolean;
  public
    property Storages: TVariableStorageList read FStorages;
    procedure Preprocess(Source, Target: TIndentTaggedStrings;
      Clear: boolean = False); overload;
    function Preprocess(Source: TIndentTaggedStrings): string; overload;
    procedure PreprocessFile(const FileName: string; Target: TIndentTaggedStrings);
      overload;
    function PreprocessFile(const FileName: string): string; overload;
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

{ TVariableState }

procedure TVariableState.Visit(const VarName: string);
begin
  FTree.Values[VarName] := nil;
end;

procedure TVariableState.Unvisit(const VarName: string);
begin
  FTree.Remove(VarName);
end;

function TVariableState.Visited(const VarName: string): boolean;
begin
  Result := FTree.Contains(VarName);
end;

constructor TVariableState.Create;
begin
  FTree := TStringToPointerTree.Create(True);
end;

destructor TVariableState.Destroy;
begin
  FreeAndNil(FTree);
  inherited Destroy;
end;

{ TTreeVariableStorage }

function TTreeVariableStorage.GetItemAsRawText(const Key: string): string;
begin
  Result := FTree[Key];
end;

procedure TTreeVariableStorage.SetItemAsRawText(const Key: string; const AValue: string);
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

procedure TTreeVariableStorage.Clear;
begin
  FTree.Clear;
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

procedure THtmlPreprocessor.PreprocessLine(const S: string; Indent: boolean;
  Target: TIndentTaggedStrings; LocalStorage: TVariableStorage;
  VarState: TVariableState);
const
  IndentChars = [#0 .. ' '];
var
  Line: TIndentTaggedStrings;
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
    RecursePreprocess: boolean;
    Pos: integer;
    VarName: string;
    Strings: TIndentTaggedStrings;
    I: integer;
    Success: boolean;
  begin
    // parse variable modifiers
    IndentStyle := isUnknown;
    EscapeStyle := esUnknown;
    PanicIfNotFound := True;
    RecursePreprocess := False;
    Pos := 2;
    while Pos < Length(Variable) do
    begin
      if ((Variable[Pos] in ['&', '\', '#']) and (EscapeStyle <> esUnknown)) or
        ((Variable[Pos] in ['<', '=', '>']) and (IndentStyle <> isUnknown)) or
        ((Variable[Pos] = '?') and (not PanicIfNotFound)) or
        ((Variable[Pos] = '+') and RecursePreprocess) then
        raise EHtmlPreprocessSyntaxError.CreateFmt(SConflictingModifiers, [Variable]);
      case Variable[Pos] of
        '&': EscapeStyle := esHTML;
        '\': EscapeStyle := esJavaScript;
        '#': EscapeStyle := esNone;
        '=': IndentStyle := isIndent;
        '<': IndentStyle := isNoIndent;
        '>': IndentStyle := isVeryIndent;
        '?': PanicIfNotFound := False;
        '+': RecursePreprocess := True
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
      // retreive contents (preprocessed or not)
      if RecursePreprocess then
        Success := PreprocessVariable(VarName, Strings, VarState)
      else
        Success := FStorages.GetItemAsStrings(VarName, Strings);
      if not Success then
      begin
        if PanicIfNotFound then
          raise EHtmlPreprocessSyntaxError.CreateFmt(SVariableNotFound, [VarName]);
        Strings.RawText := '';
      end;
      // then, we escape & append
      if EscapeStyle = esJavaScript then
      begin
        // we ignore indentation guides, just escape the contents and put it
        Line.AppendSingleLineText(JsEscapeString(Strings.Text), False);
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
          isIndent: Line.AppendIndentedLines(Strings, False, IndentStr, False);
          isNoIndent: Line.AppendIndentedLines(Strings, False, '', False);
          isVeryIndent: Line.AppendIndentedLines(Strings, False, IndentStr, True);
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
    LocalStorage.SetItemAsText(VarName, Content);
  end;

var
  RawText: boolean;
  WasPos, Pos: integer;
  AppendText: boolean;
begin
  // initialize
  RawText := False;
  AppendText := True;
  Line := TIndentTaggedStrings.Create;
  Pos := 1;

  try
    // parse the beginning of the line
    if (Length(S) >= 3) and (Copy(S, 1, 3) = '~X!') then
    begin
      AppendText := False;
      Pos := 4;
    end
    else if (Length(S) >= 3) and (Copy(S, 1, 3) = '~<!') then
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
    Line.AppendSingleLineText(IndentStr, False);

    // parse the rest of the line
    if RawText then
      // if raw text - we just append the rest of the line
      Line.AppendSingleLineText(Copy(S, Pos, Length(S) - Pos + 1), False)
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
            Line.AppendSingleLineText('~', False);
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
          Line.AppendSingleLineText(S[Pos], False);
          Inc(Pos);
        end;
      end;
    end;

    // append contents of line to the target
    if AppendText then
    begin
      Target.EnableIndents := Indent;
      Target.AppendIndentedLines(Line, True);
    end;
  finally
    FreeAndNil(Line);
  end;
end;

procedure THtmlPreprocessor.InternalPreprocessor(Source, Target: TIndentTaggedStrings;
  VarState: TVariableState);
var
  I: integer;
  LocalStorage: TVariableStorage;
begin
  LocalStorage := TTreeVariableStorage.Create;
  try
    FStorages.Insert(0, LocalStorage);
    try
      for I := 0 to Source.Count - 1 do
        PreprocessLine(Source[I], Source.IsIndented(I), Target, LocalStorage,
          VarState);
    finally
      FStorages.Remove(LocalStorage);
    end;
  finally
    FreeAndNil(LocalStorage);
  end;
end;

function THtmlPreprocessor.PreprocessVariable(const VarName: string;
  Target: TIndentTaggedStrings; VarState: TVariableState): boolean;
var
  Source: TIndentTaggedStrings;

  procedure DoPreprocess;
  begin
    if VarState.Visited(VarName) then
      raise EHtmlPreprocessSyntaxError.Create(SLoopVariables);
    try
      VarState.Visit(VarName);
      InternalPreprocessor(Source, Target, VarState);
    finally
      VarState.Unvisit(VarName);
    end;
  end;

begin
  Source := TIndentTaggedStrings.Create;
  try
    Result := FStorages.GetItemAsStrings(VarName, Source);
    if Result then
      DoPreprocess;
  finally
    FreeAndNil(Source);
  end;
end;

procedure THtmlPreprocessor.Preprocess(Source, Target: TIndentTaggedStrings;
  Clear: boolean);
var
  VarState: TVariableState;
begin
  VarState := TVariableState.Create;
  try
    if Clear then
      Target.Clear;
    InternalPreprocessor(Source, Target, VarState);
  finally
    FreeAndNil(VarState);
  end;
end;

function THtmlPreprocessor.Preprocess(Source: TIndentTaggedStrings): string;
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

procedure THtmlPreprocessor.PreprocessFile(const FileName: string;
  Target: TIndentTaggedStrings);
var
  Source: TIndentTaggedStrings;
begin
  Source := TIndentTaggedStrings.Create;
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
    Strings.RawText := GetItemAsRawText(Key);
end;

procedure TVariableStorage.SetItemAsStrings(const Key: string;
  Strings: TIndentTaggedStrings);
begin
  SetItemAsRawText(Key, Strings.RawText);
end;

procedure TVariableStorage.SetFromFile(const Key, FileName: string);
var
  Strings: TIndentTaggedStrings;
begin
  Strings := TIndentTaggedStrings.Create;
  try
    Strings.LoadFromFile(FileName);
    SetItemAsStrings(Key, Strings);
  finally
    FreeAndNil(Strings);
  end;
end;

function TVariableStorage.GetItemAsText(const Key: string): string;
var
  Strings: TIndentTaggedStrings;
begin
  Strings := TIndentTaggedStrings.Create;
  try
    if GetItemAsStrings(Key, Strings) then
      Result := Strings.Text
    else
      Result := '';
  finally
    FreeAndNil(Strings);
  end;
end;

procedure TVariableStorage.SetItemAsText(const Key: string; const Text: string);
var
  Strings: TIndentTaggedStrings;
begin
  Strings := TIndentTaggedStrings.Create;
  try
    Strings.Text := Text;
    SetItemAsStrings(Key, Strings);
  finally
    FreeAndNil(Strings);
  end;
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

function TIndentTaggedStrings.BoolToObj(B: boolean): TObject;
begin
  Result := TObject(PtrInt(Ord(B)));
end;

function TIndentTaggedStrings.ObjToBool(O: TObject): boolean;
begin
  Result := PtrInt(O) = Ord(True);
end;

procedure TIndentTaggedStrings.InsertItem(Index: integer; const S: string);
var
  Line: string;
  Indented: boolean;
begin
  if FSetRawTextMode then
  begin
    ParseRawLine(S, Indented, Line);
    InsertItem(Index, Line, BoolToObj(Indented));
  end
  else
    InsertItem(Index, S, BoolToObj(EnableIndents));
end;

function TIndentTaggedStrings.IsIndented(Index: integer): boolean;
begin
  Result := ObjToBool(Objects[Index]);
end;

procedure TIndentTaggedStrings.AppendSingleLineText(const Contents: string;
  BreakLine: boolean);
begin
  if (Count = 0) or BreakLine then
    Add(Contents)
  else
    Strings[Count - 1] := Strings[Count - 1] + Contents;
end;

procedure TIndentTaggedStrings.AppendIndentedLines(Contents: TIndentTaggedStrings;
  BreakLine: boolean; const AIndent: string; AlwaysIndent: boolean);
var
  WasEnableIndents: boolean;
  ApplyIndent: boolean;
  I: integer;
begin
  WasEnableIndents := EnableIndents;
  try
    for I := 0 to Contents.Count - 1 do
    begin
      if (I = 0) and (not BreakLine) then
      begin
        AppendSingleLineText(Contents[I], False);
        Continue;
      end;
      ApplyIndent := AlwaysIndent or Contents.IsIndented(I);
      EnableIndents := AlwaysIndent or (ApplyIndent and WasEnableIndents);
      if ApplyIndent then
        AppendSingleLineText(AIndent + Contents[I], True)
      else
        AppendSingleLineText(Contents[I], True);
    end;
  finally
    EnableIndents := WasEnableIndents;
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
