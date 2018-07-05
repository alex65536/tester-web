{
  richtextconsole - Colored/bold/italic/underline text in terminal

  Copyright (C) 2018 Alexander Kernozhitsky <sh200105@mail.ru>

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.
}
unit richtextconsole;

{$mode objfpc}{$H+}
{$WARN 5024 off : Parameter "$1" not used}

interface

uses
  Classes, SysUtils;

type
  TConsoleColor = (
    cclBlack, cclRed, cclGreen, cclYellow,
    cclBlue, cclPurple, cclCyan, cclSilver,
    cclGray, cclLtRed, cclLtGreen, cclLtYellow,
    cclLtBlue, cclLtPurple, cclLtCyan, cclWhite
  );

procedure rtcSetFgColor(var AFile: TextFile; AColor: TConsoleColor); inline;
procedure rtcSetFgColor(AColor: TConsoleColor); inline;

procedure rtcSetBgColor(var AFile: TextFile; AColor: TConsoleColor); inline;
procedure rtcSetBgColor(AColor: TConsoleColor); inline;

procedure rtcSetBold(var AFile: TextFile; ABold: boolean); inline;
procedure rtcSetBold(ABold: boolean = True); inline;

procedure rtcSetItalic(var AFile: TextFile; AItalic: boolean); inline;
procedure rtcSetItalic(AItalic: boolean = True); inline;

procedure rtcSetUnderline(var AFile: TextFile; AUnderline: boolean); inline;
procedure rtcSetUnderline(AUnderline: boolean = True); inline;

procedure rtcResetStyle(var AFile: TextFile); inline;
procedure rtcResetStyle; inline;

function rtcSupportsStyles(var AFile: TextFile): boolean; inline;
function rtcSupportsStyles: boolean; inline;

implementation

{$ifdef Unix}
  uses
    termio;

  const
    FgColorTable: array [TConsoleColor] of string = (
      '30', '31', '32', '33', '34', '35', '36', '37',
      '90', '91', '92', '93', '94', '95', '96', '97'
    );
    BgColorTable: array [TConsoleColor] of string = (
      '40', '41', '42', '43', '44', '45', '46', '47',
      '100', '101', '102', '103', '104', '105', '106', '107'
    );
    BoldTable: array [boolean] of string = ('21', '1');
    ItalicTable: array [boolean] of string = ('23', '3');
    UnderlineTable: array [boolean] of string = ('24', '4');

  procedure rtcInternalSetFgColor(var AFile: TextFile; AColor: TConsoleColor);
  begin
    Write(AFile, #27'[', FgColorTable[AColor], 'm');
  end;

  procedure rtcInternalSetBgColor(var AFile: TextFile; AColor: TConsoleColor);
  begin
    Write(AFile, #27'[', BgColorTable[AColor], 'm');
  end;

  procedure rtcInternalSetBold(var AFile: TextFile; ABold: boolean);
  begin
    Write(AFile, #27'[', BoldTable[ABold], 'm');
  end;

  procedure rtcInternalSetItalic(var AFile: TextFile; AItalic: boolean);
  begin
    Write(AFile, #27'[', ItalicTable[AItalic], 'm');
  end;     

  procedure rtcInternalSetUnderline(var AFile: TextFile; AUnderline: boolean);
  begin
    Write(AFile, #27'[', UnderlineTable[AUnderline], 'm');
  end;

  procedure rtcInternalResetStyle(var AFile: TextFile);
  begin
    Write(AFile, #27'[0m');
  end;

  function rtcInternalSupportsStyles(var AFile: TextFile): boolean;
  begin
    Result := IsATTY(AFile) = 1;
  end;

  {$define ImplOk}
{$endif}


{$ifdef Windows}
  uses
    Windows;

  const
    ColorTranslateTable: array [TConsoleColor] of word = (
      0, 4, 2, 6, 1, 5, 3, 7,
      8, 12, 10, 14, 9, 13, 11, 15
    );

  var
    StdOutAttr, StdErrAttr: word;
    StdOutDefault, StdErrDefault: word;

  procedure GetAttrVars(HFile: THandle; out PAttr, PDefault: PWord);
  begin
    if HFile = GetFileHandle(StdOut) then
    begin
      PAttr := @StdOutAttr;
      PDefault := @StdOutDefault;
    end
    else if HFile = GetFileHandle(StdErr) then
    begin
      PAttr := @StdErrAttr;
      PDefault := @StdErrDefault;
    end
    else
    begin
      PAttr := nil;
      PDefault := nil;
    end;
  end;

  procedure rtcInternalSetFgColor(var AFile: TextFile; AColor: TConsoleColor);
  var
    HFile: THandle;
    PAttr, PDefault: PWord;
  begin
    HFile := GetFileHandle(AFile);
    GetAttrVars(HFile, PAttr, PDefault);  
    PAttr^ := PAttr^ xor (PAttr^ and 15);
    PAttr^ := PAttr^ xor ColorTranslateTable[AColor];
    SetConsoleTextAttribute(HFile, PAttr^);
  end;

  procedure rtcInternalSetBgColor(var AFile: TextFile; AColor: TConsoleColor);
  var
    HFile: THandle;
    PAttr, PDefault: PWord;
  begin
    HFile := GetFileHandle(AFile);
    GetAttrVars(HFile, PAttr, PDefault);
    PAttr^ := PAttr^ xor (PAttr^ and 240);
    PAttr^ := PAttr^ xor (ColorTranslateTable[AColor] shl 4);
    SetConsoleTextAttribute(HFile, PAttr^);
  end;

  procedure rtcInternalSetBold(var AFile: TextFile; ABold: boolean);
  begin
    // not supported on windows
  end;

  procedure rtcInternalSetItalic(var AFile: TextFile; AItalic: boolean);
  begin
    // not supported on windows
  end;

  procedure rtcInternalSetUnderline(var AFile: TextFile; AUnderline: boolean);
  begin
    // not supported on windows
  end;

  procedure rtcInternalResetStyle(var AFile: TextFile);
  var
    HFile: THandle;
    PAttr, PDefault: PWord;
  begin
    HFile := GetFileHandle(AFile);
    GetAttrVars(HFile, PAttr, PDefault);
    PAttr^ := PDefault^;
    SetConsoleTextAttribute(HFile, PAttr^);
  end;

  function rtcInternalSupportsStyles(var AFile: TextFile): boolean;
  begin
    Result := (GetFileHandle(AFile) = GetStdHandle(STD_OUTPUT_HANDLE)) or
      (GetFileHandle(AFile) = GetStdHandle(STD_ERROR_HANDLE));
  end;

  procedure rtcInitWin;
  var
    Info: CONSOLE_SCREEN_BUFFER_INFO;
  begin
    GetConsoleScreenBufferInfo(GetFileHandle(StdOut), @Info);
    StdOutAttr := Info.wAttributes;
    GetConsoleScreenBufferInfo(GetFileHandle(StdErr), @Info);
    StdErrAttr := Info.wAttributes;
    StdOutDefault := StdOutAttr;
    StdErrDefault := StdErrAttr;
  end;

  {$define ImplOk}
{$endif}


{$ifndef ImplOk}
  {$warning Currently, there is no implementation for colored text in your OS.}
  {$warning So, RichTextConsole methods are just stubs for now.}
  {$warning You can implement them for your OS.}

  procedure rtcInternalSetFgColor(var AFile: TextFile; AColor: TConsoleColor);
  begin
  end;

  procedure rtcInternalSetBgColor(var AFile: TextFile; AColor: TConsoleColor);
  begin
  end;

  procedure rtcInternalSetBold(var AFile: TextFile; ABold: boolean);
  begin
  end;

  procedure rtcInternalSetItalic(var AFile: TextFile; AItalic: boolean);
  begin
  end;

  procedure rtcInternalSetUnderline(var AFile: TextFile; AUnderline: boolean);
  begin
  end;

  procedure rtcInternalResetStyle(var AFile: TextFile);
  begin
  end;

  function rtcInternalSupportsStyles(var AFile: TextFile): boolean;
  begin
    Result := False;
  end;
{$endif}

procedure rtcSetFgColor(var AFile: TextFile; AColor: TConsoleColor); inline;
begin
  if rtcSupportsStyles(AFile) then
    rtcInternalSetFgColor(AFile, AColor);
end;

procedure rtcSetFgColor(AColor: TConsoleColor); inline;
begin
  rtcSetFgColor(StdOut, AColor);
end;

procedure rtcSetBgColor(var AFile: TextFile; AColor: TConsoleColor); inline;
begin
  if rtcSupportsStyles(AFile) then
    rtcInternalSetBgColor(AFile, AColor);
end;

procedure rtcSetBgColor(AColor: TConsoleColor); inline;
begin
  rtcSetBgColor(StdOut, AColor);
end;

procedure rtcSetBold(var AFile: TextFile; ABold: boolean); inline;
begin
  if rtcSupportsStyles(AFile) then
    rtcInternalSetBold(AFile, ABold);
end;

procedure rtcSetBold(ABold: boolean); inline;
begin
  rtcSetBold(StdOut, ABold);
end;

procedure rtcSetItalic(var AFile: TextFile; AItalic: boolean); inline;
begin
  if rtcSupportsStyles(AFile) then
    rtcInternalSetItalic(AFile, AItalic);
end;

procedure rtcSetItalic(AItalic: boolean); inline;
begin
  rtcSetItalic(StdOut, AItalic);
end;

procedure rtcSetUnderline(var AFile: TextFile; AUnderline: boolean); inline;
begin
  if rtcSupportsStyles(AFile) then
    rtcInternalSetUnderline(AFile, AUnderline);
end;

procedure rtcSetUnderline(AUnderline: boolean); inline;
begin
  rtcSetUnderline(StdOut, AUnderline);
end;

procedure rtcResetStyle(var AFile: TextFile);
begin
  if rtcSupportsStyles(AFile) then
    rtcInternalResetStyle(AFile);
end;

procedure rtcResetStyle; inline;
begin
  rtcResetStyle(StdOut);
end;

function rtcSupportsStyles(var AFile: TextFile): boolean;
begin
  Result := rtcInternalSupportsStyles(AFile);
end;

function rtcSupportsStyles: boolean; inline;
begin
  Result := rtcSupportsStyles(StdOut);
end;

initialization
  {$ifdef windows}
    rtcInitWin;
  {$endif}

finalization
  rtcResetStyle;

end.

