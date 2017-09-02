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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AvgLvlTree, FGL;

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
    MODIFIERS may not conflict. VARNAME must contain latin letters, numbers and
    underscores, but not starting with number.

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

  TIndentTaggedStrings = class(TStringList)
  protected
    procedure InsertItem(Index: Integer; const S: string; O: TObject); override;
  public
    property DisableIndents: boolean read FDisableIndents write FDisableIndents;
    function GetIndentedLine(Index: integer; const AIndent: string = ''): string;
    function GetIndentedText(const AIndent: string = ''): string;
    property RawText: string read GetRawText write SetRawText;
    procedure AssignTo(Dest: TPersistent); override;
  end;

  TVariableStorage = class(TStringToStringTree)
  public
    constructor Create;
    function GetItemAsStrings(Strings: TIndentTaggedStrings): boolean;
    procedure SetItemAsStrings(Strings: TIndentTaggedStrings);
  end;

  TVariableStorageList = class(specialize TFPGObjectList<TVariableStorage>)
  public
    function GetItemAsStrings(Strings: TIndentTaggedStrings): boolean;
  end;

  THtmlPreprocessor = class
  public
    property Storages: TVariableStorageList read FStorages;
    procedure Preprocess(Source, Target: TIndentTaggedStrings);
    function Preprocess(Source: TIndentTaggedStrings): string;
    procedure PreprocessFile(const FileName: string; Target: TIndentTaggedStrings);
    function PreprocessFile(const FileName: string): string;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

end.

