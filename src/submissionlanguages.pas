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
unit submissionlanguages;

{$mode objfpc}{$H+}{$M+}

interface

uses
  Classes, SysUtils, compilers, typinfo, webstrconsts;

type
  ESubmissionLanguage = class(Exception);

  TSubmissionLanguage = (slPascal, slDelphi, slC, slCpp, slCpp11);

const
  LanguageExts: array [TSubmissionLanguage] of string =
    ('.pas', '.dpr', '.c', '.cpp', '.cpp');

  LanguageInternalExts: array [TSubmissionLanguage] of string =
    ('.pas', '.dpr', '.c', '.cxx', '.cpp');

  LanguageCompilers: array [TSubmissionLanguage] of TCompilerClass = (
    TFreePascalCompiler,
    TDelphiCompiler,
    TGnuCCompiler,
    TGnuCppCompiler,
    TGnuCpp11Compiler
    );

function LanguageFullCompilerNames(ALanguage: TSubmissionLanguage): string;
function LanguageToStr(ALanguage: TSubmissionLanguage): string;
function StrToLanguage(const S: string): TSubmissionLanguage;

implementation

var
  FFullCompilerNames: array [TSubmissionLanguage] of string;

function LanguageFullCompilerNames(ALanguage: TSubmissionLanguage): string;
begin
  Result := FFullCompilerNames[ALanguage];
end;

function LanguageToStr(ALanguage: TSubmissionLanguage): string;
begin
  Result := GetEnumName(TypeInfo(TSubmissionLanguage), Ord(ALanguage));
end;

function StrToLanguage(const S: string): TSubmissionLanguage;
var
  L: TSubmissionLanguage;
begin
  for L in TSubmissionLanguage do
    if LanguageToStr(L) = S then
      Exit(L);
  raise ESubmissionLanguage.CreateFmt(SUnknownSubmissionLanguage, [S]);
end;

procedure InitLanguages;
var
  Compiler: TCompiler;
  L: TSubmissionLanguage;
begin
  for L in TSubmissionLanguage do
  begin
    RegisterCompiler(LanguageInternalExts[L], LanguageCompilers[L]);
    Compiler := LanguageCompilers[L].Create;
    try
      FFullCompilerNames[L] := Compiler.CompilerFullName;
    finally
      FreeAndNil(Compiler);
    end;
  end;
end;

initialization
  InitLanguages;

end.
