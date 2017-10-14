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
unit tswebutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, webstrconsts;

function Id2Str(AID: integer): string;
function Str2Id(const AID: string): integer;

procedure ValidateVarNameStr(const VarType, VarName: string; AExceptClass: ExceptClass);

implementation

function Id2Str(AID: integer): string;
begin
  Result := 'id' + IntToStr(AID);
end;

function Str2Id(const AID: string): integer;
begin
  if Pos('id', AID) <> 1 then
    raise EConvertError.CreateFmt(SInvalidIDStr, [AID]);
  Result := AID.Substring(2).ToInteger;
end;

procedure ValidateVarNameStr(const VarType, VarName: string; AExceptClass: ExceptClass);
const
  AvailableChars = ['A' .. 'Z', 'a' .. 'z', '0' .. '9', '_', '-'];
  Digits = ['0' .. '9'];
  AvailableCharsStr = '[''A'' .. ''Z'', ''a'' .. ''z'', ''0'' .. ''9'', ''_'', ''-'']';
var
  C: char;
begin
  if VarName = '' then
    raise AExceptClass.CreateFmt(SVarNameNonEmpty, [VarType]);
  if VarName[1] in Digits then
    raise AExceptClass.CreateFmt(SVarNameLeadingDigit, [VarType]);
  for C in VarName do
    if not (C in AvailableChars) then
      raise AExceptClass.CreateFmt(SVarNameInvalidChar, [VarType, AvailableCharsStr]);
end;

end.

