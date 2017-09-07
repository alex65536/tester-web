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
unit escaping;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, webstrconsts;

type
  EStringUnescape = class(Exception);

function HtmlEscapeString(const S: string): string;
function JavaScriptEscapeString(const S: string): string;
function JavaScriptUnescapeString(const S: string): string;

implementation

function HtmlEscapeString(const S: string): string;
var
  C: char;
begin
  Result := '';
  for C in S do
  begin
    case C of
      '"': Result := Result + '&quot;';
      '&': Result := Result + '&amp;';
      '<': Result := Result + '&lt;';
      '>': Result := Result + '&gt;';
      '~': Result := Result + '&#126;' // we escape tilde too!
      else Result := Result + C;
    end;
  end;
end;

function JavaScriptEscapeString(const S: string): string;
var
  C: char;
begin
  Result := '';
  for C in S do
  begin
    case C of
      #0 : Result := Result + '\0';
      #8 : Result := Result + '\b';
      #9 : Result := Result + '\t';
      #10: Result := Result + '\n';
      #11: Result := Result + '\v';
      #12: Result := Result + '\f';
      #13: Result := Result + '\r';
     '''': Result := Result + '\''';
      '"': Result := Result + '\"';
      '\': Result := Result + '\\';
      '~': Result := Result + '\x7e' // we escape tilde too!
      else Result := Result + C;
    end;
  end;
end;

function JavaScriptUnescapeString(const S: string): string;
const
  HexNumbers = ['0' .. '9', 'a' .. 'f', 'A' .. 'F'];
var
  I: integer;
begin
  Result := '';
  I := 1;
  while I <= Length(S) do
  begin
    // simple characters - just insert
    if S[I] <> '\' then
    begin
      Result := Result + S[I];
      Inc(I);
      Continue;
    end;
    // otherwise, try to unescape
    if I = Length(S) then
      raise EStringUnescape.Create(SUnescapeExpectedChar);
    Inc(I);
    case S[I] of
      '0': Result := Result + #0;
      'b': Result := Result + #8;
      't': Result := Result + #9;
      'n': Result := Result + #10;
      'v': Result := Result + #11;
      'f': Result := Result + #12;
      'r': Result := Result + #13;
     '''': Result := Result + '''';
      '"': Result := Result + '"';
      '\': Result := Result + '\';
      'x':
        begin
          if I + 2 > Length(S) then
            raise EStringUnescape.Create(SUnescapeExpectedHexDigits);
          if not ((S[I + 1] in HexNumbers) and (S[I + 2] in HexNumbers)) then
            raise EStringUnescape.Create(SUnescapeExpectedHexDigits);
          Result := Result + Chr(StrToInt('$' + Copy(S, I + 1, 2)));
          Inc(I, 2);
        end
      else
        raise EStringUnescape.CreateFmt(SUnescapeUnknownSequence, ['\' + S[I]]);
    end;
    Inc(I);
  end;
end;

end.

