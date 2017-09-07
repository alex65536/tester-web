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
unit webstrconsts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

resourcestring
  SNoIndentMarker = 'No indent marker!';
  SInvalidIndentMarker = 'Invalid indent marker!';
  SConvertError = 'Could not convert %s to %s.';
  SUnclosedVariable = 'Unclosed variable specifier.';
  SConflictingModifiers = 'Conflicting or repeating modifiers at variable "%s".';
  SInvalidVariableName = 'Invalid variable name "%s".';
  SVariableNotFound = 'Variable "%s" was not found.';
  SCryptoParamsNotice = 'WARNING: Changing these parameters will lead to loss of ALL passwords, because encryption algorithm will change if you change the parameters.';
  SErrSessionTerminated = 'No web session active: Session was terminated';
  SErrNoSession = 'No web session active: Session was not started';
  SUnescapeExpectedChar = 'Expected character after "\"';
  SUnescapeUnknownSequence = 'Unknown escape sequence : "%s"';
  SUnescapeExpectedHexDigits = 'Expected two hex digits after "\x"';
  SUnterminatedQuotes = 'Unterminated quotes in assignment.';

implementation

end.

