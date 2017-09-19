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
unit editableobjects;

{$mode objfpc}{$H+}{$M+}

interface

uses
  Classes, SysUtils, TypInfo;

type
  TEditableAccessRights = (erNone, erRead, erWrite, erOwner);
  TEditableAccessRightsSet = set of TEditableAccessRights;

const
  AccessCanReadSet = [erRead, erWrite, erOwner];
  AccessCanWriteSet = [erWrite, erOwner];

type
  TEditableObject = class
    // TODO : Write it !!!!!!
  end;

function AccessRightsToStr(ARights: TEditableAccessRights): string;
function StrToAccessRights(const S: string): TEditableAccessRights;

implementation

function AccessRightsToStr(ARights: TEditableAccessRights): string;
begin
  Result := GetEnumName(TypeInfo(TEditableAccessRights), Ord(ARights));
end;

function StrToAccessRights(const S: string): TEditableAccessRights;
var
  R: TEditableAccessRights;
begin
  for R in TEditableAccessRights do
    if AccessRightsToStr(R) = S then
      Exit(R);
  raise EConvertError.Create(SNoSuchAccessRights);
end;

end.

