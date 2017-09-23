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
unit tswebdirectories;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, LazFileUtils, LazUTF8;

function InternalDirLocation: string;
function ExpandInternalDirLocation(const ALocation: string): string;

implementation

function InternalDirLocation: string;
begin
  Result := AppendPathDelim(SysToUTF8(GetUserDir)) + ApplicationName;
  if not DirectoryExistsUTF8(Result) then
    CreateDirUTF8(Result);
end;

function ExpandInternalDirLocation(const ALocation: string): string;
var
  PathItem: string;
begin
  Result := AppendPathDelim(InternalDirLocation);
  for PathItem in ALocation.Split(['/', '\']) do
  begin
    Result := AppendPathDelim(Result + PathItem);
    if not DirectoryExistsUTF8(Result) then
      CreateDirUTF8(Result);
  end;
end;

end.

