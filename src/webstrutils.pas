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
unit webstrutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, webstrconsts, LazFileUtils;

function FileSizeToStr(ASize: int64): string;
function FileSizeStr(const FileName: string): string;

implementation

function FileSizeToStr(ASize: int64): string;
begin
  if ASize < 1024 then
    Result := Format(SFileSizeBytes, [ASize])
  else if ASize < 1024 * 1024 then
    Result := Format(SFileSizeKBytes, [ASize / 1024])
  else
    Result := Format(SFileSizeMBytes, [ASize / 1048576]);
end;

function FileSizeStr(const FileName: string): string;
begin
  Result := FileSizeToStr(FileSizeUtf8(FileName));
end;

end.

