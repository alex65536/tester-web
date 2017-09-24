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
unit filemanager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, webstrconsts, FileUtil, LazFileUtils;

type
  EFileManager = class(Exception);

procedure ValidateFileSize(const FileName: string; MaxFileSize: integer);
procedure CopyReplaceFile(const SrcFile, DstFile: string);
procedure MoveReplaceFile(const SrcFile, DstFile: string);

implementation

procedure ValidateFileSize(const FileName: string; MaxFileSize: integer);
var
  Size: int64;
begin
  Size := FileSizeUTF8(FileName);
  if Size > MaxFileSize * 1024 then
    raise EFileManager.CreateFmt(SFileTooBig, [MaxFileSize]);
end;

procedure CopyReplaceFile(const SrcFile, DstFile: string);
begin
  if FileExistsUTF8(DstFile) then
    DeleteFileUTF8(DstFile);
  if not CopyFile(SrcFile, DstFile) then
    raise EFileManager.CreateFmt(SCouldNotCopyFile, [SrcFile, DstFile]);
end;

procedure MoveReplaceFile(const SrcFile, DstFile: string);
var
  Success: boolean;
begin
  if FileExistsUTF8(DstFile) then
    DeleteFileUTF8(DstFile);
  Success := RenameFileUTF8(SrcFile, DstFile);
  if not Success then
  begin
    Success := CopyFile(SrcFile, DstFile);
    if Success then
      Success := DeleteFileUTF8(SrcFile);
  end;
  if not Success then
    raise EFileManager.CreateFmt(SCouldNotMoveFile, [SrcFile, DstFile]);
end;

end.

