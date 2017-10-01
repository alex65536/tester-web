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

procedure ValidateFileSize(const FileName: string; MaxFileSize: integer;
  const ErrMsg: string);
procedure CopyReplaceFile(const SrcFile, DstFile: string);
procedure MoveReplaceFile(const SrcFile, DstFile: string);
function TryDeleteFile(const FileName: string; RaiseException: boolean = False): boolean;
function TryDeleteDir(const DirName: string; RaiseException: boolean = False): boolean;
function WaitForFile(const FileName: string; Mode: Word): TFileStream;

implementation

procedure ValidateFileSize(const FileName: string; MaxFileSize: integer;
  const ErrMsg: string);
var
  Size: int64;
begin
  Size := FileSizeUTF8(FileName);
  if Size > MaxFileSize * 1024 then
    raise EFileManager.CreateFmt(ErrMsg, [MaxFileSize]);
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

function TryDeleteFile(const FileName: string; RaiseException: boolean): boolean;
begin
  if (FileName <> '') and FileExistsUTF8(FileName) then
    Result := DeleteFileUTF8(FileName)
  else
    Result := True;
  if RaiseException and (not Result) then
    raise EFileManager.CreateFmt(SCouldNotDeleteFile, [FileName]);
end;

function TryDeleteDir(const DirName: string; RaiseException: boolean): boolean;
begin
  if (DirName <> '') and DirectoryExistsUTF8(DirName) then
    Result := DeleteDirectory(DirName, False)
  else
    Result := True;
  if RaiseException and (not Result) then
    raise EFileManager.CreateFmt(SCouldNotDeleteDir, [DirName]);
end;

function WaitForFile(const FileName: string; Mode: Word): TFileStream;
const
  TriesCount = 300;
  TriesTimeout = 15;
var
  I: integer;
begin
  I := 0;
  while True do
    try
      Result := TFileStream.Create(FileName, Mode);
      Exit;
    except
      if I < TriesCount then
      begin
        Inc(I);
        Sleep(TriesTimeout);
      end
      else
        raise EFileManager.CreateFmt(SFileOpenTimeout, [FileName]);
    end;
end;

end.

