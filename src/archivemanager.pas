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
unit archivemanager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, zipper, serverconfig, filemanager, webstrconsts,
  FileUtil, LazFileUtils;

type
  EArchiveManager = class(EFileManager);

procedure ValidateArchive(const ArchiveFileName: string;
  const MustFindFile: string);
procedure UnpackArchive(const ArchiveFileName, DirName: string; DeleteDir: boolean);

implementation

procedure ValidateArchive(Unzipper: TUnZipper; MustFindFile: string);
var
  SumSize: int64;
  I: integer;
begin
  // validate archive size
  ValidateFileSize(Unzipper.FileName, Config.Files_MaxArchiveSize, SArchiveTooBig);
  // calculate sum files size
  Unzipper.Examine;
  SumSize := 0;
  with Unzipper.Entries do
  begin
    for I := 0 to Count - 1 do
    begin
      SumSize := SumSize + Entries[I].Size;
      if Entries[I].ArchiveFileName = MustFindFile then
        MustFindFile := '';
    end;
  end;
  // if not found the expected file - raise an error
  if MustFindFile <> '' then
    raise EArchiveManager.CreateFmt(SFileNotFoundInArchive, [MustFindFile]);
  // if the size is to big - raise an error
  if SumSize > Config.Files_MaxUnpackedArchiveSize * 1024 then
    raise EArchiveManager.CreateFmt(SUnpackedTooBig,
      [Config.Files_MaxUnpackedArchiveSize]);
end;

function UnpackArchive(const DirName: string; Unzipper: TUnZipper;
  DeleteDir: boolean): boolean;
begin
  Result := False;
  // try to delete directory
  if DeleteDir and DirectoryExistsUTF8(DirName) then
  begin
    if not DeleteDirectory(DirName, True) then
      Exit;
  end;
  // create a directory if necessary
  if not DirectoryExistsUTF8(DirName) then
  begin
    if not CreateDirUTF8(DirName) then
      Exit;
  end;
  // unzip the archive
  try
    Unzipper.OutputPath := DirName;
    Unzipper.UnZipAllFiles;
  except
    Result := False;
    Exit;
  end;
  // everything is ok
  Result := True;
end;

procedure InternalProcessArchive(const ArchiveFileName, DirName, MustFindFile: string;
  Unpack, DeleteDir: boolean);
var
  Unzipper: TUnZipper;
begin
  try
    Unzipper := TUnZipper.Create;
    try
      Unzipper.FileName := ArchiveFileName;
      ValidateArchive(Unzipper, MustFindFile);
      if Unpack then
      begin
        if not UnpackArchive(DirName, Unzipper, DeleteDir) then
          raise EArchiveManager.Create(SCouldNotUnpack);
      end;
    finally
      FreeAndNil(Unzipper);
    end;
  except
    on E: EFileManager do
      raise EArchiveManager.Create(E.Message)
    else
      raise EArchiveManager.Create(SBadArchive);
  end;
end;

procedure ValidateArchive(const ArchiveFileName: string; const MustFindFile: string);
begin
  InternalProcessArchive(ArchiveFileName, '', MustFindFile, False, False);
end;

procedure UnpackArchive(const ArchiveFileName, DirName: string; DeleteDir: boolean);
begin
  InternalProcessArchive(ArchiveFileName, DirName, '', True, DeleteDir);
end;

end.
