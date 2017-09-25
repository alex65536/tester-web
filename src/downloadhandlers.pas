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
unit downloadhandlers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, webmodules, HTTPDefs, LazFileUtils, webstrconsts;

type

  { TDownloadModuleHandler }

  TDownloadModuleHandler = class(TWebModuleHandler)
  private
    FDiskFileName: string;
    FFileExt: string;
    FFileMimeType: string;
    FFileName: string;
    FFilePath: string;
    FFullFileName: string;
  protected
    property FullFileName: string read FFullFileName;
    property FileName: string read FFileName;
    property FileExt: string read FFileExt;
    property FilePath: string read FFilePath;
    property FileMimeType: string read FFileMimeType write FFileMimeType;
    property DiskFileName: string read FDiskFileName write FDiskFileName;
    procedure DoCleanup; virtual;
    procedure InternalHandleDownload; virtual; abstract;
  public
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse;
      var Handled: boolean); override;
  end;

implementation

{ TDownloadModuleHandler }

procedure TDownloadModuleHandler.DoCleanup;
begin
  FDiskFileName := '';
  FFileExt := '';
  FFileMimeType := '';
  FFileName := '';
  FFilePath := '';
  FFullFileName := '';
end;

procedure TDownloadModuleHandler.HandleRequest(ARequest: TRequest;
  AResponse: TResponse; var Handled: boolean);
var
  Location: string;
  P: integer;
begin
  try
    Location := ARequest.URI;
    WriteLn('Location = ', Location);
    // preprocess URI
    // e.g. we got URI "/download-file/file/example.html" & we are on Windows
    // after preprocessing we'll get "file\example.html".
    Delete(Location, 1, 1);
    P := Pos('/', Location);
    if P = 0 then
      Location := ''
    else
      Delete(Location, 1, P);
    Location := Location.Replace('/', DirectorySeparator);
    WriteLn('New location = ', Location);
    // fill variables
    FFullFileName := Location;
    FFileName := ExtractFileName(Location);
    FFileExt := ExtractFileExt(Location);
    FFilePath := ExtractFileDir(Location);
    FDiskFileName := '';
    FFileMimeType := 'application/octet-stream';
    // internal preprocess
    InternalHandleDownload;
    // send the file (if found)
    if (FDiskFileName <> '') and (FileExistsUTF8(FDiskFileName)) then
    begin
      // send file
      AResponse.ContentStream := TFileStream.Create(FDiskFileName, fmOpenRead);
      AResponse.FreeContentStream := True;
      AResponse.ContentType := FFileMimeType;
    end
    else
    begin
      // send 404
      AResponse.Code := 404;
      AResponse.CodeText := SCode404;
    end;
    Handled := True;
  finally
    DoCleanup;
  end;
end;

end.

