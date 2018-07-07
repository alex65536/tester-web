{
  This file is part of Tester Web

  Copyright (C) 2018 Alexander Kernozhitsky <sh200105@mail.ru>

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
unit tswebfile;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, fpwebfile, HTTPDefs, LazFileUtils, tswebconfig, tswebutils, DateUtils;

type

  { TTsWebFile }

  TTsWebFile = class(TFPCustomFileModule)
  private
    FRequest: TRequest;
  protected
    property Request: TRequest read FRequest;
  public
    function MapFileName(const AFileName: String): String; override;
    function AllowFile(const AFileName: String): Boolean; override;
    procedure SendFile(const AFileName: String; AResponse: TResponse); override;
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
  end;

implementation

{ TTsWebFile }

function TTsWebFile.MapFileName(const AFileName: String): String;
begin
  Result := inherited MapFileName(AFileName);
  Result := ExpandFileNameUTF8(Result);
  Result := Result.Replace('/', DirectorySeparator).Replace('\', DirectorySeparator);
end;

function TTsWebFile.AllowFile(const AFileName: String): Boolean;
var
  Directory: string;
begin
  Directory := AppendPathDelim(MapFileName(''));
  Result := Pos(Directory, AFileName) = 1;
end;

procedure TTsWebFile.SendFile(const AFileName: String; AResponse: TResponse);
var
  CheckModifiedSince: TDateTime;
  FileTimestamp: TDateTime;
begin
  AResponse.CacheControl := 'max-age=' + IntToStr(Config.Cache_StaticCacheSeconds);
  if Request.IfModifiedSince = '' then
    CheckModifiedSince := DateUtils.EncodeDateDay(1024, 1)
  else
    CheckModifiedSince := HTTPDateToDateTime(Request.IfModifiedSince);
  FileTimestamp := FileDateToDateTime(FileAgeUTF8(AFileName));
  if CompareDateTime(CheckModifiedSince, FileTimestamp) >= 0 then
  begin
    AResponse.Code := 304;
    AResponse.CodeText := 'Not Modified';
    AResponse.SendContent;
    Exit;
  end;
  AResponse.LastModified := DateTimeToHTTPDate(FileTimestamp);
  inherited SendFile(AFileName, AResponse);
end;

procedure TTsWebFile.HandleRequest(ARequest: TRequest; AResponse: TResponse);
begin
  FRequest := ARequest;
  try
    inherited HandleRequest(ARequest, AResponse);
  finally
    FRequest := nil;
  end;
end;

initialization
  DefaultFileModuleClass := TTsWebFile;
end.

