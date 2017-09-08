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
unit page1;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, httpdefs, fpHTTP, fpWeb, tswebhtmlpages, dateutils;

type

  { TPage1Module }

  TPage1Module = class(TFPWebModule)
    procedure DataModuleRequest(Sender: TObject; {%H-}ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
  end;

var
  Page1Module: TPage1Module;

implementation

{$R *.lfm}

{ TPage1Module }

procedure TPage1Module.DataModuleRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
  APage: TSimpleHtmlPage;
  Time: TDateTime;
begin
  Time := Now;

  APage := TSimpleHtmlPage.Create;
  APage.Title := 'Page 1';
  APage.TextContent := 'This is page 1.';
  try
    APage.UpdateResponse(AResponse);
    Handled := True;
  finally
    FreeAndNil(APage);
  end;

  WriteLn('Page1 rendered in ', SecondSpan(Time, Now) : 0 : 3, 's.');
end;

initialization
  RegisterHTTPModule('page1', TPage1Module);
end.

