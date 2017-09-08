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
unit index;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, httpdefs, fpHTTP, fpWeb, fphttpapp, tswebhtmlpages,
  htmlpreprocess;

type

  { TIndexHtmlPage }

  TIndexHtmlPage = class(TTesterHtmlPage)
  protected
    procedure AddFeatures; override;
    procedure DoGetInnerContents(Strings: TIndentTaggedStrings); override;
  public
    constructor Create;
  end;

  { TIndexModule }

  TIndexModule = class(TFPWebModule)
    procedure DataModuleRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
  end;

var
  IndexModule: TIndexModule;

implementation

{$R *.lfm}

{ TIndexHtmlPage }

procedure TIndexHtmlPage.AddFeatures;
begin
  inherited AddFeatures;
  AddFeature(THeaderFeature);
  AddFeature(TContentFeature);
  AddFeature(TFooterFeature);
end;

procedure TIndexHtmlPage.DoGetInnerContents(Strings: TIndentTaggedStrings);
begin
  Strings.Add('Hello world!');
end;

constructor TIndexHtmlPage.Create;
begin
  Title := 'Main Page';
end;

{ TIndexModule }

procedure TIndexModule.DataModuleRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
  APage: TIndexHtmlPage;
begin
  APage := TIndexHtmlPage.Create;
  try
    Response.ContentType := 'text/html;charset=utf-8';
    Response.Content := APage.Content;
    Handled := True;
  finally
    FreeAndNil(APage);
  end;
end;

initialization
  RegisterHTTPModule('index', TIndexModule);
end.

