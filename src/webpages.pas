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
unit webpages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, htmlpagewebmodules, tswebpagesbase, tswebfeatures,
  tswebnavbars, navbars, htmlpreprocess, fphttp, htmlpages;

type

  { TDefaultNavBar }

  TDefaultNavBar = class(TTesterNavBar)
  protected
    procedure DoCreateElements; override;
  end;

  { TSimpleHtmlPage }

  TSimpleHtmlPage = class(TTesterHtmlPage, IPageNavBar)
  private
    FTextContent: string;
    FNavBar: TDefaultNavBar;
  protected
    procedure AddFeatures; override;
    procedure DoGetInnerContents(Strings: TIndentTaggedStrings); override;
    procedure DoSetVariables; override;
    // IPageNavBar
    function GetNavBar: TNavBar;
    // IUnknown
    function QueryInterface(constref iid: tguid; out obj): longint; cdecl;
    function _AddRef: longint; cdecl;
    function _Release: longint; cdecl;
  public
    property TextContent: string read FTextContent write FTextContent;
    procedure Clear; override;
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TIndexWebModule }

  TIndexWebModule = class(THtmlPageWebModule)
  protected
    function DoCreatePage: THtmlPage; override;
  end;

  { TPage1WebModule }

  TPage1WebModule = class(THtmlPageWebModule)
  protected
    function DoCreatePage: THtmlPage; override;
  end;

  { TPage2WebModule }

  TPage2WebModule = class(THtmlPageWebModule)
  protected
    function DoCreatePage: THtmlPage; override;
  end;

implementation

{ TPage2WebModule }

function TPage2WebModule.DoCreatePage: THtmlPage;
begin
  Result := TSimpleHtmlPage.Create;
  try
    with Result as TSimpleHtmlPage do
    begin
      Title := 'Page 2';
      TextContent := 'This is page 2.';
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

{ TPage1WebModule }

function TPage1WebModule.DoCreatePage: THtmlPage;
begin
  Result := TSimpleHtmlPage.Create;
  try
    with Result as TSimpleHtmlPage do
    begin
      Title := 'Page 1';
      TextContent := 'This is page 1.';
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

{ TIndexWebModule }

function TIndexWebModule.DoCreatePage: THtmlPage;
begin
  Result := TSimpleHtmlPage.Create;
  try
    with Result as TSimpleHtmlPage do
    begin
      Title := 'Main Page';
      TextContent := 'Hello World!';
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;


{ TDefaultNavBar }

procedure TDefaultNavBar.DoCreateElements;
begin
  AddElement('Main Page', '~documentRoot;/index');
  AddElement('Page 1', '~documentRoot;/page1');
  AddElement('Page 2', '~documentRoot;/page2');
end;

{ TSimpleHtmlPage }

procedure TSimpleHtmlPage.AddFeatures;
begin
  inherited AddFeatures;
  AddFeature(THeaderFeature);
  AddFeature(TNavBarFeature);
  AddFeature(TContentFeature);
  AddFeature(TFooterFeature);
end;

procedure TSimpleHtmlPage.DoGetInnerContents(Strings: TIndentTaggedStrings);
begin
  Strings.Text := FTextContent;
end;

procedure TSimpleHtmlPage.DoSetVariables;
begin
  FNavBar.ActiveCaption := Title;
  inherited DoSetVariables;
end;

function TSimpleHtmlPage.GetNavBar: TNavBar;
begin
  Result := FNavBar;
end;

function TSimpleHtmlPage.QueryInterface(constref iid: tguid; out obj): longint; cdecl;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TSimpleHtmlPage._AddRef: longint; cdecl;
begin
  Result := -1;
end;

function TSimpleHtmlPage._Release: longint; cdecl;
begin
  Result := -1;
end;

procedure TSimpleHtmlPage.Clear;
begin
  inherited Clear;
  FNavBar.Clear;
end;

constructor TSimpleHtmlPage.Create;
begin
  inherited Create;
  FNavBar := TDefaultNavBar.Create(Self);
end;

destructor TSimpleHtmlPage.Destroy;
begin
  FreeAndNil(FNavBar);
  inherited Destroy;
end;

initialization
  RegisterHTTPModule('index', TIndexWebModule, True);
  RegisterHTTPModule('page1', TPage1WebModule, True);
  RegisterHTTPModule('page2', TPage2WebModule, True);

end.

