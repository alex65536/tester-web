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
unit errorpages;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, htmlpages, tswebpagesbase, tswebfeatures, htmlpreprocess, webstrconsts,
  HTTPDefs;

type

  { TErrorPageFeature }

  TErrorPageFeature = class(TTesterPageFeature)
  public
    procedure Satisfy; override;
  end;

  { TErrorHtmlPage }

  TErrorHtmlPage = class(TFeaturedHtmlPage)
  private
    FExceptObj: Exception;
  protected
    procedure DoGetSkeleton(Strings: TIndentTaggedStrings); override;
    procedure AddFeatures; override;
    procedure DoUpdateResponse; override;
  public
    property ExceptObj: Exception read FExceptObj write FExceptObj;
  end;

  TErrorHtmlPageClass = class of TErrorHtmlPage;

var
  TDefaultErrorPage: TErrorHtmlPageClass = TErrorHtmlPage;

implementation

{ TErrorPageFeature }

procedure TErrorPageFeature.Satisfy;

  procedure SetStackTrace;
  var
    Strings: TIndentTaggedStrings;
    FrameCount: integer;
    Frames: PCodePointer;
    I: integer;
  begin
    Strings := TIndentTaggedStrings.Create;
    try
      FrameCount := ExceptFrameCount;
      Frames := ExceptFrames;
      Strings.Add(Trim(BackTraceStrFunc(ExceptAddr)));
      for I := 0 to FrameCount - 1 do
        Strings.Add(Trim(BackTraceStrFunc(Frames[I])));
      Parent.Variables.SetItemAsStrings('stackTrace', Strings);
    finally
      FreeAndNil(Strings);
    end;
  end;

begin
  with Parent.Variables do
  begin
    ItemsAsText['pageHeader'] := SErrorTitle;
    ItemsAsText['title'] := SErrorTitle;
    with (Parent as TErrorHtmlPage).ExceptObj do
      ItemsAsText['errorMsg'] := Format(SErrorMsg, [ClassName, Message]);
    ItemsAsText['stackTraceMsg'] := SErrorStackTrace;
    SetStackTrace;
  end;
  LoadPagePart('', 'errorPage', 'content');
end;

{ TErrorHtmlPage }

procedure TErrorHtmlPage.DoGetSkeleton(Strings: TIndentTaggedStrings);
begin
  Strings.LoadFromFile(TemplateLocation('', 'skeleton'));
end;

procedure TErrorHtmlPage.AddFeatures;
begin
  inherited AddFeatures;
  AddFeature(THeaderFeature);
  AddFeature(TErrorPageFeature);
  AddFeature(TFooterFeature);
end;

procedure TErrorHtmlPage.DoUpdateResponse;
var
  ErrorCode: integer;
  ErrorText: string;
begin
  inherited DoUpdateResponse;
  // fill error code & text
  if ExceptObj is EHTTP then
  begin
    ErrorCode := (ExceptObj as EHTTP).StatusCode;
    ErrorText := (ExceptObj as EHTTP).StatusText;
  end
  else
  begin
    ErrorCode := ExceptObj.HelpContext;
    ErrorText := '';
  end;
  // if necessary, set defult values
  if ErrorCode = 0 then
    ErrorCode := 500;
  if ErrorText = '' then
    ErrorText := 'Application Error ' + ExceptObj.ClassName;
  // add them into response
  Response.Code := ErrorCode;
  Response.CodeText := ErrorText;
end;

end.

