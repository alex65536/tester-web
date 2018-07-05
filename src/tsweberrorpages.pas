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
unit tsweberrorpages;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, tswebpagesbase, tswebfeatures, htmlpreprocess, webstrconsts,
  errorpages, tswebmodules, tswebutils;

type

  { TErrorPageFeature }

  TErrorPageFeature = class(TTesterPageFeature)
  public
    procedure Satisfy; override;
  end;

  { TTesterErrorPage }

  TTesterErrorPage = class(TErrorHtmlPage)
  protected
    procedure AddFeatures; override;
    procedure DoGetSkeleton(Strings: TIndentTaggedStrings); override;
  end;

implementation

{ TTesterErrorPage }

procedure TTesterErrorPage.AddFeatures;
begin
  inherited AddFeatures;
  AddFeature(THeaderFeature);
  AddFeature(TErrorPageFeature);
  AddFeature(TFooterFeature);
end;

procedure TTesterErrorPage.DoGetSkeleton(Strings: TIndentTaggedStrings);
begin
  Strings.LoadFromFile(TemplateLocation('', 'skeleton'));
end;

{ TErrorPageFeature }

procedure TErrorPageFeature.Satisfy;
begin
  with Parent.Variables do
  begin
    ItemsAsText['pageHeader'] := SErrorTitle;
    ItemsAsText['title'] := SErrorTitle;
    with (Parent as TErrorHtmlPage).ExceptObj do
      ItemsAsText['errorMsg'] := Format(SErrorMsg, [ClassName, Message]);
    ItemsAsText['stackTraceMsg'] := SErrorStackTrace;
    ItemsAsText['stackTrace'] := DumpBackTrace;
  end;
  LoadPagePart('', 'errorPage', 'content');
end;

initialization
  TDefaultErrorPage := TTesterErrorPage;

end.

