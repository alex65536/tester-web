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
unit tswebpagesbase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, htmlpages, htmlpreprocess, LazFileUtils, serverconfig;

type

  { TTesterHtmlPage }

  TTesterHtmlPage = class(TFeaturedHtmlPage)
  private
    FTitle: string;
  protected
    procedure DoGetInnerContents(Strings: TIndentTaggedStrings); virtual; abstract;
    procedure DoGetSkeleton(Strings: TIndentTaggedStrings); override;
  public
    property Title: string read FTitle write FTitle;
    procedure GetInnerContents(Strings: TIndentTaggedStrings);
  end;

var
  DocumentRoot: string = '/';
  DataRoot: string = '/data';

function TemplateLocation(const TemplateName: string): string;

implementation

function TemplateLocation(const TemplateName: string): string;
begin
  Result := AppendPathDelim(Config.Location_TemplatesDir) + TemplateName + '.html';
end;

{ TTesterHtmlPage }

procedure TTesterHtmlPage.DoGetSkeleton(Strings: TIndentTaggedStrings);
begin
  Strings.LoadFromFile(TemplateLocation('skeleton'));
end;

procedure TTesterHtmlPage.GetInnerContents(Strings: TIndentTaggedStrings);
begin
  DoGetInnerContents(Strings);
end;

end.

