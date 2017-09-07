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
unit tswebhtmlpages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, htmlpages, serverconfig, LazFileUtils, htmlpreprocess,
  webstrconsts;

// TODO : Add users and login panel!
// TODO : Add navbar also!

type

  { TTesterPageFeature }

  TTesterPageFeature = class(THtmlPageFeature)
  protected
    function TemplateLocation(const TemplateName: string): string;
  end;

  { TPageBaseFeature }

  TPageBaseFeature = class(TTesterPageFeature)
  public
    procedure Satisfy; override;
    procedure DependsOn({%H-}ADependencies: THtmlPageFeatureList); override;
  end;

  { THeaderFeature }

  THeaderFeature = class(TTesterPageFeature)
  public
    procedure Satisfy; override;
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

  { TFooterFeature }

  TFooterFeature = class(TTesterPageFeature)
  public
    procedure Satisfy; override;
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

  { TTesterHtmlPage }

  TTesterHtmlPage = class(TFeaturedHtmlPage)
  private
    FTitle: string;
  protected
    procedure DoGetContents(Strings: TIndentTaggedStrings); virtual; abstract;
  public
    property Title: string read FTitle write FTitle;
  end;

var
  DocumentRoot: string = '/';
  DataRoot: string = '/data';

implementation

{ TFooterFeature }

procedure TFooterFeature.Satisfy;
begin
  with Parent do
  begin
    Variables.ItemsAsText['copyright'] := SCopyright;
    Variables.ItemsAsText['license'] := ;
    Preprocessor.PreprocessFileAndInsert(TemplateLocation('footer'), PageParts, 'footer');
  end;
end;

procedure TFooterFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  ADependencies.Add(TPageBaseFeature);
end;

{ THeaderFeature }

procedure THeaderFeature.Satisfy;
begin
  with Parent do
  begin
    Preprocessor.PreprocessFileAndInsert(TemplateLocation('header'), PageParts, 'header');
    // TODO : Add login panel here!
  end;
end;

procedure THeaderFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  ADependencies.Add(TPageBaseFeature);
end;

{ TPageBaseFeature }

procedure TPageBaseFeature.Satisfy;
begin
  with Parent.Variables do
  begin
    ItemsAsText['documentRoot'] := DocumentRoot;
    ItemsAsText['dataRoot'] := DataRoot;
    if Parent is TTesterHtmlPage then
    begin
      ItemsAsText['pageHeader'] := (Parent as TTesterHtmlPage).Title;
      ItemsAsText['title'] := (Parent as TTesterHtmlPage).Title;
    end;
  end;
end;

procedure TPageBaseFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  // do nothing
end;

{ TTesterPageFeature }

function TTesterPageFeature.TemplateLocation(const TemplateName: string): string;
begin
  Result := AppendPathDelim(Config.Location_TemplatesDir) + TemplateName + '.html';
end;

end.

