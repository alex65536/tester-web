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
unit tswebsubmissionelements;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, submissionlanguages, tswebpagesbase, htmlpages,
  htmlpreprocess;

type

  { TSubmissionLanguageItem }

  TSubmissionLanguageItem = class(TTesterHtmlPageElement)
  private
    FLanguage: TSubmissionLanguage;
  protected
    procedure DoFillVariables; override;
    procedure DoGetSkeleton(Strings: TIndentTaggedStrings); override;
  public
    property Language: TSubmissionLanguage read FLanguage;
    constructor Create(AParent: THtmlPage; ALanguage: TSubmissionLanguage);
  end;

  { TSubmissionLanguageItemList }

  TSubmissionLanguageItemList = class(TTesterHtmlListedPageElement)
  protected
    procedure DoFillVariables; override;
    procedure DoGetSkeleton(Strings: TIndentTaggedStrings); override;
  public
    constructor Create(AParent: THtmlPage);
  end;

implementation

{ TSubmissionLanguageItemList }

procedure TSubmissionLanguageItemList.DoFillVariables;
begin
  AddListToVariable('problemLanguageItems');
end;

procedure TSubmissionLanguageItemList.DoGetSkeleton(Strings: TIndentTaggedStrings);
begin
  Strings.LoadFromFile(TemplateLocation('problem', 'problemLanguageList'));
end;

constructor TSubmissionLanguageItemList.Create(AParent: THtmlPage);
var
  L: TSubmissionLanguage;
begin
  inherited Create(AParent);
  for L in TSubmissionLanguage do
    List.Add(TSubmissionLanguageItem.Create(AParent, L));
end;

{ TSubmissionLanguageItem }

procedure TSubmissionLanguageItem.DoFillVariables;
begin
  Storage.ItemsAsText['problemLanguageOptionName'] := LanguageToStr(Language);
  Storage.ItemsAsText['problemLanguageOptionText'] := LanguageFullCompilerNames(Language);
end;

procedure TSubmissionLanguageItem.DoGetSkeleton(Strings: TIndentTaggedStrings);
begin
  Strings.LoadFromFile(TemplateLocation('problem', 'problemLanguageOption'));
end;

constructor TSubmissionLanguageItem.Create(AParent: THtmlPage; ALanguage: TSubmissionLanguage);
begin
  inherited Create(AParent);
  FLanguage := ALanguage;
end;

end.

