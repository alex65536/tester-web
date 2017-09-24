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
unit tswebproblemfeatures;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, tswebeditablefeatures, webstrconsts, htmlpages,
  tswebfeatures, serverconfig, problems;

type

  { TProblemBaseFeature }

  TProblemBaseFeature = class(TTesterPageFeature)
  public
    procedure Satisfy; override;
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

  { TProblemCreateFormFeature }

  TProblemCreateFormFeature = class(TTesterPageFeature)
  public
    procedure Satisfy; override;
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

  { TProblemEditViewBaseFeature }

  TProblemEditViewBaseFeature = class(TTesterPageFeature)
  public
    procedure Satisfy; override;
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

  { TProblemEditFeature }

  TProblemEditFeature = class(TEditableTransactionPageFeature)
  protected
    procedure InternalSatisfy; override;
  public
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

implementation

{ TProblemEditViewBaseFeature }

procedure TProblemEditViewBaseFeature.Satisfy;
begin
  with Parent.Variables do
  begin
    ItemsAsText['problemArchiveKey'] := SProblemArchiveKey;
    ItemsAsText['problemStatementsKey'] := SProblemStatementsKey;
    ItemsAsText['problemStatementsTypeKey'] := SProblemStatementsTypeKey;
    ItemsAsText['problemMaxSrcKey'] := SProblemMaxSrcKey;
  end;
end;

procedure TProblemEditViewBaseFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TEditableEditViewBaseFeature);
end;

{ TProblemEditFeature }

procedure TProblemEditFeature.InternalSatisfy;
begin
  with Parent.Variables, Transaction as TProblemTransaction do
  begin
    ItemsAsText['objectEditSubmit'] := SProblemEditSubmit;
    ItemsAsText['archiveMaxSize'] := IntToStr(Config.Files_MaxArchiveSize);
    ItemsAsText['problemStatementsHtml'] := SProblemStatementsHtml;
    ItemsAsText['problemStatementsPdf'] := SProblemStatementsPdf;
    case StatementsType of
      stHtml: ItemsAsText['stHtmlSelected'] := ' selected';
      stPdf: ItemsAsText['stPdfSelected'] := ' selected';
    end;
    ItemsAsText['statementsMaxSize'] := IntToStr(Config.Files_MaxStatementsSize);
    ItemsAsText['srcMaxSize'] := IntToStr(Config.Files_MaxSrcSize);
    ItemsAsText['srcDefaultSize'] := IntToStr(Config.Files_DefaultSrcSize);
  end;
  LoadPagePart('problem', 'problemEdit', 'objectEditContent');
end;

procedure TProblemEditFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TProblemEditViewBaseFeature);
  ADependencies.Add(TEditableEditFeature);
end;

{ TProblemCreateFormFeature }

procedure TProblemCreateFormFeature.Satisfy;
begin
  with Parent.Variables do
  begin
    ItemsAsText['editableCreateNamePrompt'] := SProblemCreateNamePrompt;
    ItemsAsText['editableCreateTitlePrompt'] := SProblemCreateTitlePrompt;
    ItemsAsText['editableCreatePrompt'] := SProblemCreatePrompt;
  end;
end;

procedure TProblemCreateFormFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TEditableCreateFormFeature);
end;

{ TProblemBaseFeature }

procedure TProblemBaseFeature.Satisfy;
begin
  with Parent.Variables do
  begin
    ItemsAsText['editableNewRef'] := 'problem-new';
    ItemsAsText['editableNodeDeleteRef'] := 'problem-delete';
    ItemsAsText['editableViewRef'] := 'problem-view';
    ItemsAsText['editableEditRef'] := 'problem-edit';
    ItemsAsText['editableAccessRef'] := 'problem-access';
  end;
end;

procedure TProblemBaseFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TEditableBaseFeature);
end;

end.

