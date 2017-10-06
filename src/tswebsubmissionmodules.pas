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
unit tswebsubmissionmodules;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, tswebsubmissionelements, tswebsubmissionfeatures, submissions,
  tswebmodules, tswebpages, tswebmanagers, webmodules, fphttp, navbars,
  htmlpages, htmlpreprocess, webstrconsts;

type

  { TSubmissionHtmlPage }

  TSubmissionHtmlPage = class(TDefaultHtmlPage, IViewSubmissionPage)
  private
    FSubmission: TViewSubmission;
    FSubmissionSession: TProblemSubmissionSession;
  protected
    procedure DoUpdateRequest; override;
    procedure DoGetInnerContents(Strings: TIndentTaggedStrings); override;
  public
    function Submission: TViewSubmission;
    function SubmissionSession: TProblemSubmissionSession;
    function CreateNavBar: TNavBar; override;
    procedure AddFeatures; override;
    destructor Destroy; override;
  end;

  { TSubmissionViewWebModule }

  TSubmissionViewWebModule = class(THtmlPageWebModule)
  protected
    function DoCreatePage: THtmlPage; override;
  end;

implementation

{ TSubmissionViewWebModule }

function TSubmissionViewWebModule.DoCreatePage: THtmlPage;
begin
  Result := TSubmissionHtmlPage.Create;
end;

{ TSubmissionHtmlPage }

procedure TSubmissionHtmlPage.DoUpdateRequest;
var
  SubmissionID: integer;
begin
  inherited DoUpdateRequest;
  // free old submission & session
  FreeAndNil(FSubmission);
  FreeAndNil(FSubmissionSession);
  // get submission ID
  SubmissionID := StrToInt(Request.QueryFields.Values['id']);
  // get new submission & session
  FSubmissionSession := ProblemManager.CreateSubmissionSession(User);
  FSubmission := SubmissionSession.GetSubmission(SubmissionID);
  // fill the title
  Title := Format(SSubmissionPageCaption, [SubmissionID]);
end;

procedure TSubmissionHtmlPage.DoGetInnerContents(Strings: TIndentTaggedStrings);
begin
  Strings.Text := '~#+submissionView;';
end;

function TSubmissionHtmlPage.Submission: TViewSubmission;
begin
  Result := FSubmission;
end;

function TSubmissionHtmlPage.SubmissionSession: TProblemSubmissionSession;
begin
  Result := FSubmissionSession;
end;

function TSubmissionHtmlPage.CreateNavBar: TNavBar;
begin
  Result := TDefaultNavBar.Create(Self);
end;

procedure TSubmissionHtmlPage.AddFeatures;
begin
  inherited AddFeatures;
  AddFeature(TSubmissionViewPageFeature);
end;

destructor TSubmissionHtmlPage.Destroy;
begin
  FreeAndNil(FSubmission);
  FreeAndNil(FSubmissionSession);
  inherited Destroy;
end;

initialization
  RegisterHTTPModule('submissions', TSubmissionViewWebModule, True);

end.

