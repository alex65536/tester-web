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
  tswebmodules, tswebpages, tswebmanagers, fphttp, navbars, htmlpages,
  htmlpreprocess, webstrconsts, tswebfeatures, tsmiscwebmodules, HTTPDefs;

type

  { TSubmissionHtmlPage }

  TSubmissionHtmlPage = class(TPostHtmlPage, IViewSubmissionPage)
  private
    FSubmission: TViewSubmission;
    FSubmissionSession: TProblemSubmissionSession;
  protected
    procedure DoGetInnerContents(Strings: TIndentTaggedStrings); override;
  public
    function Submission: TViewSubmission;
    function SubmissionSession: TProblemSubmissionSession;
    function CreateNavBar: TNavBar; override;
    procedure AddFeatures; override;
    constructor Create(ASubmission: TViewSubmission;
      ASubmissionSession: TProblemSubmissionSession); overload;
    procedure AfterConstruction; override;
  end;

  { TSubmissionViewWebModule }

  TSubmissionViewWebModule = class(TPostUserWebModule)
  private
    FSubmission: TViewSubmission;
    FSubmissionSession: TProblemSubmissionSession;
  protected
    property Submission: TViewSubmission read FSubmission;
    property SubmissionSession: TProblemSubmissionSession read FSubmissionSession;
    procedure DoHandlePost(ARequest: TRequest); override;
    function CanRedirect: boolean; override;
    function RedirectLocation: string; override;
    function DoCreatePage: THtmlPage; override;
    procedure DoSessionCreated; override;
    procedure DoAfterRequest; override;
  end;

implementation

{ TSubmissionViewWebModule }

procedure TSubmissionViewWebModule.DoHandlePost(ARequest: TRequest);
begin
  if ARequest.ContentFields.Values['query'] = 'rejudge' then
    SubmissionSession.RejudgeSubmission(Submission.ID);
end;

function TSubmissionViewWebModule.CanRedirect: boolean;
begin
  Result := True;
end;

function TSubmissionViewWebModule.RedirectLocation: string;
begin
  Result := Request.URI;
end;

function TSubmissionViewWebModule.DoCreatePage: THtmlPage;
begin
  Result := TSubmissionHtmlPage.Create(Submission, SubmissionSession);
end;

procedure TSubmissionViewWebModule.DoSessionCreated;
var
  SubmissionID: integer;
begin
  inherited DoSessionCreated;
  SubmissionID := StrToInt(Request.QueryFields.Values['id']);
  FSubmissionSession := ProblemManager.CreateSubmissionSession(User);
  FSubmission := SubmissionSession.GetSubmission(SubmissionID);
end;

procedure TSubmissionViewWebModule.DoAfterRequest;
begin
  FreeAndNil(FSubmission);
  FreeAndNil(FSubmissionSession);
  inherited DoAfterRequest;
end;

{ TSubmissionHtmlPage }

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
  AddFeature(TUserBarFeature);
end;

constructor TSubmissionHtmlPage.Create(ASubmission: TViewSubmission;
  ASubmissionSession: TProblemSubmissionSession);
begin
  inherited Create;
  FSubmission := ASubmission;
  FSubmissionSession := ASubmissionSession;
end;

procedure TSubmissionHtmlPage.AfterConstruction;
begin
  inherited AfterConstruction;
  Title := Format(SSubmissionPageCaption, [Submission.ID]);
end;

initialization
  RegisterHTTPModule('submissions', TSubmissionViewWebModule, True);

end.

