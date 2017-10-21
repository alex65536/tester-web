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
unit tswebsolvepages;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, tswebsolvefeatures, tswebsolveelements, webstrconsts, tswebmodules,
  tswebpages, htmlpreprocess, navbars, contests, tswebpagesbase, tswebnavbars,
  tswebeditableelements, tswebmanagers, editableobjects, users, userpages;

type

  { TSolveListPage }

  TSolveListPage = class(TDefaultHtmlPage)
  protected
    procedure AddFeatures; override;
    procedure DoGetInnerContents(Strings: TIndentTaggedStrings); override;
    function CreateNavBar: TNavBar; override;
  public
    procedure AfterConstruction; override;
  end;

  { TSolveContestPage }

  TSolveContestPage = class(TDefaultHtmlPage, IContestSolvePage)
  private
    FContest: TContest;
  protected
    procedure AddFeatures; override;
    function CreateNavBar: TNavBar; override;
    procedure DoUpdateRequest; override;
  public
    function Contest: TContest;
    function ContestName: string;
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TSolveContestPostPage }

  TSolveContestPostPage = class(TSolveContestPage, IPostHtmlPage)
  private
    FError: string;
    FSuccess: string;
  protected
    function GetError: string;
    procedure SetError(AValue: string);
    function GetSuccess: string;
    procedure SetSuccess(AValue: string);
  public
    property Error: string read GetError write SetError;
    property Success: string read GetSuccess write SetSuccess;
  end;

  { TSolveContestNavBar }

  TSolveContestNavBar = class(TTesterNavBar)
  protected
    procedure DoCreateElements; override;
    procedure DoFillVariables; override;
  public
    procedure AddNestedElement(const ACaption, ALink: string);
  end;

  { TSolveProblemListPage }

  TSolveProblemListPage = class(TSolveContestPage)
  protected
    procedure AddFeatures; override;
    procedure DoGetInnerContents(Strings: TIndentTaggedStrings); override;
  public
    procedure AfterConstruction; override;
  end;

  { TSolveContestProblemPage }

  TSolveContestProblemPage = class(TSolveContestPostPage, IContestProblemPage, IEditablePage)
  private
    FProblemIndex: integer;
    FTransaction: TTestContestTransaction;
  protected
    procedure AddFeatures; override;
    procedure DoGetInnerContents(Strings: TIndentTaggedStrings); override;
    function GetProblemIndex: integer;
    function EditableObject: TEditableObject;
    procedure DoUpdateRequest; override;
  public
    property ProblemIndex: integer read GetProblemIndex write FProblemIndex;
    property Transaction: TTestContestTransaction read FTransaction;
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TSolveSubmissionsPage }

  TSolveSubmissionsPage = class(TSolveContestPostPage)
  protected
    procedure AddFeatures; override;
    procedure DoGetInnerContents(Strings: TIndentTaggedStrings); override;
  public
    procedure AfterConstruction; override;
  end;

implementation

uses
  tswebsolvemodules;

{ TSolveSubmissionsPage }

procedure TSolveSubmissionsPage.AddFeatures;
begin
  inherited AddFeatures;
  AddFeature(TSolveSubmissionsFeature);
end;

procedure TSolveSubmissionsPage.DoGetInnerContents(Strings: TIndentTaggedStrings);
begin
  Strings.Text := '~+#solveSubmissions;';
end;

procedure TSolveSubmissionsPage.AfterConstruction;
begin
  inherited AfterConstruction;
  Title := SSolveSubmissionsTitle;
end;

{ TSolveContestProblemPage }

procedure TSolveContestProblemPage.AddFeatures;
begin
  inherited AddFeatures;
  AddFeature(TSolveContestProblemFeature);
end;

procedure TSolveContestProblemPage.DoGetInnerContents(Strings: TIndentTaggedStrings);
begin
  Strings.Text := '~+#solveContestProblem;';
end;

function TSolveContestProblemPage.GetProblemIndex: integer;
begin
  Result := FProblemIndex;
end;

function TSolveContestProblemPage.EditableObject: TEditableObject;
begin
  Result := Transaction.Problems[ProblemIndex];
end;

procedure TSolveContestProblemPage.DoUpdateRequest;
begin
  FreeAndNil(FTransaction);
  inherited DoUpdateRequest;
  FTransaction := Contest.CreateTestTransaction(User);
  Title := Format(SSolveProblemTitle, [ProblemIndex + 1]);
end;

constructor TSolveContestProblemPage.Create;
begin
  inherited Create;
  FTransaction := nil;
end;

destructor TSolveContestProblemPage.Destroy;
begin
  FreeAndNil(FTransaction);
  inherited Destroy;
end;

{ TSolveProblemListPage }

procedure TSolveProblemListPage.AddFeatures;
begin
  inherited AddFeatures;
  AddFeature(TSolveProblemListFeature);
end;

procedure TSolveProblemListPage.DoGetInnerContents(Strings: TIndentTaggedStrings);
begin
  Strings.Text := '~#solveProblemList;';
end;

procedure TSolveProblemListPage.AfterConstruction;
begin
  inherited AfterConstruction;
  Title := SSolveProblemListTitle;
end;

{ TSolveContestPage }

procedure TSolveContestPage.AddFeatures;
begin
  inherited AddFeatures;
  AddFeature(TSolveContestBaseFeature);
end;

function TSolveContestPage.CreateNavBar: TNavBar;
begin
  Result := TSolveContestNavBar.Create(Self);
end;

procedure TSolveContestPage.DoUpdateRequest;
begin
  inherited DoUpdateRequest;
  FreeAndNil(FContest);
  FContest := SolveContestFromRequest(Request);
end;

function TSolveContestPage.Contest: TContest;
begin
  Result := FContest;
end;

function TSolveContestPage.ContestName: string;
begin
  Result := SolveContestNameFromRequest(Request);
end;

constructor TSolveContestPage.Create;
begin
  inherited Create;
  FContest := nil;
end;

destructor TSolveContestPage.Destroy;
begin
  FreeAndNil(FContest);
  inherited Destroy;
end;

{ TSolveContestPostPage }

function TSolveContestPostPage.GetError: string;
begin
  Result := FError;
end;

procedure TSolveContestPostPage.SetError(AValue: string);
begin
  FError := AValue;
end;

function TSolveContestPostPage.GetSuccess: string;
begin
  Result := FSuccess;
end;

procedure TSolveContestPostPage.SetSuccess(AValue: string);
begin
  FSuccess := AValue;
end;

{ TSolveContestNavBar }

procedure TSolveContestNavBar.DoCreateElements;
var
  User: TUser;
  Transaction: TTestContestTransaction;
  I: integer;
begin
  // add common elements
  AddElement(SMainPage, '~documentRoot;/main');
  AddElement(SContestSolveTitle, '~documentRoot;/solve');
  // add contest-specific elements
  User := (Parent as TUserPage).User;
  Transaction := (Parent as IContestSolvePage).Contest.CreateTestTransaction(User);
  try
    AddSplitter;
    AddElement(SSolveProblemListTitle, '~documentRoot;/solve-contest~+contestParam;');
    // add submissions & standings pages
    AddNestedElement(SSolveSubmissionsTitle, '~documentRoot;/solve-submissions~+contestParam;');
    if Transaction.CanGetStandings then
      AddNestedElement(SSolveStandingsTitle, '~documentRoot;/solve-standings~+contestParam;');
    // add problem elements
    for I := 0 to Transaction.ProblemCount - 1 do
      AddNestedElement(Format(SSolveProblemTitle, [I + 1]),
        Format('~documentRoot;/solve-problem~+contestParam;&problem=%d', [I + 1]));
  finally
    FreeAndNil(Transaction);
  end;
end;

procedure TSolveContestNavBar.DoFillVariables;
begin
  Storage.ItemsAsText['contestParam'] := '?contest=' + (Parent as IContestSolvePage).ContestName;
  inherited DoFillVariables;
end;

procedure TSolveContestNavBar.AddNestedElement(const ACaption, ALink: string);
begin
  (AddElement(ACaption, ALink) as TTesterNavBarElement).ShowMiddleDot := True;
end;

{ TSolveListPage }

procedure TSolveListPage.AddFeatures;
begin
  inherited AddFeatures;
  AddFeature(TSolveContestListFeature);
end;

procedure TSolveListPage.DoGetInnerContents(Strings: TIndentTaggedStrings);
begin
  Strings.Text := '~#solveList;';
end;

function TSolveListPage.CreateNavBar: TNavBar;
begin
  Result := TDefaultNavBar.Create(Self);
end;

procedure TSolveListPage.AfterConstruction;
begin
  inherited AfterConstruction;
  Title := SContestSolveTitle;
end;

end.

