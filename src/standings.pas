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
unit standings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, typinfo, contestproblems, submissions, webstrconsts,
  datastorages, users, editableobjects, tswebobservers, fgl;

type
  TContestScoringPolicy = (spMaxScore, spLastScore);

  TStandingsManager = class;

  TDoubleList = specialize TFPGList<Double>;

  { TStandingsObject }

  TStandingsObject = class
  private
    FManager: TStandingsManager;
    FStorage: TAbstractDataStorage;
  protected
    property Storage: TAbstractDataStorage read FStorage;
    {%H-}constructor Create(AManager: TStandingsManager);
  public
    property Manager: TStandingsManager read FManager;
    constructor Create;
  end;

  { TStandingsLoadableObject }

  TStandingsLoadableObject = class(TStandingsObject)
  private
    FDataLoaded: boolean;
  protected
    procedure DoLoadData; virtual; abstract;
    {%H-}constructor Create(AManager: TStandingsManager);
  public
    procedure LoadData;
  end;

  { TStandingsRow }

  TStandingsRow = class(TStandingsLoadableObject)
  private
    FUserInfo: TUserInfo;
    function GetProblemScoreCount: integer;
    function GetProblemScores(I: integer): double;
    function GetSumScore: double;
  protected
    procedure DoLoadData; override;
    {%H-}constructor Create(AManager: TStandingsManager; AUserID: integer);
  public
    property UserInfo: TUserInfo read FUserInfo;
    property ProblemScores[I: integer]: double read GetProblemScores; default;
    property ProblemScoreCount: integer read GetProblemScoreCount;
    property SumScore: double read GetSumScore;
    destructor Destroy; override;
  end;

  TStandingsRowList = specialize TFPGObjectList<TStandingsRow>;

  { TStandingsTable }

  TStandingsTable = class(TStandingsLoadableObject)
  private
    function GetRowCount: integer;
    function GetRows(I: integer): TStandingsRow;
    function GetRowsByUsername(Username: string): TStandingsRow;
  protected
    procedure DoLoadData; override;
    {%H-}constructor Create(AManager: TStandingsManager);
  public
    property Rows[I: integer]: TStandingsRow read GetRows;
    property RowsByUsername[Username: string]: TStandingsRow read GetRowsByUsername;
      default;
    property RowCount: integer read GetRowCount;
    procedure SortByScore;
  end;

  { TStandingsContestHandler }

  TStandingsContestHandler = class(TStandingsObject)
  private
    FContest: TBaseContest;
  protected
    procedure RecalcCell(AProblem: TContestProblem; AInfo: TUserInfo); virtual;
    {%H-}constructor Create(AManager: TStandingsManager; AContest: TBaseContest);
  public
    property Contest: TBaseContest read FContest;
    procedure UserAdded(AInfo: TUserInfo); virtual;
    procedure UserDeleted(AInfo: TUserInfo); virtual;
    procedure ProblemAdded(AProblem: TContestProblem); virtual;
    procedure ProblemDeleted(AProblem: TContestProblem); virtual;
    procedure SubmissionTested(ASubmission: TTestSubmission); virtual;
    procedure ContestDeleted; virtual;
  end;

  { TStandingsManager }

  TStandingsManager = class(IMessageSubscriber)
  private
    FStorage: TAbstractDataStorage;
  protected
    property Storage: TAbstractDataStorage read FStorage;
    function DoCreateDataStorage: TAbstractDataStorage; virtual;
    procedure MessageReceived(AMessage: TAuthorMessage);
    function GetStandings(AContest: TBaseContest): TStandingsTable; virtual;
  public
    function SubmissionManager: TSubmissionManager; virtual; abstract;
    function ContestManager: TBaseContestManager; virtual; abstract;
    constructor Create;
    destructor Destroy; override;
  end;

  { TBaseStandingsContest }

  TBaseStandingsContest = class(TBaseContest)
  private
    function GetStandingsTable: TStandingsTable;
  protected
    property StandingsTable: TStandingsTable read GetStandingsTable;
    {%H-}constructor Create(const AName: string; AManager: TEditableManager);
  public
    destructor Destroy; override;
  end;

function ScoringPolicyToStr(APolicy: TContestScoringPolicy): string;
function StrToScoringPolicy(const S: string): TContestScoringPolicy;

implementation

function ScoringPolicyToStr(APolicy: TContestScoringPolicy): string;
begin
  Result := GetEnumName(TypeInfo(APolicy), Ord(APolicy));
end;

function StrToScoringPolicy(const S: string): TContestScoringPolicy;
var
  P: TContestScoringPolicy;
begin
  for P in TContestScoringPolicy do
    if ScoringPolicyToStr(P) = S then
      Exit(P);
  raise EConvertError.Create(SNoSuchScoringPolicy);
end;

{ TBaseStandingsContest }

function TBaseStandingsContest.GetStandingsTable: TStandingsTable;
begin

end;

constructor TBaseStandingsContest.Create(const AName: string;
  AManager: TEditableManager);
begin

end;

destructor TBaseStandingsContest.Destroy;
begin
  inherited Destroy;
end;

{ TStandingsManager }

function TStandingsManager.DoCreateDataStorage: TAbstractDataStorage;
begin

end;

procedure TStandingsManager.MessageReceived(AMessage: TAuthorMessage);
begin

end;

function TStandingsManager.GetStandings(AContest: TBaseContest): TStandingsTable;
begin

end;

constructor TStandingsManager.Create;
begin

end;

destructor TStandingsManager.Destroy;
begin

end;

{ TStandingsContestHandler }

procedure TStandingsContestHandler.RecalcCell(AProblem: TContestProblem;
  AInfo: TUserInfo);
begin

end;

constructor TStandingsContestHandler.Create(AManager: TStandingsManager;
  AContest: TBaseContest);
begin

end;

procedure TStandingsContestHandler.UserAdded(AInfo: TUserInfo);
begin

end;

procedure TStandingsContestHandler.UserDeleted(AInfo: TUserInfo);
begin

end;

procedure TStandingsContestHandler.ProblemAdded(AProblem: TContestProblem);
begin

end;

procedure TStandingsContestHandler.ProblemDeleted(AProblem: TContestProblem);
begin

end;

procedure TStandingsContestHandler.SubmissionTested(ASubmission: TTestSubmission);
begin

end;

procedure TStandingsContestHandler.ContestDeleted;
begin

end;

{ TStandingsTable }

function TStandingsTable.GetRowCount: integer;
begin

end;

function TStandingsTable.GetRows(I: integer): TStandingsRow;
begin

end;

function TStandingsTable.GetRowsByUsername(Username: string): TStandingsRow;
begin

end;

procedure TStandingsTable.DoLoadData;
begin

end;

constructor TStandingsTable.Create(AManager: TStandingsManager);
begin

end;

procedure TStandingsTable.SortByScore;
begin

end;

{ TStandingsRow }

function TStandingsRow.GetProblemScoreCount: integer;
begin

end;

function TStandingsRow.GetProblemScores(I: integer): double;
begin

end;

function TStandingsRow.GetSumScore: double;
begin

end;

procedure TStandingsRow.DoLoadData;
begin

end;

constructor TStandingsRow.Create(AManager: TStandingsManager; AUserID: integer);
begin

end;

destructor TStandingsRow.Destroy;
begin
  inherited Destroy;
end;

{ TStandingsLoadableObject }

constructor TStandingsLoadableObject.Create(AManager: TStandingsManager);
begin

end;

procedure TStandingsLoadableObject.LoadData;
begin

end;

{ TStandingsObject }

constructor TStandingsObject.Create(AManager: TStandingsManager);
begin

end;

constructor TStandingsObject.Create;
begin

end;

end.

