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

{$mode objfpc}{$H+}{$inline on}

interface

uses
  Classes, SysUtils, typinfo, contestproblems, submissions, webstrconsts,
  datastorages, users, editableobjects, tswebobservers, fgl, tswebutils, math;

const
  ScoreCompareEps = 1e-7;

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
    procedure ReloadData;
  end;

  TStandingsTable = class;

  { TStandingsRow }

  TStandingsRow = class(TStandingsLoadableObject)
  private
    FContest: TBaseContest;
    FMaxPlace: integer;
    FMinPlace: integer;
    FTable: TStandingsTable;
    FUserInfo: TUserInfo;
    FList: TDoubleList;
    FSumScore: double;
    function GetProblemScoreCount: integer;
    function GetProblemScores(I: integer): double;
    function GetSumScore: double;
  protected
    procedure DoLoadData; override;
    {%H-}constructor Create(AManager: TStandingsManager; ATable: TStandingsTable;
      AUserID: integer);
  public
    property MinPlace: integer read FMinPlace write FMinPlace;
    property MaxPlace: integer read FMaxPlace write FMaxPlace;
    property Contest: TBaseContest read FContest;
    property Table: TStandingsTable read FTable;
    property UserInfo: TUserInfo read FUserInfo;
    property ProblemScores[I: integer]: double read GetProblemScores; default;
    property ProblemScoreCount: integer read GetProblemScoreCount;
    property SumScore: double read GetSumScore;
    destructor Destroy; override;
  end;

  { TStandingsRowList }

  TStandingsRowList = class(specialize TFPGObjectList<TStandingsRow>)
  public
    procedure Sort;
  end;

  { TStandingsTable }

  TStandingsTable = class(TStandingsLoadableObject)
  private
    FList: TStandingsRowList;
    FContest: TBaseContest;
    function GetRowCount: integer;
    function GetRows(I: integer): TStandingsRow;
    function GetRowsByUsername(Username: string): TStandingsRow;
  protected
    procedure DoLoadData; override;
    procedure FillRowsPlace;
    {%H-}constructor Create(AManager: TStandingsManager; AContest: TBaseContest);
  public
    property Contest: TBaseContest read FContest;
    property Rows[I: integer]: TStandingsRow read GetRows;
    property RowsByUsername[Username: string]: TStandingsRow read GetRowsByUsername;
      default;
    property RowCount: integer read GetRowCount;
    procedure SortByScore;
    procedure RecalcTable;
    destructor Destroy; override;
  end;

  { TStandingsContestHandler }

  TStandingsContestHandler = class(TStandingsObject)
  private
    FContest: TBaseContest;
  protected
    function ContestSectionName: string;
    function RowSectionName(AInfo: TUserInfo): string;
    function ColumnSectionName(AInfo: TUserInfo; AProblem: TContestProblem): string;
    procedure DoRecalcCell(AInfo: TUserInfo; AProblem: TContestProblem); virtual;
    procedure DoRecalcRow(AInfo: TUserInfo);
    procedure DoRecalcCol(AProblem: TContestProblem);
    {%H-}constructor Create(AManager: TStandingsManager; AContest: TBaseContest);
  public
    property Contest: TBaseContest read FContest;
    procedure UserAdded(AInfo: TUserInfo); virtual;
    procedure UserDeleted(AInfo: TUserInfo); virtual;
    procedure ProblemAdded(AProblem: TContestProblem); virtual;
    procedure ProblemDeleted(AProblem: TContestProblem); virtual;
    procedure SubmissionTested(ASubmission: TTestSubmission); virtual;
    procedure ContestDeleted; virtual;
    procedure RecalcEntireTable;
  end;

  { TStandingsManager }

  TStandingsManager = class(IMessageSubscriber)
  private
    FStorage: TAbstractDataStorage;
  protected
    property Storage: TAbstractDataStorage read FStorage;
    function TablesSectionName: string;
    function ContestSectionName(AContest: TBaseContest): string;
    function RowSectionName(AContest: TBaseContest; AInfo: TUserInfo): string;
    function ColumnSectionName(AContest: TBaseContest; AInfo: TUserInfo;
      AProblem: TContestProblem): string;
    function DoCreateDataStorage: TAbstractDataStorage; virtual;
    procedure MessageReceived(AMessage: TAuthorMessage);
    function GetStandings(AContest: TBaseContest): TStandingsTable; virtual;
  public
    function SubmissionManager: TContestSubmissionManager; virtual; abstract;
    function ContestManager: TBaseContestManager; virtual; abstract;
    constructor Create;
    destructor Destroy; override;
  end;

  { TBaseStandingsContest }

  TBaseStandingsContest = class(TBaseContest)
  private
    FStandingsTable: TStandingsTable;
    function GetStandingsTable: TStandingsTable;
  protected
    property StandingsTable: TStandingsTable read GetStandingsTable;
    function ContestScoringPolicy: TContestScoringPolicy; virtual; abstract;
    {%H-}constructor Create(const AName: string; AManager: TEditableManager);
  public
    function StandingsManager: TStandingsManager;
    destructor Destroy; override;
  end;

  { TBaseStandingsContestManager }

  TBaseStandingsContestManager = class(TBaseContestManager)
  public
    function StandingsManager: TStandingsManager; virtual; abstract;
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

function StandingsRowCompare(const ARow1, ARow2: TStandingsRow): integer;
begin
  Result := -CompareValue(ARow1.SumScore, ARow2.SumScore, ScoreCompareEps);
end;

{ TStandingsRowList }

procedure TStandingsRowList.Sort;
begin
  inherited Sort(@StandingsRowCompare);
end;

{ TBaseStandingsContest }

function TBaseStandingsContest.GetStandingsTable: TStandingsTable;
begin
  if FStandingsTable = nil then
    FStandingsTable := StandingsManager.GetStandings(Self);
  Result := FStandingsTable;
end;

constructor TBaseStandingsContest.Create(const AName: string;
  AManager: TEditableManager);
begin
  inherited Create(AName, AManager);
  FStandingsTable := nil;
end;

function TBaseStandingsContest.StandingsManager: TStandingsManager;
begin
  Result := (Manager as TBaseStandingsContestManager).StandingsManager;
end;

destructor TBaseStandingsContest.Destroy;
begin
  FreeAndNil(FStandingsTable);
  inherited Destroy;
end;

{ TStandingsManager }

function TStandingsManager.TablesSectionName: string;
begin
  Result := 'tables';
end;

function TStandingsManager.ContestSectionName(AContest: TBaseContest): string;
begin
  Result := TablesSectionName + '.' + Id2Str(AContest.ID);
end;

function TStandingsManager.RowSectionName(AContest: TBaseContest;
  AInfo: TUserInfo): string;
begin
  Result := ContestSectionName(AContest) + '.' + Id2Str(AInfo.ID);
end;

function TStandingsManager.ColumnSectionName(AContest: TBaseContest;
  AInfo: TUserInfo; AProblem: TContestProblem): string;
begin
  Result := RowSectionName(AContest, AInfo) + '.' + Id2Str(AProblem.ID);
end;

function TStandingsManager.DoCreateDataStorage: TAbstractDataStorage;
begin
  Result := TXmlDataStorage.Create('standings');
end;

procedure TStandingsManager.MessageReceived(AMessage: TAuthorMessage);
var
  Submission: TTestSubmission;
  Problem: TContestProblem;
  Contest: TBaseContest;
  Handler: TStandingsContestHandler;
begin
  // init everything carefully
  Problem := nil;
  Contest := nil;
  Submission := nil;
  try
    // retrieve contest (and submission, if needed)
    if AMessage is TSubmissionTestedMessage then
    begin
      Submission := (AMessage as TSubmissionTestedMessage).Submission;
      Problem := Submission.Problem as TContestProblem;
      Contest := Problem.Contest;
    end
    else if (AMessage is TEditableObjectMessage) and (AMessage.Sender = ContestManager) then
      Contest := (AMessage as TEditableObjectMessage).EditableObject as TBaseContest;
    if Contest = nil then
      Exit;
    // create handler
    Handler := TStandingsContestHandler.Create(Self, Contest);
    try
      // now, handle everything we want
      if AMessage is TSubmissionTestedMessage then
        Handler.SubmissionTested(Submission)
      else if AMessage is TEditableDeletingMessage then
        Handler.ContestDeleted
      else if AMessage is TContestParticipantAddedMessage then
        Handler.UserAdded((AMessage as TContestParticipantAddedMessage).UserInfo)
      else if AMessage is TContestParticipantDeletedMessage then
        Handler.UserDeleted((AMessage as TContestParticipantDeletedMessage).UserInfo)
      else if AMessage is TContestProblemAddedMessage then
        Handler.ProblemAdded((AMessage as TContestProblemAddedMessage).Problem)
      else if AMessage is TContestProblemDeletedMessage then
        Handler.ProblemDeleted((AMessage as TContestProblemDeletedMessage).Problem);
    finally
      FreeAndNil(Handler);
    end;
  finally
    FreeAndNil(Problem);
  end;
end;

function TStandingsManager.GetStandings(AContest: TBaseContest): TStandingsTable;
begin
  Result := TStandingsTable.Create(Self, AContest);
end;

constructor TStandingsManager.Create;
begin
  inherited Create;
  FStorage := DoCreateDataStorage;
  ContestManager.Subscribe(Self);
  SubmissionManager.Subscribe(Self);
end;

destructor TStandingsManager.Destroy;
begin
  SubmissionManager.Unsubscribe(Self);
  ContestManager.Unsubscribe(Self);
  FreeAndNil(FStorage);
  inherited Destroy;
end;

{ TStandingsContestHandler }

function TStandingsContestHandler.ContestSectionName: string;
begin
  Result := Manager.ContestSectionName(Contest);
end;

function TStandingsContestHandler.RowSectionName(AInfo: TUserInfo): string;
begin
  Result := Manager.RowSectionName(Contest, AInfo);
end;

function TStandingsContestHandler.ColumnSectionName(AInfo: TUserInfo;
  AProblem: TContestProblem): string;
begin
  Result := Manager.ColumnSectionName(Contest, AInfo, AProblem);
end;

procedure TStandingsContestHandler.DoRecalcCell(AInfo: TUserInfo;
  AProblem: TContestProblem);
var
  Submission: TContestViewSubmission;
  BestID: integer;
  BestScore: double;

  function BetterSubmissionFound: boolean;
  begin
    Result := False;
    case (Contest as TBaseStandingsContest).ContestScoringPolicy of
      spMaxScore: Result := Submission.Score > BestScore;
      spLastScore: Result := Submission.ID > BestID;
    end;
  end;

var
  SubmissionIds: TIdList;
  ID: integer;
begin
  SubmissionIds := Manager.SubmissionManager.ListByContestProblemUser(AProblem,
    AInfo);
  try
    BestID := -1;
    BestScore := 0.0;
    for ID in SubmissionIds do
    begin
      Submission := Manager.SubmissionManager.GetSubmission(ID) as TContestViewSubmission;
      try
        if Submission.Rated then
          if BetterSubmissionFound then
          begin
            BestID := Submission.ID;
            BestScore := Submission.Score;
          end;
      finally
        FreeAndNil(Submission);
      end;
    end;
    Storage.WriteFloat(ColumnSectionName(AInfo, AProblem) + '.score', BestScore);
  finally
    FreeAndNil(SubmissionIds);
  end;
end;

procedure TStandingsContestHandler.DoRecalcRow(AInfo: TUserInfo);
var
  I: integer;
  Problem: TContestProblem;
begin
  for I := 0 to Contest.ContestProblemCount - 1 do
  begin
    Problem := Contest.ContestProblem(I);
    try
      DoRecalcCell(AInfo, Problem);
    finally
      FreeAndNil(Problem);
    end;
  end;
end;

procedure TStandingsContestHandler.DoRecalcCol(AProblem: TContestProblem);
var
  UserList: TStringList;
  Username: string;
  Info: TUserInfo;
begin
  UserList := Contest.ListParticipants;
  try
    for Username in UserList do
    begin
      Info := UserManager.GetUserInfo(Username);
      try
        DoRecalcCell(Info, AProblem);
      finally
        FreeAndNil(Info);
      end;
    end;
  finally
    FreeAndNil(UserList);
  end;
end;

constructor TStandingsContestHandler.Create(AManager: TStandingsManager;
  AContest: TBaseContest);
begin
  inherited Create(AManager);
  FContest := AContest;
end;

procedure TStandingsContestHandler.UserAdded(AInfo: TUserInfo);
begin
  DoRecalcRow(AInfo);
end;

procedure TStandingsContestHandler.UserDeleted(AInfo: TUserInfo);
begin
  Storage.DeletePath(RowSectionName(AInfo));
end;

procedure TStandingsContestHandler.ProblemAdded(AProblem: TContestProblem);
begin
  DoRecalcCol(AProblem);
end;

procedure TStandingsContestHandler.ProblemDeleted(AProblem: TContestProblem);
var
  UserList: TStringList;
  Username: string;
  Info: TUserInfo;
begin
  UserList := Contest.ListParticipants;
  try
    for Username in UserList do
    begin
      Info := UserManager.GetUserInfo(Username);
      try
        Storage.DeletePath(ColumnSectionName(Info, AProblem));
      finally
        FreeAndNil(Info);
      end;
    end;
  finally
    FreeAndNil(UserList);
  end;
end;

procedure TStandingsContestHandler.SubmissionTested(ASubmission: TTestSubmission);
var
  Info: TUserInfo;
  Problem: TContestProblem;
begin
  Info := ASubmission.Owner;
  try
    Problem := ASubmission.Problem as TContestProblem;
    try
      DoRecalcCell(Info, Problem);
    finally
      FreeAndNil(Problem);
    end;
  finally
    FreeAndNil(Info);
  end;
end;

procedure TStandingsContestHandler.ContestDeleted;
begin
  Storage.DeletePath(ContestSectionName);
end;

procedure TStandingsContestHandler.RecalcEntireTable;
var
  UserList: TStringList;
  Username: string;
  Info: TUserInfo;
begin
  UserList := Contest.ListParticipants;
  try
    for Username in UserList do
    begin
      Info := UserManager.GetUserInfo(Username);
      try
        DoRecalcRow(Info);
      finally
        FreeAndNil(Info);
      end;
    end;
  finally
    FreeAndNil(UserList);
  end;
end;

{ TStandingsTable }

function TStandingsTable.GetRowCount: integer;
begin
  LoadData;
  Result := FList.Count;
end;

function TStandingsTable.GetRows(I: integer): TStandingsRow;
begin
  LoadData;
  Result := FList[I];
end;

function TStandingsTable.GetRowsByUsername(Username: string): TStandingsRow;
var
  I: integer;
begin
  LoadData;
  Result := nil;
  for I := 0 to FList.Count - 1 do
    if FList[I].UserInfo.Username = Username then
      Exit(FList[I]);
end;

procedure TStandingsTable.DoLoadData;
var
  Participants: TStringList;
  Username: string;
begin
  FList := TStandingsRowList.Create(True);
  try
    Participants := Contest.ListParticipants;
    try
      for Username in Participants do
        FList.Add(TStandingsRow.Create(Manager, Self, UserManager.UsernameToId(Username)));
    finally
      FreeAndNil(Participants);
    end;
  except
    FreeAndNil(FList);
    raise;
  end;
end;

procedure TStandingsTable.FillRowsPlace;

  function RowsEqual(A, B: integer): boolean; inline;
  begin
    Result := CompareValue(FList[A].SumScore, FList[B].SumScore, ScoreCompareEps) = 0;
  end;

var
  WasIndex, Index, I: integer;
begin
  Index := 0;
  while Index < FList.Count do
  begin
    WasIndex := Index;
    while (Index < FList.Count) and RowsEqual(Index, WasIndex) do
      Inc(Index);
    for I := WasIndex to Index - 1 do
    begin
      FList[I].MinPlace := WasIndex;
      FList[I].MaxPlace := Index - 1;
    end;
  end;
end;

constructor TStandingsTable.Create(AManager: TStandingsManager;
  AContest: TBaseContest);
begin
  inherited Create(AManager);
  FContest := AContest;
  FList := nil;
end;

procedure TStandingsTable.SortByScore;
begin
  LoadData;
  FList.Sort;
  FillRowsPlace;
end;

procedure TStandingsTable.RecalcTable;
var
  Handler: TStandingsContestHandler;
begin
  Handler := TStandingsContestHandler.Create(Manager, Contest);
  try
    Handler.RecalcEntireTable;
  finally
    FreeAndNil(Handler);
  end;
  ReloadData;
end;

destructor TStandingsTable.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

{ TStandingsRow }

function TStandingsRow.GetProblemScoreCount: integer;
begin
  LoadData;
  Result := FList.Count;
end;

function TStandingsRow.GetProblemScores(I: integer): double;
begin
  LoadData;
  Result := FList[I];
end;

function TStandingsRow.GetSumScore: double;
begin
  LoadData;
  Result := FSumScore;
end;

procedure TStandingsRow.DoLoadData;
var
  I: integer;
  ScoreKey: string;
  Score: double;
  Problem: TContestProblem;
begin
  FSumScore := 0.0;
  FList := TDoubleList.Create;
  try
    for I := 0 to Contest.ContestProblemCount - 1 do
    begin
      Problem := Contest.ContestProblem(I);
      try
        ScoreKey := Manager.ColumnSectionName(Contest, UserInfo, Problem) + '.score';
        Score := Storage.ReadFloat(ScoreKey, 0.0);
        FList.Add(Score);
        FSumScore := FSumScore + Score;
      finally
        FreeAndNil(Problem);
      end;
    end;
  except
    FreeAndNil(FList);
    raise;
  end;
end;

constructor TStandingsRow.Create(AManager: TStandingsManager;
  ATable: TStandingsTable; AUserID: integer);
begin
  inherited Create(AManager);
  FTable := ATable;
  FContest := FTable.Contest;
  FUserInfo := UserManager.GetUserInfo(AUserID);
  FList := nil;
  FSumScore := 0.0;
  FMinPlace := -1;
  FMaxPlace := -1;
end;

destructor TStandingsRow.Destroy;
begin
  FreeAndNil(FList);
  FreeAndNil(FUserInfo);
  inherited Destroy;
end;

{ TStandingsLoadableObject }

constructor TStandingsLoadableObject.Create(AManager: TStandingsManager);
begin
  inherited Create(AManager);
  FDataLoaded := False;
end;

procedure TStandingsLoadableObject.LoadData;
begin
  if FDataLoaded then
    Exit;
  DoLoadData;
  FDataLoaded := True;
end;

procedure TStandingsLoadableObject.ReloadData;
begin
  if FDataLoaded then
  begin
    FDataLoaded := False;
    LoadData;
  end;
end;

{ TStandingsObject }

constructor TStandingsObject.Create(AManager: TStandingsManager);
begin
  inherited Create;
  FManager := AManager;
  FStorage := AManager.Storage;
end;

constructor TStandingsObject.Create;
begin
  // we don't want all standings objects to be created publicly!
  raise EInvalidOperation.CreateFmt(SCreationPublic, [ClassName]);
end;

end.

