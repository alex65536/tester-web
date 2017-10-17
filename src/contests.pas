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
unit contests;

// TODO : Finish implementing contest system !!!

{$mode objfpc}{$H+}{$M+}

interface

uses
  Classes, SysUtils, editableobjects, webstrconsts, datastorages, users,
  tswebutils, dateutils, serverconfig, contestproblems, standings, tswebobservers;

type
  EContestAction = class(EEditableAction);
  EContestValidate = class(EEditableValidate);
  EContestAccessDenied = class(EEditableAccessDenied);

  TContest = class;
  TContestManager = class;

  { TContestAccessSession }

  TContestAccessSession = class(TEditableObjectAccessSession)
  protected
    {%H-}constructor Create(AManager: TEditableManager; AUser: TUser;
      AObject: TEditableObject);
  end;

  { TContestParticipantSession }

  TContestParticipantSession = class(TEditableObjectSession)
  private
    FContest: TContest;
  protected
    {%H-}constructor Create(AManager: TEditableManager; AUser: TUser;
      AObject: TEditableObject);
  public
    property Contest: TContest read FContest;
    function CanAddParticipant: boolean; virtual;
    function CanDeleteParticipant: boolean; virtual;
    function CanListParticipants: boolean; virtual;
    procedure AddParticipant(AInfo: TUserInfo);
    procedure DeleteParticipant(AInfo: TUserInfo);
    function ListParticipants: TStringList;
  end;

  { TBaseContestTransaction }

  TBaseContestTransaction = class(TEditableTransaction)
  private
    FAllowUpsolving: boolean;
    FDurationMinutes: integer;
    FProblemList: TStringList;
    FWasProblemList: TStringList;
    FScoringPolicy: TContestScoringPolicy;
    FStartTime: TDateTime;
    function GetContest: TContest;
    function GetEndTime: TDateTime;
    function GetProblemCount: integer;
    function GetProblemNames(I: integer): string;
    function GetProblemTitles(I: integer): string;
    function GetStatus: TContestStatus;
  protected
    property ProblemList: TStringList read FProblemList;
    procedure ReloadProblemList;
    procedure CommitProblemList;
    procedure DoReload; override;
    procedure DoCommit; override;
    procedure DoClone(ADest: TEditableTransaction); override;
    {%H-}constructor Create(AManager: TEditableManager; AUser: TUser;
      AObject: TEditableObject);
  public
    property Contest: TContest read GetContest;
    property StartTime: TDateTime read FStartTime write FStartTime;
    property EndTime: TDateTime read GetEndTime;
    property Status: TContestStatus read GetStatus;
    property DurationMinutes: integer read FDurationMinutes write FDurationMinutes;
    property ScoringPolicy: TContestScoringPolicy read FScoringPolicy write FScoringPolicy;
    property AllowUpsolving: boolean read FAllowUpsolving write FAllowUpsolving;
    property ProblemNames[I: integer]: string read GetProblemNames;
    property ProblemTitles[I: integer]: string read GetProblemTitles;
    property ProblemCount: integer read GetProblemCount;
    function GetProblem(AIndex: integer): TContestProblem;
    function CanAddProblem(const AProblemName: string): boolean; virtual;
    procedure AddProblem(const AProblemName: string; AIndex: integer = -1);
    function CanDeleteProblem(AIndex: integer): boolean;
    procedure DeleteProblem(AIndex: integer);
    function CanMoveProblemUp(AIndex: integer): boolean;
    procedure MoveProblemUp(AIndex: integer);
    function CanMoveProblemDown(AIndex: integer): boolean;
    procedure MoveProblemDown(AIndex: integer);
    procedure Validate; override;
    destructor Destroy; override;
  end;

  { TContestTransaction }

  TContestTransaction = class(TBaseContestTransaction)
  public
  end;

  { TContestManagerSession }

  TContestManagerSession = class(TEditableManagerSession)
  protected
    {%H-}constructor Create(AManager: TEditableManager; AUser: TUser);
  end;

  { TContestProblemList }

  TContestProblemList = class
  private
    FContest: TContest;
    FStorage: TAbstractDataStorage;
    function ProblemIndexToId(AIndex: integer): integer;
    procedure AddProblemToList(AIndex: integer; const AProblemName: string);
  protected
    property Storage: TAbstractDataStorage read FStorage;
    function ProblemsSectionName: string;
    function ProblemsByIdSectionName: string;
    function ProblemByIdKeyName(AProblemID: integer): string;
    function ProblemsByIndexSectionName: string;
    function ProblemByIndexKeyName(AIndex: integer): string;
    function ProblemCountKeyName: string;
    function GetProblemList: TStringList;
    procedure SetProblemList(AValue: TStringList);
    function GetProblemCount: integer;
    {%H-}constructor Create(AContest: TContest);
  public
    property Contest: TContest read FContest;
    property ProblemList: TStringList read GetProblemList write SetProblemList;
    property ProblemCount: integer read GetProblemCount;
    function ProblemIndex(AProblem: TContestProblem): integer;
    function HasProblem(AProblem: TContestProblem): boolean;
    function GetProblem(AIndex: integer): TContestProblem;
    function IsProblemListValid(AValue: TStringList): boolean;
    constructor Create;
  end;

  { TContest }

  TContest = class(TBaseContest)
  private
    FProblemList: TContestProblemList;
    function GetManager: TContestManager;
  protected
    property Storage;
    property ProblemList: TContestProblemList read FProblemList;
    function DoCreateProblemList: TContestProblemList; virtual;
    function ProblemsSectionName: string;
    function ParticipantsSectionName: string;
    function ParticipantsFullKeyName(ParticipantID: integer): string;
    procedure AddParticipant(AInfo: TUserInfo);
    procedure DoAddParticipant(AInfo: TUserInfo);
    procedure DeleteParticipant(AInfo: TUserInfo);
    procedure DoDeleteParticipant(AInfo: TUserInfo);
    function HasParticipant(AInfo: TUserInfo): boolean; override;
    function DoGetProblem(const AProblemName: string): TContestProblem; virtual;
    function ListParticipants: TStringList;
    function ContestStartTime: TDateTime;
    function ContestDurationMinutes: integer;
    function ContestEndTime: TDateTime;
    function ContestStatus: TContestStatus; override;
    function ContestAllowUpsolving: boolean; override;
    procedure HandleSelfDeletion; override;
    procedure HandleUserDeleting(AInfo: TUserInfo); override;
    procedure HandleProblemDeleting(AProblem: TContestProblem); virtual;
    procedure MessageReceived(AMessage: TAuthorMessage); override;
    {%H-}constructor Create(const AName: string; AManager: TEditableManager);
  public
    property Manager: TContestManager read GetManager;
    function ProblemManager: TContestProblemManager;
    function CreateAccessSession(AUser: TUser): TEditableObjectAccessSession; override;
    function CreateParticipantSession(AUser: TUser): TContestParticipantSession; virtual;
    function CreateTransaction(AUser: TUser): TEditableTransaction; override;
    destructor Destroy; override;
  end;

  { TContestManager }

  TContestManager = class(TBaseContestManager)
  protected
    function ObjectTypeName: string; override;
    function CreateObject(const AName: string): TEditableObject; override;
    function CreateStorage: TAbstractDataStorage; override;
    procedure DoCreateNewObject(AObject: TEditableObject); override;
  public
    function ProblemManager: TContestProblemManager; virtual; abstract;
    function SubmissionManager: TContestSubmissionManager; virtual; abstract;
    function CreateManagerSession(AUser: TUser): TEditableManagerSession; override;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TContestProblemList }

function TContestProblemList.ProblemIndexToId(AIndex: integer): integer;
begin
  Result := Storage.ReadInteger(ProblemByIndexKeyName(AIndex), -1);
end;

procedure TContestProblemList.AddProblemToList(AIndex: integer;
  const AProblemName: string);
var
  ProblemID: integer;
begin
  ProblemID := Contest.ProblemManager.ObjectNameToId(AProblemName);
  Storage.WriteInteger(ProblemByIdKeyName(ProblemID), AIndex);
  Storage.WriteInteger(ProblemByIndexKeyName(AIndex), ProblemID);
end;

function TContestProblemList.ProblemsSectionName: string;
begin
  Result := Contest.ProblemsSectionName;
end;

function TContestProblemList.ProblemsByIdSectionName: string;
begin
  Result := ProblemsSectionName + '.byId';
end;

function TContestProblemList.ProblemByIdKeyName(AProblemID: integer): string;
begin
  Result := ProblemsByIdSectionName + '.' + Id2Str(AProblemID);
end;

function TContestProblemList.ProblemsByIndexSectionName: string;
begin
  Result := ProblemsSectionName + '.byIndex';
end;

function TContestProblemList.ProblemByIndexKeyName(AIndex: integer): string;
begin
  Result := ProblemsByIndexSectionName + '.idx' + IntToStr(AIndex);
end;

function TContestProblemList.ProblemCountKeyName: string;
begin
  Result := ProblemsSectionName + '.count';
end;

function TContestProblemList.GetProblemList: TStringList;
var
  I: integer;
begin
  Result := TStringList.Create;
  try
    for I := 0 to ProblemCount - 1 do
      Result.Add(Contest.ProblemManager.IdToObjectName(ProblemIndexToId(I)));
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TContestProblemList.SetProblemList(AValue: TStringList);
var
  I: integer;
begin
  // validate new list
  if not IsProblemListValid(AValue) then
    raise EContestValidate.Create(SProblemListNotValid);
  // delete old values
  Storage.DeletePath(ProblemsByIdSectionName);
  Storage.DeletePath(ProblemsByIndexSectionName);
  // add new ones
  Storage.WriteInteger(ProblemCountKeyName, AValue.Count);
  for I := 0 to AValue.Count - 1 do
    AddProblemToList(I, AValue[I]);
end;

function TContestProblemList.GetProblemCount: integer;
begin
  Result := Storage.ReadInteger(ProblemCountKeyName, 0);
end;

constructor TContestProblemList.Create(AContest: TContest);
begin
  inherited Create;
  FContest := AContest;
  FStorage := AContest.Storage;
end;

function TContestProblemList.ProblemIndex(AProblem: TContestProblem): integer;
begin
  Result := Storage.ReadInteger(ProblemByIdKeyName(AProblem.ID), -1);
end;

function TContestProblemList.HasProblem(AProblem: TContestProblem): boolean;
begin
  Result := ProblemIndex(AProblem) >= 0;
end;

function TContestProblemList.GetProblem(AIndex: integer): TContestProblem;
begin
  if (AIndex < 0) or (AIndex >= ProblemCount) then
    raise EContestValidate.CreateFmt(SIndexOutOfBounds, [AIndex]);
  Result := Contest.DoGetProblem(ProblemByIndexKeyName(AIndex));
end;

function TContestProblemList.IsProblemListValid(AValue: TStringList): boolean;
var
  I: integer;
begin
  Result := False;
  for I := 0 to AValue.Count - 1 do
    if not Contest.ProblemManager.ObjectExists(AValue[I]) then
      Exit;
  Result := True;
end;

constructor TContestProblemList.Create;
begin
  // we don't want the contest problem list to be created publicly!
  raise EInvalidOperation.CreateFmt(SCreationPublic, [ClassName]);
end;

{ TBaseContestTransaction }

function TBaseContestTransaction.GetEndTime: TDateTime;
begin
  Result := Contest.ContestEndTime;
end;

function TBaseContestTransaction.GetProblemCount: integer;
begin
  Result := FProblemList.Count;
end;

function TBaseContestTransaction.GetProblemNames(I: integer): string;
begin
  Result := FProblemList.Strings[I];
end;

function TBaseContestTransaction.GetProblemTitles(I: integer): string;
var
  Problem: TContestProblem;
  Transaction: TContestTestProblemTransaction;
begin
  Problem := GetProblem(I);
  try
    Transaction := Problem.CreateTestTransaction(User) as TContestTestProblemTransaction;
    try
      Result := Transaction.Title;
    finally
      FreeAndNil(Transaction);
    end;
  finally
    FreeAndNil(Problem);
  end;
end;

function TBaseContestTransaction.GetContest: TContest;
begin
  Result := EditableObject as TContest;
end;

function TBaseContestTransaction.GetStatus: TContestStatus;
begin
  Result := Contest.ContestStatus;
end;

procedure TBaseContestTransaction.ReloadProblemList;
begin
  FreeAndNil(FProblemList);
  FProblemList := Contest.ProblemList.ProblemList;
  FWasProblemList.Assign(FProblemList);
end;

procedure TBaseContestTransaction.CommitProblemList;
begin
  if not FWasProblemList.Equals(FProblemList) then
    Contest.ProblemList.ProblemList := FProblemList;
end;

procedure TBaseContestTransaction.DoReload;
begin
  inherited DoReload;
  FStartTime := Storage.ReadFloat(FullKeyName('startTime'), Now);
  FDurationMinutes := Storage.ReadInteger(FullKeyName('durationMinutes'), 0);
  FScoringPolicy := StrToScoringPolicy(Storage.ReadString(FullKeyName('scoringPolicy'),
    ScoringPolicyToStr(spMaxScore)));
  FAllowUpsolving := Storage.ReadBool(FullKeyName('allowUpsolving'), True);
  ReloadProblemList;
end;

procedure TBaseContestTransaction.DoCommit;
begin
  inherited DoCommit;
  Storage.WriteFloat(FullKeyName('startTime'), FStartTime);
  Storage.WriteInteger(FullKeyName('durationMinutes'), FDurationMinutes);
  Storage.WriteString(FullKeyName('scoringPolicy'), ScoringPolicyToStr(FScoringPolicy));
  Storage.WriteBool(FullKeyName('allowUpsolving'), FAllowUpsolving);
  CommitProblemList;
end;

procedure TBaseContestTransaction.DoClone(ADest: TEditableTransaction);
begin
  inherited DoClone(ADest);
  with ADest as TBaseContestTransaction do
  begin
    StartTime := Self.StartTime;
    DurationMinutes := Self.DurationMinutes;
    ScoringPolicy := Self.ScoringPolicy;
    AllowUpsolving := Self.AllowUpsolving;
    ProblemList.Assign(Self.ProblemList);
  end;
end;

constructor TBaseContestTransaction.Create(AManager: TEditableManager;
  AUser: TUser; AObject: TEditableObject);
begin
  inherited Create(AManager, AUser, AObject);
  FProblemList := TStringList.Create;
  FWasProblemList := TStringList.Create;
end;

function TBaseContestTransaction.GetProblem(AIndex: integer): TContestProblem;
begin
  Result := Contest.DoGetProblem(ProblemList[AIndex]);
end;

function TBaseContestTransaction.CanAddProblem(const AProblemName: string): boolean;
begin
  with Contest.ProblemManager do
    Result := CanWriteData and (GetAccessRights(AProblemName, User.Info) in AccessCanReadSet);
end;

procedure TBaseContestTransaction.AddProblem(const AProblemName: string;
  AIndex: integer);
begin
  // validate
  if not Contest.ProblemManager.ObjectExists(AProblemName) then
    raise EContestValidate.CreateFmt(SProblemNotExist, [AProblemName]);
  if ProblemList.IndexOf(AProblemName) >= 0 then
    raise EContestValidate.CreateFmt(SProblemAlreadyAdded, [AProblemName]);
  if not CanAddProblem(AProblemName) then
    raise EContestAccessDenied.Create(SAccessDenied);
  // add problem
  if AIndex < 0 then
    ProblemList.Add(AProblemName)
  else
    ProblemList.Insert(AIndex, AProblemName);
end;

function TBaseContestTransaction.CanDeleteProblem(AIndex: integer): boolean;
begin
  Result := CanWriteData and (AIndex >= 0) and (AIndex < ProblemList.Count);
end;

procedure TBaseContestTransaction.DeleteProblem(AIndex: integer);
begin
  if not CanDeleteProblem(AIndex) then
    raise EContestValidate.Create(SUnableDeleteProblem);
  ProblemList.Delete(AIndex);
end;

function TBaseContestTransaction.CanMoveProblemUp(AIndex: integer): boolean;
begin
  Result := CanWriteData and (AIndex > 0) and (AIndex < ProblemList.Count);
end;

procedure TBaseContestTransaction.MoveProblemUp(AIndex: integer);
begin
  if not CanMoveProblemUp(AIndex) then
    raise EContestValidate.Create(SUnableMoveProblemUp);
  ProblemList.Exchange(AIndex - 1, AIndex);
end;

function TBaseContestTransaction.CanMoveProblemDown(AIndex: integer): boolean;
begin
  Result := CanWriteData and (AIndex >= 0) and (AIndex + 1 < ProblemList.Count);
end;

procedure TBaseContestTransaction.MoveProblemDown(AIndex: integer);
begin
  if not CanMoveProblemDown(AIndex) then
    raise EContestValidate.Create(SUnableMoveProblemDown);
  ProblemList.Exchange(AIndex, AIndex + 1);
end;

procedure TBaseContestTransaction.Validate;
begin
  inherited Validate;
  if (FDurationMinutes < 0) or (FDurationMinutes > Config.Contest_MaxDurationMinutes) then
    raise EContestValidate.CreateFmt(SContestDurationInterval, [0, Config.Contest_MaxDurationMinutes]);
  if not Contest.ProblemList.IsProblemListValid(FProblemList) then
    raise EContestValidate.Create(SProblemListNotValid);
end;

destructor TBaseContestTransaction.Destroy;
begin
  FreeAndNil(FWasProblemList);
  FreeAndNil(FProblemList);
  inherited Destroy;
end;

{ TContestParticipantSession }

constructor TContestParticipantSession.Create(AManager: TEditableManager;
  AUser: TUser; AObject: TEditableObject);
begin
  inherited Create(AManager, AUser, AObject);
  FContest := AObject as TContest;
end;

function TContestParticipantSession.CanAddParticipant: boolean;
begin
  Result := AccessLevel in AccessCanWriteSet;
end;

function TContestParticipantSession.CanDeleteParticipant: boolean;
begin
  Result := AccessLevel in AccessCanWriteSet;
end;

function TContestParticipantSession.CanListParticipants: boolean;
begin
  Result := AccessLevel in AccessCanReadSet;
end;

procedure TContestParticipantSession.AddParticipant(AInfo: TUserInfo);
begin
  if not CanAddParticipant then
    raise EContestAccessDenied.Create(SAccessDenied);
  Contest.AddParticipant(AInfo);
end;

procedure TContestParticipantSession.DeleteParticipant(AInfo: TUserInfo);
begin
  if not CanDeleteParticipant then
    raise EContestAccessDenied.Create(SAccessDenied);
  Contest.DeleteParticipant(AInfo);
end;

function TContestParticipantSession.ListParticipants: TStringList;
begin
  if not CanListParticipants then
    raise EContestAccessDenied.Create(SAccessDenied);
  Result := Contest.ListParticipants;
end;

{ TContestManager }

function TContestManager.ObjectTypeName: string;
begin
  Result := SContestTypeName;
end;

function TContestManager.CreateObject(const AName: string): TEditableObject;
begin
  Result := TContest.Create(AName, Self);
end;

function TContestManager.CreateStorage: TAbstractDataStorage;
begin
  Result := TXmlDataStorage.Create('contests');
end;

procedure TContestManager.DoCreateNewObject(AObject: TEditableObject);
begin
  inherited DoCreateNewObject(AObject);
  (AObject as TContest).ContestStartTime; // we write start time to storage
end;

function TContestManager.CreateManagerSession(AUser: TUser): TEditableManagerSession;
begin
  Result := TContestManagerSession.Create(Self, AUser);
end;

constructor TContestManager.Create;
begin
  inherited Create;
  ProblemManager.Subscribe(Self);
end;

destructor TContestManager.Destroy;
begin
  if ProblemManager <> nil then
    ProblemManager.Unsubscribe(Self);
  inherited Destroy;
end;

{ TContest }

function TContest.GetManager: TContestManager;
begin
  Result := (inherited Manager) as TContestManager;
end;

function TContest.DoCreateProblemList: TContestProblemList;
begin
  Result := TContestProblemList.Create(Self);
end;

function TContest.ProblemsSectionName: string;
begin
  Result := FullKeyName('problems');
end;

function TContest.ParticipantsSectionName: string;
begin
  Result := FullKeyName('participants');
end;

function TContest.ParticipantsFullKeyName(ParticipantID: integer): string;
begin
  Result := ParticipantsSectionName + '.' + Id2Str(ParticipantID);
end;

procedure TContest.AddParticipant(AInfo: TUserInfo);
begin
  if HasParticipant(AInfo) then
    raise EContestValidate.CreateFmt(SParticipantAlreadyAdded, [AInfo.Username]);
  DoAddParticipant(AInfo);
  UpdateModifyTime;
end;

procedure TContest.DoAddParticipant(AInfo: TUserInfo);
begin
  Storage.WriteBool(ParticipantsFullKeyName(AInfo.ID), True);
end;

procedure TContest.DeleteParticipant(AInfo: TUserInfo);
begin
  if not HasParticipant(AInfo) then
    raise EContestValidate.CreateFmt(SParticipantAlreadyDeleted, [AInfo.Username]);
  DoDeleteParticipant(AInfo);
  UpdateModifyTime;
end;

procedure TContest.DoDeleteParticipant(AInfo: TUserInfo);
begin
  Storage.DeleteVariable(ParticipantsFullKeyName(AInfo.ID));
end;

function TContest.HasParticipant(AInfo: TUserInfo): boolean;
begin
  Result := Storage.VariableExists(ParticipantsFullKeyName(AInfo.ID));
end;

function TContest.DoGetProblem(const AProblemName: string): TContestProblem;
begin
  Result := ProblemManager.GetObject(AProblemName) as TContestProblem;
  try
    SetToProblem(Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TContest.ListParticipants: TStringList;
var
  Keys: TStringList;
  Key: string;
begin
  Result := TStringList.Create;
  try
    Keys := Storage.GetChildElements(ParticipantsSectionName);
    try
      if Keys <> nil then
        for Key in Keys do
          Result.Add(UserManager.IdToUsername(Str2Id(Key)));
    finally
      FreeAndNil(Keys);
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TContest.ContestStartTime: TDateTime;
begin
  Result := Storage.ReadFloat(FullKeyName('startTime'), Now);
end;

function TContest.ContestDurationMinutes: integer;
begin
  Result := Storage.ReadInteger(FullKeyName('durationMinutes'), 0);
end;

function TContest.ContestEndTime: TDateTime;
begin
  Result := IncMinute(ContestStartTime, ContestDurationMinutes);
end;

function TContest.ContestStatus: TContestStatus;
var
  CurTime: TDateTime;
begin
  CurTime := Now;
  if CompareDateTime(CurTime, ContestStartTime) <= 0 then
    Result := csNotStarted
  else if CompareDateTime(CurTime, ContestEndTime) < 0 then
    Result := csRunning
  else
    Result := csUpsolve;
end;

function TContest.ContestAllowUpsolving: boolean;
begin
  Result := Storage.ReadBool(FullKeyName('allowUpsolving'), True);
end;

procedure TContest.HandleSelfDeletion;
begin
  inherited HandleSelfDeletion;
  // to be implemented later ...
  // TODO : Implement it, make "later" come :)
end;

procedure TContest.HandleUserDeleting(AInfo: TUserInfo);
begin
  inherited HandleUserDeleting(AInfo);
  DoDeleteParticipant(AInfo);
end;

procedure TContest.HandleProblemDeleting(AProblem: TContestProblem);
var
  ProblemNames: TStringList;
  ProblemIndex: integer;
begin
  if ProblemList.HasProblem(AProblem) then
  begin
    ProblemNames := ProblemList.ProblemList;
    try
      ProblemIndex := ProblemNames.IndexOf(AProblem.Name);
      if ProblemIndex >= 0 then
        ProblemNames.Delete(ProblemIndex);
      ProblemList.ProblemList := ProblemNames;
    finally
      FreeAndNil(ProblemNames);
    end;
  end;
end;

procedure TContest.MessageReceived(AMessage: TAuthorMessage);
begin
  if (AMessage is TEditableDeletingMessage) and
    (AMessage.Sender is TContestProblemManager) then
    HandleProblemDeleting((AMessage as TEditableDeletingMessage).EditableObject as TContestProblem)
  else
    inherited MessageReceived(AMessage);
end;

constructor TContest.Create(const AName: string; AManager: TEditableManager);
begin
  inherited Create(AName, AManager);
  FProblemList := DoCreateProblemList;
end;

function TContest.ProblemManager: TContestProblemManager;
begin
  Result := (Manager as TContestManager).ProblemManager;
end;

function TContest.CreateAccessSession(AUser: TUser): TEditableObjectAccessSession;
begin
  Result := TContestAccessSession.Create(Manager, AUser, Self);
end;

function TContest.CreateParticipantSession(AUser: TUser): TContestParticipantSession;
begin
  Result := TContestParticipantSession.Create(Manager, AUser, Self);
end;

function TContest.CreateTransaction(AUser: TUser): TEditableTransaction;
begin
  Result := TContestTransaction.Create(Manager, AUser, Self);
end;

destructor TContest.Destroy;
begin
  FreeAndNil(FProblemList);
  inherited Destroy;
end;

{ TContestManagerSession }

constructor TContestManagerSession.Create(AManager: TEditableManager; AUser: TUser);
begin
  inherited Create(AManager, AUser);
end;

{ TContestAccessSession }

constructor TContestAccessSession.Create(AManager: TEditableManager;
  AUser: TUser; AObject: TEditableObject);
begin
  inherited Create(AManager, AUser, AObject);
end;

end.

