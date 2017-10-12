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
  tswebutils, dateutils, typinfo, serverconfig, contestproblems,
  tswebobservers;

type
  EContestAction = class(EEditableAction);
  EContestValidate = class(EEditableValidate);
  EContestAccessDenied = class(EEditableAccessDenied);

  TContest = class;
  TContestManager = class;

  TContestStatus = (csNotStarted, csRunning, csUpsolve);
  TContestScoringPolicy = (spMaxScore, spLastScore);

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
    FScoringPolicy: TContestScoringPolicy;
    FStartTime: TDateTime;
    function GetEndTime: TDateTime;
    function GetStatus: TContestStatus;
  protected
    procedure DoReload; override;
    procedure DoCommit; override;
    procedure DoClone(ADest: TEditableTransaction); override;
    {%H-}constructor Create(AManager: TEditableManager; AUser: TUser;
      AObject: TEditableObject);
  public
    property StartTime: TDateTime read FStartTime write FStartTime;
    property EndTime: TDateTime read GetEndTime;
    property Status: TContestStatus read GetStatus;
    property DurationMinutes: integer read FDurationMinutes write FDurationMinutes;
    property ScoringPolicy: TContestScoringPolicy read FScoringPolicy write FScoringPolicy;
    property AllowUpsolving: boolean read FAllowUpsolving write FAllowUpsolving;
    procedure Validate; override;
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

  { TContest }

  TContest = class(TBaseContest)
  private
    function GetManager: TContestManager;
  protected
    function ParticipantsSectionName: string;
    function ParticipantsFullKeyName(ParticipantID: integer): string;
    procedure AddParticipant(AInfo: TUserInfo);
    procedure DoAddParticipant(AInfo: TUserInfo);
    procedure DeleteParticipant(AInfo: TUserInfo);
    procedure DoDeleteParticipant(AInfo: TUserInfo);
    function HasParticipant(AInfo: TUserInfo): boolean; override;
    function ParticipantCanSubmit(AInfo: TUserInfo): boolean; override;
    function ParticipantCanView(AInfo: TUserInfo): boolean; override;
    function ListParticipants: TStringList;
    function ContestStartTime: TDateTime;
    function ContestDurationMinutes: integer;
    function ContestEndTime: TDateTime;
    function ContestStatus: TContestStatus;
    function ContestAllowUpsolving: boolean;
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

{ TBaseContestTransaction }

function TBaseContestTransaction.GetEndTime: TDateTime;
begin
  Result := (EditableObject as TContest).ContestEndTime;
end;

function TBaseContestTransaction.GetStatus: TContestStatus;
begin
  Result := (EditableObject as TContest).ContestStatus;
end;

procedure TBaseContestTransaction.DoReload;
begin
  inherited DoReload;
  FStartTime := Storage.ReadFloat(FullKeyName('startTime'), Now);
  FDurationMinutes := Storage.ReadInteger(FullKeyName('durationMinutes'), 0);
  FScoringPolicy := StrToScoringPolicy(Storage.ReadString(FullKeyName('scoringPolicy'),
    ScoringPolicyToStr(spMaxScore)));
  FAllowUpsolving := Storage.ReadBool(FullKeyName('allowUpsolving'), True);
end;

procedure TBaseContestTransaction.DoCommit;
begin
  inherited DoCommit;
  Storage.WriteFloat(FullKeyName('startTime'), FStartTime);
  Storage.WriteInteger(FullKeyName('durationMinutes'), FDurationMinutes);
  Storage.WriteString(FullKeyName('scoringPolicy'), ScoringPolicyToStr(FScoringPolicy));
  Storage.WriteBool(FullKeyName('allowUpsolving'), FAllowUpsolving);
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
  end;
end;

constructor TBaseContestTransaction.Create(AManager: TEditableManager;
  AUser: TUser; AObject: TEditableObject);
begin
  inherited Create(AManager, AUser, AObject);
end;

procedure TBaseContestTransaction.Validate;
begin
  inherited Validate;
  if (FDurationMinutes < 0) or (FDurationMinutes > Config.Contest_MaxDurationMinutes) then
    raise EContestValidate.CreateFmt(SContestDurationInterval, [0, Config.Contest_MaxDurationMinutes]);
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

{ TContest }

function TContest.GetManager: TContestManager;
begin
  Result := (inherited Manager) as TContestManager;
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

function TContest.ParticipantCanSubmit(AInfo: TUserInfo): boolean;
begin
  if not HasParticipant(AInfo) then
    Exit(False);
  case ContestStatus of
    csRunning: Result := True;
    csUpsolve: Result := ContestAllowUpsolving
    else
      Result := False;
  end;
end;

function TContest.ParticipantCanView(AInfo: TUserInfo): boolean;
begin
  if not HasParticipant(AInfo) then
    Exit(False);
  Result := ContestStatus in [csRunning, csUpsolve];
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
begin
  // TODO : Implement HandleProblemDeleting !!!
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

