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
unit contestproblems;

{$mode objfpc}{$H+}{$B-}

interface

uses
  Classes, SysUtils, problems, submissions, editableobjects, users, tswebutils,
  tswebobservers, webstrconsts;

type
  TContestProblem = class;

  TContestStatus = (csNotStarted, csRunning, csUpsolve);

  { TBaseContest }

  TBaseContest = class(TEditableObject)
  protected
    procedure SetToProblem(AProblem: TContestProblem);
    function ContestStatus: TContestStatus; virtual; abstract;
    function ContestAllowUpsolving: boolean; virtual; abstract;
    function HasParticipant(AInfo: TUserInfo): boolean; virtual; abstract;
    function ParticipantCanSubmit(AInfo: TUserInfo): boolean; virtual;
    function ParticipantCanView(AInfo: TUserInfo): boolean; virtual;
  end;

  { TBaseContestManager }

  TBaseContestManager = class(TEditableManager)
  end;

  { TContestTestSubmission }

  TContestTestSubmission = class(TTestSubmission)
  private
    function GetContest: TBaseContest;
    function GetContestID: integer;
    function GetContestName: string;
    function GetRated: boolean;
  protected
    function DoGetProblem: TTestableProblem; override;
    {%H-}constructor Create(AManager: TSubmissionManager; AID: integer);
  public
    function ContestManager: TBaseContestManager;
    property Contest: TBaseContest read GetContest;
    property ContestID: integer read GetContestID;
    property ContestName: string read GetContestName;
    property Rated: boolean read GetRated;
  end;

  { TContestProblemSubmissionSession }

  TContestProblemSubmissionSession = class(TProblemSubmissionSession)
  protected
    function ContestAccessLevel(AProblem: TContestProblem): TEditableAccessRights;
    function DoCreateProblemFromSubmission(AID: integer): TTestableProblem;
      override;
    {%H-}constructor Create(AManager: TEditableManager; AUser: TUser);
  public
    function CanReadSubmission(AProblem: TTestableProblem; AOwner: TUserInfo): boolean;
      override;
    function CanRejudgeSubmission(AProblem: TTestableProblem): boolean;
      override;
    function ListByContest(AContest: TBaseContest): TIdList;
  end;

  { TContestTestProblemTransaction }

  TContestTestProblemTransaction = class(TTestProblemTransaction)
  protected
    function ContestAccessLevel: TEditableAccessRights;
    {%H-}constructor Create(AManager: TEditableManager; AUser: TUser;
      AObject: TEditableObject);
  public
    function Contest: TBaseContest;
    function CanTestProblem: boolean; override;
    function CanReadData: boolean; override;
  end;

  { TContestProblem }

  TContestProblem = class(TTestableProblem)
  private
    FContest: TBaseContest;
    function GetContestManager: TBaseContestManager;
  protected
    procedure SetContest(AID: integer);
    procedure SetContest(const AName: string);
    {%H-}constructor Create(const AName: string; AManager: TEditableManager);
  public
    property ContestManager: TBaseContestManager read GetContestManager;
    property Contest: TBaseContest read FContest;
    function CreateTestTransaction(AUser: TUser): TTestProblemTransaction;
      override;
    destructor Destroy; override;
  end;

  { TContestProblemManager }

  TContestProblemManager = class(TTestableProblemManager)
  protected
    function CreateObject(const AName: string): TEditableObject; override;
  public
    function ContestManager: TBaseContestManager; virtual; abstract;
    function CreateSubmissionSession(AUser: TUser): TProblemSubmissionSession;
      override;
  end;

  { TContestSubmissionManager }

  TContestSubmissionManager = class(TSubmissionManager)
  protected
    function ContestSectionName(AID: integer): string;
    function SubmissionContestID(AID: integer): integer;
    procedure HandleContestDeleting(AContest: TBaseContest); virtual;
    function DoCreateTestSubmission(AID: integer): TTestSubmission; override;
    procedure DoInternalCreateSubmission(ASubmission: TTestSubmission;
      AProblem: TProblem; AUser: TUser); override;
    procedure DoDeleteSubmission(AID: integer); override;
    procedure MessageReceived(AMessage: TAuthorMessage); override;
  public
    function ContestManager: TBaseContestManager; virtual; abstract;
    function ContestFilter(AID: integer; AObject: TObject): boolean;
    function ListByContest(AContest: TBaseContest): TIdList;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TContestTestSubmission }

function TContestTestSubmission.GetContest: TBaseContest;
begin
  if ContestID >= 0 then
    Result := ContestManager.GetObject(ContestID) as TBaseContest
  else
    Result := nil;
end;

function TContestTestSubmission.GetContestID: integer;
begin
  Result := (Manager as TContestSubmissionManager).SubmissionContestID(ID);
end;

function TContestTestSubmission.GetContestName: string;
begin
  Result := ContestManager.IdToObjectName(ContestID);
end;

function TContestTestSubmission.GetRated: boolean;
begin
  Result := Storage.ReadBool(FullKeyName('isItRated'), False);
end;

function TContestTestSubmission.DoGetProblem: TTestableProblem;
begin
  Result := inherited DoGetProblem;
  try
    if ContestID >= 0 then
      (Result as TContestProblem).SetContest(ContestID);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

constructor TContestTestSubmission.Create(AManager: TSubmissionManager;
  AID: integer);
begin
  inherited Create(AManager, AID);
end;

function TContestTestSubmission.ContestManager: TBaseContestManager;
begin
  Result := (Manager as TContestSubmissionManager).ContestManager;
end;

{ TBaseContest }

procedure TBaseContest.SetToProblem(AProblem: TContestProblem);
begin
  AProblem.SetContest(Self.Name);
end;

function TBaseContest.ParticipantCanSubmit(AInfo: TUserInfo): boolean;
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

function TBaseContest.ParticipantCanView(AInfo: TUserInfo): boolean;
begin
  if not HasParticipant(AInfo) then
    Exit(False);
  Result := ContestStatus in [csRunning, csUpsolve];
end;

{ TContestSubmissionManager }

function TContestSubmissionManager.ContestSectionName(AID: integer): string;
begin
  Result := 'contests.' + Id2Str(AID);
end;

function TContestSubmissionManager.SubmissionContestID(AID: integer): integer;
begin
  Result := Storage.ReadInteger(SubmissionSectionName(AID) + '.contestId', -1);
end;

procedure TContestSubmissionManager.HandleContestDeleting(AContest: TBaseContest);
var
  List: TIdList;
  ID: integer;
begin
  List := ListByContest(AContest);
  try
    for ID in List do
      DeleteSubmission(ID);
    Storage.DeletePath(ContestSectionName(AContest.ID));
  finally
    FreeAndNil(List);
  end;
end;

function TContestSubmissionManager.DoCreateTestSubmission(AID: integer): TTestSubmission;
begin
  Result := TContestTestSubmission.Create(Self, AID);
end;

function TContestSubmissionManager.ListByContest(AContest: TBaseContest): TIdList;
var
  StrList: TStringList;
begin
  StrList := Storage.GetChildElements(ContestSectionName(AContest.ID));
  try
    Result := StrListToIdList(StrList);
  finally
    FreeAndNil(StrList);
  end;
end;

function TContestSubmissionManager.ContestFilter(AID: integer;
  AObject: TObject): boolean;
begin
  Result := SubmissionContestID(AID) = (AObject as TBaseContest).ID;
end;

procedure TContestSubmissionManager.MessageReceived(AMessage: TAuthorMessage);
begin
  if (AMessage is TEditableDeletingMessage) and (AMessage.Sender is TBaseContest) then
    HandleContestDeleting((AMessage as TEditableDeletingMessage).EditableObject as TBaseContest)
  else
    inherited MessageReceived(AMessage);
end;

procedure TContestSubmissionManager.DoInternalCreateSubmission(
  ASubmission: TTestSubmission; AProblem: TProblem; AUser: TUser);
var
  Contest: TBaseContest;
  ID: integer;
begin
  inherited DoInternalCreateSubmission(ASubmission, AProblem, AUser);
  Contest := (AProblem as TContestProblem).Contest;
  ID := ASubmission.ID;
  if Contest <> nil then
  begin
    Storage.WriteInteger(SubmissionSectionName(ID) + '.contestId', Contest.ID);
    Storage.WriteBool(ContestSectionName(Contest.ID) + '.' + Id2Str(ID), True);
    if Contest.ContestStatus = csRunning then
      Storage.WriteBool(SubmissionSectionName(ID) + '.isItRated', True);
  end;
end;

procedure TContestSubmissionManager.DoDeleteSubmission(AID: integer);
var
  ContestID: integer;
begin
  ContestID := SubmissionContestID(AID);
  if ContestID >= 0 then
    Storage.DeleteVariable(ContestSectionName(ContestID) + '.' + Id2Str(AID));
  inherited DoDeleteSubmission(AID);
end;

constructor TContestSubmissionManager.Create;
begin
  inherited Create;
  ContestManager.Subscribe(Self);
end;

destructor TContestSubmissionManager.Destroy;
begin
  if ContestManager <> nil then
    ContestManager.Unsubscribe(Self);
  inherited Destroy;
end;

{ TContestProblemManager }

function TContestProblemManager.CreateObject(const AName: string): TEditableObject;
begin
  Result := TContestProblem.Create(AName, Self);
end;

function TContestProblemManager.CreateSubmissionSession(AUser: TUser): TProblemSubmissionSession;
begin
  Result := TContestProblemSubmissionSession.Create(Self, AUser);
end;

{ TContestProblem }

function TContestProblem.GetContestManager: TBaseContestManager;
begin
  Result := (Manager as TContestProblemManager).ContestManager;
end;

procedure TContestProblem.SetContest(AID: integer);
begin
  SetContest(ContestManager.IdToObjectName(AID));
end;

procedure TContestProblem.SetContest(const AName: string);
begin
  if FContest <> nil then
    raise EEditableValidate.Create(SContestAlreadySet);
  FContest := ContestManager.GetObject(AName) as TBaseContest;
end;

constructor TContestProblem.Create(const AName: string; AManager: TEditableManager);
begin
  inherited Create(AName, AManager);
  FContest := nil;
end;

function TContestProblem.CreateTestTransaction(AUser: TUser): TTestProblemTransaction;
begin
  Result := TContestTestProblemTransaction.Create(Manager, AUser, Self);
end;

destructor TContestProblem.Destroy;
begin
  FreeAndNil(FContest);
  inherited Destroy;
end;

{ TContestTestProblemTransaction }

function TContestTestProblemTransaction.Contest: TBaseContest;
begin
  Result := (EditableObject as TContestProblem).Contest;
end;

function TContestTestProblemTransaction.ContestAccessLevel: TEditableAccessRights;
begin
  if (User is TEditorUser) and (Contest <> nil) then
    Result := Contest.GetAccessRights(User as TEditorUser)
  else
    Result := erNone;
end;

constructor TContestTestProblemTransaction.Create(AManager: TEditableManager;
  AUser: TUser; AObject: TEditableObject);
begin
  inherited Create(AManager, AUser, AObject);
end;

function TContestTestProblemTransaction.CanTestProblem: boolean;
begin
  // inherit access
  Result := inherited CanTestProblem;
  if Result then
    Exit;
  // for contest setters
  if ContestAccessLevel in AccessCanReadSet then
    Exit(True);
  // for participants
  if Contest <> nil then
    Result := Contest.ParticipantCanSubmit(User.Info);
end;

function TContestTestProblemTransaction.CanReadData: boolean;
begin
  // inherit access
  Result := inherited CanReadData;
  if Result then
    Exit;
  // for contest setters
  if ContestAccessLevel in AccessCanReadSet then
    Exit(True);
  // for participants
  if Contest <> nil then
    Result := Contest.ParticipantCanView(User.Info);
end;

{ TContestProblemSubmissionSession }

function TContestProblemSubmissionSession.ContestAccessLevel(
  AProblem: TContestProblem): TEditableAccessRights;
begin
  if (User is TEditorUser) and (AProblem.Contest <> nil) then
    Result := AProblem.Contest.GetAccessRights(User as TEditorUser)
  else
    Result := erNone;
end;

function TContestProblemSubmissionSession.DoCreateProblemFromSubmission(AID: integer): TTestableProblem;
var
  ContestID: integer;
  ContestSubmitMgr: TContestSubmissionManager;
begin
  Result := inherited DoCreateProblemFromSubmission(AID);
  try
    ContestSubmitMgr := SubmissionManager as TContestSubmissionManager;
    ContestID := ContestSubmitMgr.SubmissionContestID(AID);
    if ContestID >= 0 then
      (Result as TContestProblem).SetContest(ContestID);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

constructor TContestProblemSubmissionSession.Create(AManager: TEditableManager;
  AUser: TUser);
begin
  inherited Create(AManager, AUser);
end;

function TContestProblemSubmissionSession.CanReadSubmission(AProblem: TTestableProblem;
  AOwner: TUserInfo): boolean;
var
  Contest: TBaseContest;
  ContestAccess: TEditableAccessRights;
begin
  // inherit access
  Result := inherited CanReadSubmission(AProblem, AOwner);
  if Result then
    Exit;
  // for contest setters
  ContestAccess := ContestAccessLevel(AProblem as TContestProblem);
  if (ContestAccess in AccessCanReadSet) and (AOwner.ID = User.ID) then
    Exit(True);
  if ContestAccess in AccessCanWriteSet then
    Exit(True);
  // for participants
  if AOwner.ID = User.ID then
  begin
    Contest := (AProblem as TContestProblem).Contest;
    if Contest <> nil then
      Result := Contest.HasParticipant(User.Info);
  end;
end;

function TContestProblemSubmissionSession.CanRejudgeSubmission(
  AProblem: TTestableProblem): boolean;
begin
  // inherit access
  Result := inherited CanRejudgeSubmission(AProblem);
  if Result then
    Exit;
  // for contest setters (with write access!)
  Result := ContestAccessLevel(AProblem as TContestProblem) in AccessCanWriteSet;
end;

function TContestProblemSubmissionSession.ListByContest(AContest: TBaseContest): TIdList;
begin
  with SubmissionManager do
    Result := Filter(ListByContest(AContest), Self, @AvailableFilter);
end;

end.
