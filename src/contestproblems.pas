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

  { TBaseContest }

  TBaseContest = class(TEditableObject)
  protected
    function HasParticipant(AInfo: TUserInfo): boolean; virtual; abstract;
    function ParticipantCanSubmit(AInfo: TUserInfo): boolean; virtual; abstract;
    function ParticipantCanView(AInfo: TUserInfo): boolean; virtual; abstract;
  end;

  { TBaseContestManager }

  TBaseContestManager = class(TEditableManager)
  end;

  { TContestProblemSubmissionSession }

  TContestProblemSubmissionSession = class(TProblemSubmissionSession)
  protected
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
    function Contest: TBaseContest;
    {%H-}constructor Create(AManager: TEditableManager; AUser: TUser;
      AObject: TEditableObject);
  public
    function CanTestProblem: boolean; override;
    function CanReadData: boolean; override;
  end;

  { TContestProblem }

  TContestProblem = class(TTestableProblem)
  private
    FContest: TBaseContest;
    function GetContestId: integer;
    function GetContestManager: TBaseContestManager;
  protected
    procedure SetContest(AID: integer);
    procedure SetContest(const AName: string);
    {%H-}constructor Create(const AName: string; AManager: TEditableManager);
  public
    property ContestManager: TBaseContestManager read GetContestManager;
    property Contest: TBaseContest read FContest;
    property ContestId: integer read GetContestId;
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
    function SubmissionProblemID(AID: integer): integer;
    function SubmissionContestID(AID: integer): integer;
    procedure HandleContestDeleting(AContest: TBaseContest); virtual;
    function ListByContest(AContest: TBaseContest): TIdList;
    procedure MessageReceived(AMessage: TAuthorMessage); override;
  public
    function ContestManager: TBaseContestManager; virtual; abstract;
    function ContestFilter(AID: integer; AObject: TObject): boolean;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TContestSubmissionManager }

function TContestSubmissionManager.ContestSectionName(AID: integer): string;
begin
  Result := 'contests.' + Id2Str(AID);
end;

function TContestSubmissionManager.SubmissionProblemID(AID: integer): integer;
begin
  Result := inherited SubmissionProblemID(AID);
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

function TContestSubmissionManager.ContestFilter(AID: integer; AObject: TObject): boolean;
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
  Result := Manager as TBaseContestManager;
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

function TContestProblem.GetContestId: integer;
begin
  if FContest = nil then
    Result := -1
  else
    Result := FContest.ID;
end;

constructor TContestProblem.Create(const AName: string;
  AManager: TEditableManager);
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

constructor TContestTestProblemTransaction.Create(AManager: TEditableManager;
  AUser: TUser; AObject: TEditableObject);
begin
  inherited Create(AManager, AUser, AObject);
end;

function TContestTestProblemTransaction.CanTestProblem: boolean;
begin
  Result := inherited CanTestProblem;
  if Result then
    Exit;
  if Contest <> nil then
    Result := Contest.ParticipantCanSubmit(User.Info);
end;

function TContestTestProblemTransaction.CanReadData: boolean;
begin
  Result := inherited CanReadData;
  if Result then
    Exit;
  if Contest <> nil then
    Result := Contest.ParticipantCanView(User.Info);
end;

{ TContestProblemSubmissionSession }

function TContestProblemSubmissionSession.DoCreateProblemFromSubmission(AID: integer): TTestableProblem;
var
  ContestID: integer;
  Contest: TBaseContest;
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
begin
  Result := inherited CanReadSubmission(AProblem, AOwner);
  if Result then
    Exit;
  if (AOwner.ID = User.ID) then
  begin
    Contest := (AProblem as TContestProblem).Contest;
    if Contest <> nil then
      Result := Contest.HasParticipant(User.Info);
  end;
end;

function TContestProblemSubmissionSession.CanRejudgeSubmission(
  AProblem: TTestableProblem): boolean;
var
  Contest: TBaseContest;
begin
  Result := inherited CanRejudgeSubmission(AProblem);
  if Result then
    Exit;
  Contest := (AProblem as TContestProblem).Contest;
  if (Contest <> nil) and (User is TEditorUser) then
    Result := Contest.GetAccessRights(User as TEditorUser) in AccessCanWriteSet;
end;

function TContestProblemSubmissionSession.ListByContest(AContest: TBaseContest): TIdList;
begin
  with SubmissionManager do
    Filter(ListByContest(AContest), Self, @AvailableFilter);
end;

end.
