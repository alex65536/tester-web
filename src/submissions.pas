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
unit submissions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, submissionlanguages, users, datastorages, problems,
  testresults, jsonsaver, tswebobservers, filemanager, fgl, editableobjects,
  webstrconsts, tswebutils, serverconfig, LazFileUtils, tswebdirectories;

type
  ESubmissionAction = class(EUserAction);
  ESubmissionAccessDenied = class(ESubmissionAction);
  ESubmissionNotExist = class(ESubmissionAction);

  { TTestProblemTransaction }

  TTestProblemTransaction = class(TProblemTransaction)
  protected
    {%H-}constructor Create(AManager: TEditableManager; AUser: TUser;
      AObject: TEditableObject);
  public
    function CanWriteData: boolean; override;
    function CanTestProblem: boolean; virtual;
    function CanReadSubmissions(AOwner: TUserInfo): boolean; virtual;
  end;

  { TTestableProblem }

  TTestableProblem = class(TProblem)
  protected
    function UnpackedFileName: string; overload;
    function PropsFileName: string;
    {%H-}constructor Create(const AName: string; AManager: TEditableManager);
  public
    function CreateTestTransaction(AUser: TEditorUser): TTestProblemTransaction;
      virtual;
  end;

  { TTestableProblemManager }

  TTestableProblemManager = class(TProblemManager)
  protected
    function CreateObject(const AName: string): TEditableObject; override;
  end;

  TIdList = specialize TFPGList<integer>;

  TSubmissionManager = class;

  { TBaseSubmission }

  TBaseSubmission = class(TMessageAuthor)
  private
    FID: integer;
    FManager: TSubmissionManager;
    FStorage: TAbstractDataStorage;
    function GetFileName: string;
    function GetFinished: boolean;
    function GetLanguage: TSubmissionLanguage;
    function GetOwner: TUserInfo;
    function GetOwnerName: string;
    function GetProblem: TTestableProblem;
    function GetProblemName: string;
    function GetResultsFileName: string;
    function GetSubmitTime: TDateTime;
    function GetSuccess: boolean;
  protected
    property Storage: TAbstractDataStorage read FStorage;
    function FilesLocation: string; virtual;
    function SectionName: string;
    function FullKeyName(const Key: string): string;
    {%H-}constructor Create(AManager: TSubmissionManager; AID: integer);
  public
    property Manager: TSubmissionManager read FManager;
    property ID: integer read FID;
    property Language: TSubmissionLanguage read GetLanguage;
    property FileName: string read GetFileName;
    property ResultsFileName: string read GetResultsFileName;
    property SubmitTime: TDateTime read GetSubmitTime;
    property Owner: TUserInfo read GetOwner;
    property OwnerName: string read GetOwnerName;
    property Problem: TTestableProblem read GetProblem;
    property ProblemName: string read GetProblemName;
    property Finished: boolean read GetFinished;
    property Success: boolean read GetSuccess;
    function SourceCode: TStringList;
    constructor Create;
  end;

  { TTestSubmission }

  TTestSubmission = class(TBaseSubmission)
  protected
    function GetUnpackedFileName: string;
    function GetPropsFileName: string;
    procedure Prepare; virtual;
    procedure UpdateSubmitTime;
    procedure Finish(ASuccess: boolean);
    procedure Unfinish;
  public
    procedure AddFile(ALanguage: TSubmissionLanguage; const AFile: string);
  end;

  { TViewSubmission }

  TViewSubmission = class(TBaseSubmission)
  private
    FResults: TTestedProblem;
  protected
    procedure LoadResults; virtual;
    procedure HandleSelfDeletion; virtual;
    {%H-}constructor Create(AManager: TSubmissionManager; AID: integer);
  public
    property Results: TTestedProblem read FResults;
    destructor Destroy; override;
  end;

  { TTestSubmissionList }

  TTestSubmissionList = class(specialize TFPGObjectList<TTestSubmission>)
  public
    function FindByID(AID: integer): TTestSubmission;
  end;

  { TTestSubmissionMessage }

  TTestSubmissionMessage = class(TAuthorMessage)
  private
    FSubmission: TTestSubmission;
  public
    property Submission: TTestSubmission read FSubmission;
    function AddSubmission(ASubmission: TTestSubmission): TTestSubmissionMessage;
  end;

  TSubmissionTestedMessage = class(TTestSubmissionMessage);
  TSubmissionDeletingPoolMessage = class(TTestSubmissionMessage);

  TSubmissionQueue = class;

  { TSubmissionPool }

  TSubmissionPool = class(TMessageAuthor)
  private
    FList: TTestSubmissionList;
    FMaxPoolSize: integer;
    FQueue: TSubmissionQueue;
    FStorage: TAbstractDataStorage;
    function GetSubmissionCount: integer;
    function GetSubmissions(I: integer): TTestSubmission;
  protected
    property Storage: TAbstractDataStorage read FStorage;
    procedure DoAdd(ASubmission: TTestSubmission); virtual; abstract;
    procedure DoDelete(ASubmission: TTestSubmission); virtual; abstract;
    procedure InternalDelete(ASubmission: TTestSubmission);
    procedure InternalDeleteAndFree(ASubmission: TTestSubmission);
    procedure TriggerTestingFinished(ASubmission: TTestSubmission);
    procedure RevertToQueue;
    {%H-}constructor Create(AQueue: TSubmissionQueue; AMaxPoolSize: integer = 0);
  public
    property MaxPoolSize: integer read FMaxPoolSize;
    property Submissions[I: integer]: TTestSubmission read GetSubmissions; default;
    property SubmissionCount: integer read GetSubmissionCount;
    property Queue: TSubmissionQueue read FQueue;
    function AddSubmission(ASubmission: TTestSubmission): boolean;
    function DeleteSubmission(AID: integer): boolean;
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
  end;

  { TSubmissionQueue }

  TSubmissionQueue = class(TMessageAuthor, IMessageSubscriber, IFPObserver)
  private
    FList: TTestSubmissionList;
    FManager: TSubmissionManager;
    FPool: TSubmissionPool;
    FStorage: TAbstractDataStorage;
    FDestructing: boolean;
    function GetSubmissionCount: integer;
    function GetSubmissions(I: integer): TTestSubmission;
  protected
    property Storage: TAbstractDataStorage read FStorage;
    function TriggerAddToPool: boolean;
    function NextSubmission: TTestSubmission;
    function CreatePool: TSubmissionPool; virtual; abstract;
    procedure Reload; virtual;
    procedure Commit; virtual;
    function SubmissionNode(ASubmission: TTestSubmission): string;
    procedure MessageReceived(AMessage: TAuthorMessage);
    procedure FPOObservedChanged({%H-}ASender: TObject;
      Operation: TFPObservedOperation; Data: Pointer);
    {%H-}constructor Create(AManager: TSubmissionManager);
  public
    property Submissions[I: integer]: TTestSubmission read GetSubmissions; default;
    property SubmissionCount: integer read GetSubmissionCount;
    property Manager: TSubmissionManager read FManager;
    property Pool: TSubmissionPool read FPool;
    procedure AddSubmission(ASubmission: TTestSubmission);
    procedure DeleteSubmission(AID: integer);
    procedure Clear;
    constructor Create;
    procedure BeforeDestruction; override;
    destructor Destroy; override;
  end;

  { TSubmissionManager }

  TSubmissionManager = class(TMessageAuthor, IMessageSubscriber)
  private
    FQueue: TSubmissionQueue;
    FStorage: TAbstractDataStorage;
  protected type
    TSubmissionFilter = function(AID: integer; AObject: TObject): boolean of object;
  protected
    property Storage: TAbstractDataStorage read FStorage;
    function NextID: integer;
    function SubmissionSectionName(AID: integer): string;
    function OwnerSectionName(AID: integer): string;
    function ProblemSectionName(AID: integer): string;
    function CreateStorage: TAbstractDataStorage; virtual;
    function GetProblemManager: TTestableProblemManager; virtual; abstract;
    function CreateQueue: TSubmissionQueue; virtual; abstract;
    function DoCreateTestSubmission(AID: integer): TTestSubmission; virtual;
    function DoCreateViewSubmission(AID: integer): TViewSubmission; virtual;
    procedure DeleteSubmission(AID: integer); virtual;
    procedure HandleUserDeleting(AInfo: TUserInfo); virtual;
    procedure HandleProblemDeleting(AProblem: TTestableProblem); virtual;
    procedure ResumeCreateSubmission(AID: integer); virtual;
    function SubmissionOwnerID(AID: integer): integer;
    function SubmissionProblemID(AID: integer): integer;
    function StrListToIdList(AList: TStringList): TIdList;

    function ListAll: TIdList;
    function ListByOwner(AInfo: TUserInfo): TIdList;
    function ListByProblem(AProblem: TTestableProblem): TIdList;

    function Filter(AList: TIdList; AObject: TObject; AFilter: TSubmissionFilter): TIdList;
    function ProblemFilter(AID: integer; AObject: TObject): boolean;
    function AvailableFilter(AID: integer; AObject: TObject): boolean;

    function CanAccessSubmission(AID: integer; ATransaction: TTestProblemTransaction): boolean;
    procedure MessageReceived(AMessage: TAuthorMessage);
  public
    property ProblemManager: TTestableProblemManager read GetProblemManager;
    property Queue: TSubmissionQueue read FQueue;
    procedure CreateSubmission(ATransaction: TTestProblemTransaction;
      ALanguage: TSubmissionLanguage; const AFileName: string);
    function GetSubmission(AID: integer; ATransaction: TTestProblemTransaction): TViewSubmission;
    function SubmissionExists(AID: integer): boolean;
    function ListByOwner(AUser: TUser): TIdList;
    function ListByOwner(AUser: TUser; AProblem: TTestableProblem): TIdList;
    function ListAvailable(ATransaction: TTestProblemTransaction): TIdList;
    function ListAvailable(ATransaction: TTestProblemTransaction;
      AProblem: TTestableProblem): TIdList;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TTestSubmissionList }

function TTestSubmissionList.FindByID(AID: integer): TTestSubmission;
var
  I: integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].ID = AID then
    begin
      Result := Items[I];
      Exit;
    end;
end;

{ TTestProblemTransaction }

constructor TTestProblemTransaction.Create(AManager: TEditableManager;
  AUser: TUser; AObject: TEditableObject);
begin
  inherited Create(AManager, AUser, AObject);
end;

function TTestProblemTransaction.CanWriteData: boolean;
begin
  Result := False;
end;

function TTestProblemTransaction.CanTestProblem: boolean;
begin
  Result := CanReadData;
end;

function TTestProblemTransaction.CanReadSubmissions(AOwner: TUserInfo): boolean;
begin
  Result := True;
  if CanReadData and (AOwner.ID = User.ID) then
    Exit;
  if AccessLevel in AccessCanWriteSet then
    Exit;
  Result := False;
end;

{ TSubmissionManager }

function TSubmissionManager.NextID: integer;
begin
  Result := Storage.ReadInteger('lastId', 0);
  Inc(Result);
  Storage.WriteInteger('lastId', Result);
end;

function TSubmissionManager.SubmissionSectionName(AID: integer): string;
begin
  Result := 'sids.' + Id2Str(AID);
end;

function TSubmissionManager.OwnerSectionName(AID: integer): string;
begin
  Result := 'owners.' + Id2Str(AID);
end;

function TSubmissionManager.ProblemSectionName(AID: integer): string;
begin
  Result := 'problems.' + Id2Str(AID);
end;

function TSubmissionManager.CreateStorage: TAbstractDataStorage;
begin
  Result := TXmlDataStorage.Create('submissions');
end;

function TSubmissionManager.DoCreateTestSubmission(AID: integer): TTestSubmission;
begin
  Result := TTestSubmission.Create(Self, AID);
end;

function TSubmissionManager.DoCreateViewSubmission(AID: integer): TViewSubmission;
begin
  Result := TViewSubmission.Create(Self, AID);
end;

procedure TSubmissionManager.DeleteSubmission(AID: integer);
var
  Submission: TViewSubmission;
begin
  // ask the queue to delete
  Queue.DeleteSubmission(AID);
  // delete files using HandleDeletion
  Submission := DoCreateViewSubmission(AID);
  try
    Submission.HandleSelfDeletion;
  finally
    FreeAndNil(Submission);
  end;
  // remove from storage
  Storage.DeleteVariable(OwnerSectionName(SubmissionOwnerID(AID)) + '.' + Id2Str(AID));
  Storage.DeleteVariable(OwnerSectionName(SubmissionProblemID(AID)) + '.' +  Id2Str(AID));
  Storage.DeletePath(SubmissionSectionName(AID));
end;

procedure TSubmissionManager.HandleUserDeleting(AInfo: TUserInfo);
var
  List: TIdList;
  ID: integer;
begin
  List := ListByOwner(AInfo);
  try
    for ID in List do
      DeleteSubmission(ID);
  finally
    FreeAndNil(List);
  end;
end;

procedure TSubmissionManager.HandleProblemDeleting(AProblem: TTestableProblem);
var
  List: TIdList;
  ID: integer;
begin
  List := ListByProblem(AProblem);
  try
    for ID in List do
      DeleteSubmission(ID);
  finally
    FreeAndNil(List);
  end;
end;

procedure TSubmissionManager.ResumeCreateSubmission(AID: integer);
begin
  Queue.AddSubmission(DoCreateTestSubmission(AID));
end;

function TSubmissionManager.SubmissionOwnerID(AID: integer): integer;
begin
  Result := Storage.ReadInteger(SubmissionSectionName(AID) + '.ownerId', -1);
end;

function TSubmissionManager.SubmissionProblemID(AID: integer): integer;
begin
  Result := Storage.ReadInteger(SubmissionSectionName(AID) + '.problemId', -1);
end;

function TSubmissionManager.StrListToIdList(AList: TStringList): TIdList;
var
  IdStr: string;
begin
  Result := TIdList.Create;
  try
    for IdStr in AList do
      Result.Add(Str2Id(IdStr));
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TSubmissionManager.ListAll: TIdList;
var
  StrList: TStringList;
begin
  StrList := Storage.GetChildElements('sids');
  try
    Result := StrListToIdList(StrList);
  finally
    FreeAndNil(StrList);
  end;
end;

function TSubmissionManager.ListByOwner(AInfo: TUserInfo): TIdList;
var
  StrList: TStringList;
begin
  StrList := Storage.GetChildElements(OwnerSectionName(AInfo.ID));
  try
    Result := StrListToIdList(StrList);
  finally
    FreeAndNil(StrList);
  end;
end;

function TSubmissionManager.ListByProblem(AProblem: TTestableProblem): TIdList;
var
  StrList: TStringList;
begin
  StrList := Storage.GetChildElements(ProblemSectionName(AProblem.ID));
  try
    Result := StrListToIdList(StrList);
  finally
    FreeAndNil(StrList);
  end;
end;

function TSubmissionManager.Filter(AList: TIdList; AObject: TObject;
  AFilter: TSubmissionFilter): TIdList;
var
  ID: integer;
begin
  Result := TIdList.Create;
  try
    for ID in AList do
      if AFilter(ID, AObject) then
        Result.Add(ID);
  except
    FreeAndNil(Result);
    FreeAndNil(AList);
    raise;
  end;
  FreeAndNil(AList);
end;

function TSubmissionManager.ProblemFilter(AID: integer; AObject: TObject): boolean;
begin
  Result := AID = (AObject as TTestableProblem).ID;
end;

function TSubmissionManager.AvailableFilter(AID: integer; AObject: TObject): boolean;
begin
  Result := CanAccessSubmission(AID, (AObject as TTestProblemTransaction));
end;

function TSubmissionManager.CanAccessSubmission(AID: integer;
  ATransaction: TTestProblemTransaction): boolean;
var
  Info: TUserInfo;
begin
  Info := UserManager.GetUserInfo(SubmissionOwnerID(AID));
  try
    Result := ATransaction.CanReadSubmissions(Info);
  finally
    FreeAndNil(Info);
  end;
end;

procedure TSubmissionManager.MessageReceived(AMessage: TAuthorMessage);
begin
  if AMessage is TUserDeletingMessage then
    HandleUserDeleting((AMessage as TUserDeletingMessage).Info)
  else if AMessage is TEditableDeletingMessage then
    HandleProblemDeleting((AMessage as TEditableDeletingMessage).EditableObject as TTestableProblem);
end;

procedure TSubmissionManager.CreateSubmission(ATransaction: TTestProblemTransaction;
  ALanguage: TSubmissionLanguage; const AFileName: string);
var
  ID: integer;
  User: TUser;
  Problem: TTestableProblem;
  Submission: TTestSubmission;
begin
  if not ATransaction.CanTestProblem then
    raise ESubmissionAccessDenied.Create(SAccessDenied);
  // determine data
  Problem := ATransaction.Problem as TTestableProblem;
  User := ATransaction.User;
  ID := NextID;
  // create submission
  Submission := DoCreateTestSubmission(ID);
  try
    // fill owner & problem
    Storage.WriteString(SubmissionSectionName(ID) + '.ownerId', Id2Str(User.ID));
    Storage.WriteBool(OwnerSectionName(User.ID) + '.' + Id2Str(ID), True);
    Storage.WriteString(SubmissionSectionName(ID) + '.problemId', Id2Str(Problem.ID));
    Storage.WriteBool(ProblemSectionName(Problem.ID) + '.' + Id2Str(ID), True);
    // prepare submission for adding to queue
    Submission.UpdateSubmitTime;
    Submission.AddFile(ALanguage, AFileName);
    // add to queue
    Queue.AddSubmission(Submission);
  except
    FreeAndNil(Submission);
    raise;
  end;
end;

function TSubmissionManager.GetSubmission(AID: integer;
  ATransaction: TTestProblemTransaction): TViewSubmission;
begin
  // check for validness
  if not SubmissionExists(AID) then
    raise ESubmissionNotExist.CreateFmt(SSubmissionDoesNotExist, [AID]);
  if not CanAccessSubmission(AID, ATransaction) then
    raise ESubmissionAccessDenied.Create(SAccessDenied);
  // create submission instance
  Result := DoCreateViewSubmission(AID);
  try
    Result.LoadResults;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TSubmissionManager.SubmissionExists(AID: integer): boolean;
begin
  Result := Storage.VariableExists(SubmissionSectionName(AID) + '.ownerId');
end;

function TSubmissionManager.ListByOwner(AUser: TUser): TIdList;
begin
  Result := ListByOwner(AUser.Info);
end;

function TSubmissionManager.ListByOwner(AUser: TUser; AProblem: TTestableProblem): TIdList;
begin
  Result := Filter(ListByOwner(AUser), AProblem, @ProblemFilter);
end;

function TSubmissionManager.ListAvailable(ATransaction: TTestProblemTransaction): TIdList;
begin
  Result := Filter(ListAll, ATransaction, @AvailableFilter);
end;

function TSubmissionManager.ListAvailable(ATransaction: TTestProblemTransaction;
  AProblem: TTestableProblem): TIdList;
begin
  Result := Filter(ListByProblem(AProblem), ATransaction, @AvailableFilter);
end;

constructor TSubmissionManager.Create;
begin
  FStorage := CreateStorage;
  FQueue := CreateQueue;
  UserManager.Subscribe(Self);
  ProblemManager.Subscribe(Self);
  Queue.Subscribe(Self);
end;

destructor TSubmissionManager.Destroy;
begin
  FreeAndNil(FStorage);
  FreeAndNil(FQueue);
  inherited Destroy;
end;

{ TSubmissionQueue }

function TSubmissionQueue.GetSubmissionCount: integer;
begin
  Result := FList.Count;
end;

function TSubmissionQueue.GetSubmissions(I: integer): TTestSubmission;
begin
  Result := FList[I];
end;

function TSubmissionQueue.TriggerAddToPool: boolean;
var
  SubmissionToTest: TTestSubmission;
begin
  Result := False;
  if FDestructing then
    Exit;
  SubmissionToTest := NextSubmission;
  if SubmissionToTest = nil then
    Exit;
  if not Pool.AddSubmission(SubmissionToTest) then
    Exit;
  FList.Remove(SubmissionToTest);
  Result := True;
end;

function TSubmissionQueue.NextSubmission: TTestSubmission;
var
  I: integer;
begin
  if FList.Count = 0 then
    Exit(nil);
  Result := FList[0];
  for I := 1 to FList.Count - 1 do
    if FList[I].ID < Result.ID then
      Result := FList[I];
end;

procedure TSubmissionQueue.Reload;
var
  StrList: TStringList;
  StrId: string;
begin
  StrList := Storage.GetChildElements('queue');
  try
    for StrId in StrList do
      Manager.ResumeCreateSubmission(Str2Id(StrId));
  finally
    FreeAndNil(StrList);
  end;
end;

procedure TSubmissionQueue.Commit;
var
  Submission: TTestSubmission;
begin
  // remove old section
  Storage.DeletePath('queue');
  // append submissions
  for Submission in FList do
    Storage.WriteBool(SubmissionNode(Submission), True);
end;

function TSubmissionQueue.SubmissionNode(ASubmission: TTestSubmission): string;
begin
  Result := 'queue.' + Id2Str(ASubmission.ID);
end;

procedure TSubmissionQueue.MessageReceived(AMessage: TAuthorMessage);
begin
  if (AMessage is TSubmissionDeletingPoolMessage) or
    (AMessage is TSubmissionTestedMessage) then
  begin
    TriggerAddToPool;
    Broadcast(AMessage);
  end;
end;

procedure TSubmissionQueue.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  if (Operation = ooCustom) and ({%H-}PtrUInt(Data) = DS_CODE_COMMITING) and
    (not FDestructing) then
    Commit;
end;

constructor TSubmissionQueue.Create(AManager: TSubmissionManager);
begin
  FDestructing := False;
  FList := TTestSubmissionList.Create(False);
  FManager := AManager;
  FPool := CreatePool;
  FStorage := Manager.Storage;
  Pool.Subscribe(Self);
  Storage.FPOAttachObserver(Self);
end;

procedure TSubmissionQueue.AddSubmission(ASubmission: TTestSubmission);
begin
  FList.Add(ASubmission);
  TriggerAddToPool;
end;

procedure TSubmissionQueue.DeleteSubmission(AID: integer);
var
  Submission: TTestSubmission;
begin
  Submission := FList.FindByID(AID);
  if Submission <> nil then
  begin
    FList.Remove(Submission);
    FreeAndNil(Submission);
  end
  else
    Pool.DeleteSubmission(AID);
  TriggerAddToPool;
end;

procedure TSubmissionQueue.Clear;
var
  Submission: TTestSubmission;
begin
  while FList.Count > 0 do
  begin
    Submission := FList.Last;
    FList.Delete(FList.Count - 1);
    FreeAndNil(Submission);
  end;
end;

procedure TSubmissionQueue.BeforeDestruction;
begin
  FDestructing := True;
  Pool.RevertToQueue;
  inherited BeforeDestruction;
end;

constructor TSubmissionQueue.Create;
begin
  // we don't want the queue to be created publicly!
  raise EInvalidOperation.CreateFmt(SCreationPublic, [ClassName]);
end;

destructor TSubmissionQueue.Destroy;
begin
  if Storage <> nil then
    Storage.FPODetachObserver(Self);
  if FList <> nil then
    Clear;
  FreeAndNil(FList);
  FreeAndNil(FPool);
  inherited Destroy;
end;

{ TSubmissionPool }

function TSubmissionPool.GetSubmissionCount: integer;
begin
  Result := FList.Count;
end;

function TSubmissionPool.GetSubmissions(I: integer): TTestSubmission;
begin
  Result := FList[I];
end;

procedure TSubmissionPool.InternalDelete(ASubmission: TTestSubmission);
begin
  FList.Remove(ASubmission);
  DoDelete(ASubmission);
end;

procedure TSubmissionPool.InternalDeleteAndFree(ASubmission: TTestSubmission);
begin
  Broadcast(TSubmissionDeletingPoolMessage.Create.AddSubmission(ASubmission)
    .AddSender(Self).Lock);
  InternalDelete(ASubmission);
  FreeAndNil(ASubmission);
end;

procedure TSubmissionPool.TriggerTestingFinished(ASubmission: TTestSubmission);
begin
  Broadcast(TSubmissionTestedMessage.Create.AddSubmission(ASubmission)
    .AddSender(Self).Lock);
  if not ASubmission.Finished then
    ASubmission.Finish(True);
  FList.Remove(ASubmission);
  FreeAndNil(ASubmission);
end;

procedure TSubmissionPool.RevertToQueue;
var
  Submission: TTestSubmission;
begin
  while FList.Count > 0 do
  begin
    Submission := FList.Last;
    InternalDelete(Submission);
    Queue.AddSubmission(Submission);
  end;
end;

constructor TSubmissionPool.Create(AQueue: TSubmissionQueue; AMaxPoolSize: integer);
begin
  FQueue := AQueue;
  if AMaxPoolSize = 0 then
    AMaxPoolSize := Config.Testing_MaxPoolSize;
  FMaxPoolSize := AMaxPoolSize;
  FStorage := FQueue.Storage;
  FList := TTestSubmissionList.Create(False);
end;

function TSubmissionPool.AddSubmission(ASubmission: TTestSubmission): boolean;
begin
  if FList.Count >= MaxPoolSize then
    Exit(False);
  Result := True;
  ASubmission.Prepare;
  FList.Add(ASubmission);
  DoAdd(ASubmission);
end;

function TSubmissionPool.DeleteSubmission(AID: integer): boolean;
var
  Submission: TTestSubmission;
begin
  Submission := FList.FindByID(AID);
  Result := Submission <> nil;
  if Result then
    InternalDeleteAndFree(Submission);
end;

procedure TSubmissionPool.Clear;
begin
  while FList.Count > 0 do
    InternalDeleteAndFree(FList.Last);
end;

constructor TSubmissionPool.Create;
begin
  // pool cannot be created publicly!
  raise EInvalidOperation.CreateFmt(SCreationPublic, [ClassName]);
end;

destructor TSubmissionPool.Destroy;
begin
  if FList <> nil then
    Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

{ TTestSubmissionMessage }

function TTestSubmissionMessage.AddSubmission(ASubmission: TTestSubmission): TTestSubmissionMessage;
begin
  NeedsUnlocked;
  FSubmission := ASubmission;
  Result := Self;
end;

{ TViewSubmission }

procedure TViewSubmission.LoadResults;
var
  FileStream: TFileStream;
  S: string;
begin
  FreeAndNil(FResults);
  if not FileExistsUTF8(ResultsFileName) then
    Exit;
  FResults := TTestedProblem.Create;
  try
    FileStream := WaitForFile(ResultsFileName, fmOpenRead or fmShareExclusive);
    try
      SetLength(S, FileStream.Size);
      FileStream.Read(S[1], FileStream.Size);
      LoadTestedProblemFromJSONStr(S, Results);
    finally
      FreeAndNil(FileStream);
    end;
  except
    FreeAndNil(FResults);
    raise;
  end;
end;

procedure TViewSubmission.HandleSelfDeletion;
begin
  TryDeleteFile(FileName);
  TryDeleteFile(ResultsFileName);
end;

constructor TViewSubmission.Create(AManager: TSubmissionManager; AID: integer);
begin
  inherited Create(AManager, AID);
  FResults := nil;
end;

destructor TViewSubmission.Destroy;
begin
  FreeAndNil(FResults);
  inherited Destroy;
end;

{ TTestSubmission }

function TTestSubmission.GetUnpackedFileName: string;
begin
  Result := Problem.UnpackedFileName;
end;

function TTestSubmission.GetPropsFileName: string;
begin
  Result := Problem.PropsFileName;
end;

procedure TTestSubmission.Prepare;
begin
  Unfinish;
end;

procedure TTestSubmission.UpdateSubmitTime;
begin
  Storage.WriteFloat(FullKeyName('time'), Now);
end;

procedure TTestSubmission.Finish(ASuccess: boolean);
begin
  Storage.WriteBool(FullKeyName('finished'), True);
  Storage.WriteBool(FullKeyName('success'), ASuccess);
end;

procedure TTestSubmission.Unfinish;
begin
  Storage.WriteBool(FullKeyName('finished'), False);
  Storage.DeleteVariable(FullKeyName('success'));
end;

procedure TTestSubmission.AddFile(ALanguage: TSubmissionLanguage; const AFile: string);
begin
  Storage.WriteString(FullKeyName('language'), LanguageToStr(ALanguage));
  CopyReplaceFile(AFile, FileName);
end;

{ TBaseSubmission }

function TBaseSubmission.GetFileName: string;
begin
  Result := AppendPathDelim(FilesLocation) + IntToStr(ID) + LanguageInnerExts[Language];
end;

function TBaseSubmission.GetFinished: boolean;
begin
  Result := Storage.ReadBool(FullKeyName('finished'), False);
end;

function TBaseSubmission.GetLanguage: TSubmissionLanguage;
begin
  Result := StrToLanguage(Storage.ReadString(FullKeyName('language'), ''));
end;

function TBaseSubmission.GetOwner: TUserInfo;
var
  OwnerID: integer;
begin
  OwnerID := Manager.SubmissionOwnerID(ID);
  Result := UserManager.GetUserInfo(OwnerID);
end;

function TBaseSubmission.GetOwnerName: string;
begin
  with GetOwner do
    try
      Result := Username;
    finally
      Free;
    end;
end;

function TBaseSubmission.GetProblem: TTestableProblem;
begin
  Result := Manager.ProblemManager.GetObject(ProblemName) as TTestableProblem;
end;

function TBaseSubmission.GetProblemName: string;
var
  ProblemID: integer;
begin
  ProblemID := Manager.SubmissionProblemID(ID);
  Result := Manager.ProblemManager.IdToObjectName(ProblemID);
end;

function TBaseSubmission.GetResultsFileName: string;
begin
  Result := AppendPathDelim(FilesLocation) + IntToStr(ID) + '.json';
end;

function TBaseSubmission.GetSubmitTime: TDateTime;
begin
  Result := Storage.ReadFloat(FullKeyName('time'), 0.0);
end;

function TBaseSubmission.GetSuccess: boolean;
begin
  Result := Storage.ReadBool(FullKeyName('success'), True);
end;

function TBaseSubmission.FilesLocation: string;
begin
  Result := ExpandInternalDirLocation('submissions');
end;

function TBaseSubmission.SectionName: string;
begin
  Result := Manager.SubmissionSectionName(ID);
end;

function TBaseSubmission.FullKeyName(const Key: string): string;
begin
  Result := SectionName + '.' + Key;
end;

constructor TBaseSubmission.Create(AManager: TSubmissionManager; AID: integer);
begin
  FID := AID;
  FManager := AManager;
  FStorage := AManager.Storage;
end;

function TBaseSubmission.SourceCode: TStringList;
begin
  Result := TStringList.Create;
  try
    Result.LoadFromFile(FileName);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

constructor TBaseSubmission.Create;
begin
  // we don't want them to be created publicly!
  raise EInvalidOperation.CreateFmt(SCreationPublic, [ClassName]);
end;

{ TTestableProblemManager }

function TTestableProblemManager.CreateObject(const AName: string): TEditableObject;
begin
  Result := TTestableProblem.Create(AName, Self);
end;

{ TTestableProblem }

function TTestableProblem.UnpackedFileName: string;
begin
  Result := UnpackedFileName(True);
end;

function TTestableProblem.PropsFileName: string;
begin
  Result := inherited PropsFileName;
end;

constructor TTestableProblem.Create(const AName: string;
  AManager: TEditableManager);
begin
  inherited Create(AName, AManager);
end;

function TTestableProblem.CreateTestTransaction(AUser: TEditorUser): TTestProblemTransaction;
begin
  Result := TTestProblemTransaction.Create(Manager, AUser, Self);
end;

end.