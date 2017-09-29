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
  testresults, jsonsaver, tswebobservers, filemanager, fgl, editableobjects;

type
  ESubmission = class(Exception);
  ESubmissionAccessDenied = class(EUserAccessDenied);

  { TTestProblemTransaction }

  TTestProblemTransaction = class(TProblemTransaction)
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

  TBaseSubmission = class
  private
    FID: integer;
    FManager: TSubmissionManager;
    FUser: TUser;
    function GetFileName: string;
    function GetLanguage: TSubmissionLanguage;
    function GetOwner: TUserInfo;
    function GetOwnerName: string;
    function GetProblem: TTestableProblem;
    function GetProblemName: string;
    function GetResultsFileName: string;
  protected
    {%H-}constructor Create(AManager: TSubmissionManager; AID: integer);
  public
    property Manager: TSubmissionManager read FManager;
    property ID: integer read FID;
    property User: TUser read FUser;
    property Language: TSubmissionLanguage read GetLanguage;
    property FileName: string read GetFileName;
    property ResultsFileName: string read GetResultsFileName;
    property Owner: TUserInfo read GetOwner;
    property OwnerName: string read GetOwnerName;
    property Problem: TTestableProblem read GetProblem;
    property ProblemName: string read GetProblemName;
    function SourceCode: TStringList;
    constructor Create;
    destructor Destroy; override;
  end;

  { TTestSubmission }

  TTestSubmission = class(TBaseSubmission)
  public
    procedure AddFile(ALanguage: TSubmissionLanguage; const AFile: string);
  end;

  { TViewSubmission }

  TViewSubmission = class(TBaseSubmission)
  private
    FResults: TTestedProblem;
  protected
    procedure LoadResults; virtual;
    {%H-}constructor Create(AManager: TSubmissionManager; AID: integer; AUser: TUser);
  public
    property User: TUser read FUser;
    property Results: TTestedProblem read FResults;
    function CanAccess(AUser: TUser): boolean; virtual;
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

  TTestSubmissionList = specialize TFPGObjectList<TTestSubmission>;

  { TTestSubmissionMessage }

  TTestSubmissionMessage = class(TAuthorMessage)
  private
    FSubmission: TTestSubmission;
  public
    property Submission: TTestSubmission read FSubmission;
    function AddSubmission(ASubmission: TTestSubmission): TTestSubmissionMessage;
  end;

  TSubmissionTestedMessage = class(TTestSubmissionMessage);
  TSubmissionDeletedPoolMessage = class(TTestSubmissionMessage);

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
    procedure SetSubmissions(I: integer; AValue: TTestSubmission);
  protected
    property Storage: TAbstractDataStorage read FStorage;
    function GetUnpackedFileName(ASubmission: TTestSubmission): string;
    function GetPropsFileName(ASubmission: TTestSubmission): string;
    procedure DoAdd(ASubmission: TTestSubmission); virtual;
    procedure DoDelete(AID: integer); virtual;
    procedure TriggerTestingFinished;
    {%H-}constructor Create(AQueue: TSubmissionQueue; AMaxPoolSize: integer = 0);
  public
    property MaxPoolSize: integer read FMaxPoolSize;
    property Submissions[I: integer]: TTestSubmission
      read GetSubmissions write SetSubmissions; default;
    property SubmissionCount: integer read GetSubmissionCount;
    property Queue: TSubmissionQueue read FQueue;
    function AddSubmission(ASubmission: TTestSubmission): boolean;
    function DeleteSubmission(AID: integer): boolean;
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
  end;

  { TSubmissionQueue }

  TSubmissionQueue = class(TMessageAuthor, IMessageSubscriber)
  private
    FList: TTestSubmissionList;
    FDestructing: boolean;
    FManager: TSubmissionManager;
    FPool: TSubmissionPool;
    FStorage: TAbstractDataStorage;
    function GetSubmissionCount: integer;
    function GetSubmissions(I: integer): TTestSubmission;
    procedure SetSubmissions(I: integer; AValue: TTestSubmission);
  protected
    property Storage: TAbstractDataStorage read FStorage;
    procedure TriggerAddToPool;
    function CreateStorage: TAbstractDataStorage; virtual;
    function CreatePool: TSubmissionPool; virtual; abstract;
    procedure Reload; virtual;
    procedure Commit; virtual;
    procedure MessageReceived(AMessage: TAuthorMessage);
    {%H-}constructor Create(AManager: TSubmissionManager);
  public
    property Submissions[I: integer]: TTestSubmission
      read GetSubmissions write SetSubmissions; default;
    property SubmissionCount: integer read GetSubmissionCount;
    property Manager: TSubmissionManager read FManager;
    property Pool: TSubmissionPool read FPool;
    procedure AddSubmission(ASubmission: TTestSubmission);
    procedure DeleteSubmission(AID: integer);
    procedure Clear;
    procedure BeforeDestruction; override;
    constructor Create;
    destructor Destroy; override;
  end;

  { TSubmissionManager }

  TSubmissionManager = class(TMessageAuthor, IMessageSubscriber)
  private
    FQueue: TSubmissionQueue;
    FStorage: TAbstractDataStorage;
  protected
    property Storage: TAbstractDataStorage read FStorage;
    function CreateStorage: TAbstractDataStorage; virtual;
    function GetProblemManager: TTestableProblemManager; virtual; abstract;
    function CreateQueue: TSubmissionQueue; virtual; abstract;
    function DoCreateTestSubmission(AID: integer): TTestSubmission; virtual;
    function DoCreateViewSubmission(AID: integer; AUser: TUser): TViewSubmission; virtual;
    procedure DeleteSubmission(AID: integer); virtual;
    procedure HandleUserDeleted(AUser: TUser); virtual;
    procedure HandleProblemDeleted(AProblem: TProblem); virtual;
    function ResumeCreateSubmission(AID: integer): TTestSubmission; virtual;
    procedure MessageReceived(AMessage: TAuthorMessage);
  public
    property ProblemManager: TTestableProblemManager read GetProblemManager;
    property Queue: TSubmissionQueue read FQueue;
    function CreateSubmission(ATransaction: TTestProblemTransaction): TTestSubmission;
    function GetSubmission(ATransaction: TTestProblemTransaction): TTestSubmission;
    function ListSubmissions(AUser: TUser): TIdList;
    function ListSubmissions(AUser: TUser; AProblem: TTestableProblem): TIdList;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TSubmissionManager }

function TSubmissionManager.CreateStorage: TAbstractDataStorage;
begin

end;

function TSubmissionManager.DoCreateTestSubmission(AID: integer): TTestSubmission;
begin

end;

function TSubmissionManager.DoCreateViewSubmission(AID: integer;
  AUser: TUser): TViewSubmission;
begin

end;

procedure TSubmissionManager.DeleteSubmission(AID: integer);
begin

end;

procedure TSubmissionManager.HandleUserDeleted(AUser: TUser);
begin

end;

procedure TSubmissionManager.HandleProblemDeleted(AProblem: TProblem);
begin

end;

function TSubmissionManager.ResumeCreateSubmission(AID: integer): TTestSubmission;
begin

end;

procedure TSubmissionManager.MessageReceived(AMessage: TAuthorMessage);
begin

end;

function TSubmissionManager.CreateSubmission(ATransaction: TTestProblemTransaction): TTestSubmission;
begin

end;

function TSubmissionManager.GetSubmission(ATransaction: TTestProblemTransaction): TTestSubmission;
begin

end;

function TSubmissionManager.ListSubmissions(AUser: TUser): TIdList;
begin

end;

function TSubmissionManager.ListSubmissions(AUser: TUser;
  AProblem: TTestableProblem): TIdList;
begin

end;

constructor TSubmissionManager.Create;
begin

end;

destructor TSubmissionManager.Destroy;
begin
  inherited Destroy;
end;

{ TSubmissionQueue }

function TSubmissionQueue.GetSubmissionCount: integer;
begin

end;

function TSubmissionQueue.GetSubmissions(I: integer): TTestSubmission;
begin

end;

procedure TSubmissionQueue.SetSubmissions(I: integer; AValue: TTestSubmission);
begin

end;

procedure TSubmissionQueue.TriggerAddToPool;
begin

end;

function TSubmissionQueue.CreateStorage: TAbstractDataStorage;
begin

end;

procedure TSubmissionQueue.Reload;
begin

end;

procedure TSubmissionQueue.Commit;
begin

end;

procedure TSubmissionQueue.MessageReceived(AMessage: TAuthorMessage);
begin

end;

constructor TSubmissionQueue.Create(AManager: TSubmissionManager);
begin

end;

procedure TSubmissionQueue.AddSubmission(ASubmission: TTestSubmission);
begin

end;

procedure TSubmissionQueue.DeleteSubmission(AID: integer);
begin

end;

procedure TSubmissionQueue.Clear;
begin

end;

procedure TSubmissionQueue.BeforeDestruction;
begin
  inherited BeforeDestruction;
end;

constructor TSubmissionQueue.Create;
begin

end;

destructor TSubmissionQueue.Destroy;
begin
  inherited Destroy;
end;

{ TSubmissionPool }

function TSubmissionPool.GetSubmissionCount: integer;
begin

end;

function TSubmissionPool.GetSubmissions(I: integer): TTestSubmission;
begin

end;

procedure TSubmissionPool.SetSubmissions(I: integer; AValue: TTestSubmission);
begin

end;

function TSubmissionPool.GetUnpackedFileName(ASubmission: TTestSubmission): string;
begin

end;

function TSubmissionPool.GetPropsFileName(ASubmission: TTestSubmission): string;
begin

end;

procedure TSubmissionPool.DoAdd(ASubmission: TTestSubmission);
begin

end;

procedure TSubmissionPool.DoDelete(AID: integer);
begin

end;

procedure TSubmissionPool.TriggerTestingFinished;
begin

end;

constructor TSubmissionPool.Create(AQueue: TSubmissionQueue; AMaxPoolSize: integer);
begin

end;

function TSubmissionPool.AddSubmission(ASubmission: TTestSubmission): boolean;
begin

end;

function TSubmissionPool.DeleteSubmission(AID: integer): boolean;
begin

end;

procedure TSubmissionPool.Clear;
begin

end;

constructor TSubmissionPool.Create;
begin

end;

destructor TSubmissionPool.Destroy;
begin
  inherited Destroy;
end;

{ TTestSubmissionMessage }

function TTestSubmissionMessage.AddSubmission(ASubmission: TTestSubmission): TTestSubmissionMessage;
begin

end;

{ TViewSubmission }

procedure TViewSubmission.LoadResults;
begin

end;

constructor TViewSubmission.Create(AManager: TSubmissionManager; AID: integer; AUser: TUser);
begin

end;

function TViewSubmission.CanAccess(AUser: TUser): boolean;
begin

end;

procedure TViewSubmission.AfterConstruction;
begin
  inherited AfterConstruction;
end;

destructor TViewSubmission.Destroy;
begin
  inherited Destroy;
end;

{ TTestSubmission }

procedure TTestSubmission.AddFile(ALanguage: TSubmissionLanguage; const AFile: string);
begin

end;

{ TBaseSubmission }

function TBaseSubmission.GetFileName: string;
begin

end;

function TBaseSubmission.GetLanguage: TSubmissionLanguage;
begin

end;

function TBaseSubmission.GetOwner: TUserInfo;
begin

end;

function TBaseSubmission.GetOwnerName: string;
begin

end;

function TBaseSubmission.GetProblem: TTestableProblem;
begin

end;

function TBaseSubmission.GetProblemName: string;
begin

end;

function TBaseSubmission.GetResultsFileName: string;
begin

end;

constructor TBaseSubmission.Create(AManager: TSubmissionManager; AID: integer);
begin

end;

function TBaseSubmission.SourceCode: TStringList;
begin

end;

constructor TBaseSubmission.Create;
begin

end;

destructor TBaseSubmission.Destroy;
begin
  inherited Destroy;
end;

{ TTestableProblemManager }

function TTestableProblemManager.CreateObject(const AName: string): TEditableObject;
begin
  Result := inherited CreateObject(AName);
end;

{ TTestableProblem }

function TTestableProblem.UnpackedFileName: string;
begin

end;

function TTestableProblem.PropsFileName: string;
begin

end;

constructor TTestableProblem.Create(const AName: string;
  AManager: TEditableManager);
begin

end;

function TTestableProblem.CreateTestTransaction(AUser: TEditorUser): TTestProblemTransaction;
begin

end;

end.
