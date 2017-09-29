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

// TODO : Finish sumission logic !!!

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, submissionlanguages, users, datastorages, problems,
  testresults, jsonsaver, tswebobservers, filemanager, fgl;

//type
//  ESubmission = class(Exception);
//  ESubmissionAccessDenied = class(EUserAccessDenied);
//
//  TIdList = specialize TFPGList<integer>;
//
//  TSubmissionManager = class;
//
//  TBaseSubmission = class
//  protected
//    function GetLanguage: TSubmissionLanguage; virtual;
//    function GetFileName: string; virtual;
//    function GetOwner: TUserInfo; virtual;
//    constructor Create(AManager: TSubmissionManager; ATransaction: TProblemTransaction;
//      AID: integer);
//  public
//    property Manager: TSubmissionManager read FManager;
//    property Transaction: TProblemTransaction read FTransaction;
//    property ID: integer read FID;
//    property Language: TSubmissionLanguage read GetLanguage;
//    property FileName: string read GetFileName;
//    property Owner: TUserInfo read GetOwner;
//    property OwnerName: integer read GetOwnerName;
//    constructor Create;
//  end;
//
//  TTestSubmission = class(TBaseSubmission)
//  public
//    procedure AddFile(ALanguage: TSubmissionLanguage; const AFile: string);
//    destructor Destroy; override;
//  end;
//
//  TViewSubmission = class(TBaseSubmission)
//  protected
//    procedure LoadResults; virtual;
//    constructor Create(AManager: TSubmissionManager;
//      ATransaction: TProblemTransaction; AID: integer);
//  public
//    property Results: TTestedProblem read FResults;
//    function CanAccess(AUser: TUser): boolean; virtual;
//    procedure AfterConstruction; override;
//    destructor Destroy; override;
//  end;
//
//  TTestSubmissionList = specialize TFPGObjectList<TTestSubmission>;
//
//  TTestSubmissionMessage = class(TAuthorMessage)
//  public
//    property Submission: TTestSubmission read FSubmission;
//    function AddSubmission(ASubmission: TTestSubmission): TTestSubmissionMessage;
//  end;
//
//  TSubmissionTestedMessage = class(TTestSubmissionMessage);
//  TSubmissionDeletedPoolMessage = class(TTestSubmissionMessage);
//
//  TSubmissionPool = class(TMessageAuthor)
//  private
//    FList: TTestSubmissionList;
//  protected
//    property Storage: TAbstractDataStorage read FStorage;
//    procedure DoAdd(ASubmission: TTestSubmission); virtual;
//    procedure DoDelete(ASubmission: TTestSubmission); virtual;
//    procedure TestingFinished;
//    constructor Create(AQueue: TSubmissionQueue);
//  public
//    property Submissions[I: integer]: TTestSubmission read GetSubmissions
//      write SetSubmissions; default;
//    property SubmissionCount: integer read GetSubmissionCount;
//    property Queue: TSubmissionQueue read FQueue;
//    function AddSubmission(ASubmission: TTestSubmission): boolean;
//    function DeleteSubmission(ASubmission: TTestSubmission): boolean;
//    procedure Clear;
//    constructor Create;
//    destructor Destroy; override;
//  end;
//
//  TSubmissionQueue = class(TMessageAuthor)
//  private
//    FList: TTestSubmissionList;
//    FDeleting: integer;
//    procedure BeginDelete;
//    procedure EndDelete;
//  protected
//    property Storage: TAbstractDataStorage read FStorage;
//    procedure TriggerAddToPool;
//    function CreateStorage: TAbstractDataStorage; virtual;
//    function CreatePool: TSubmissionPool; virtual; abstract;
//    procedure Reload; virtual;
//    procedure Commit; virtual;
//    constructor Create(AManager: TSubmissionManager);
//  public
//    property Submissions[I: integer]: TTestSubmission read GetSubmissions
//      write SetSubmissions; default;
//    property SubmissionCount: integer read GetSubmissionCount;
//    property Manager: TSubmissionManager read FManager;
//    property Pool: TSubmissionPool read FPool;
//    procedure AddSubmission(ASubmission: TTestSubmission);
//    procedure DeleteSubmission(ASubmission: TTestSubmission);
//    procedure Clear;
//    procedure BeforeDestruction; override;
//    constructor Create;
//    destructor Destroy; override;
//  end;
//
//  TSubmissionManager = class(TMessageAuthor, IMessageSubscriber)
//  protected
//    property Storage: TAbstractDataStorage read FStorage;
//    function CreateStorage: TAbstractDataStorage; virtual;
//    function CreateQueue: TSubmissionQueue; virtual; abstract;
//    procedure DeleteSubmission(AID: integer); virtual;
//    procedure MessageReceived(AMessage: TAuthorMessage);
//  public
//    property Queue: TSubmissionQueue read FQueue;
//    function CreateSubmission(ATransaction: TProblemTransaction): TTestSubmission; virtual;
//    function GetSubmission(ATransaction: TProblemTransaction): TTestSubmission; virtual;
//    function ListSubmissions(AUser: TUser): TIdList;
//    function ListSubmissions(AUser: TUser; AProblem: TProblem): TIdList;
//    constructor Create;
//    destructor Destroy; override;
//  end;

implementation


end.

