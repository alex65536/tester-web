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
//  TSubmissionQueue = class
//  private
//
//  public
//
//  end;
//
//  TSubmissionBase = class
//  protected
//    function GetLanguage: TSubmissionLanguage; virtual;
//    function GetFileName: string; virtual;
//    function GetOwner: TUserInfo; virtual;
//    constructor Create(AUser: TUser; AProblem: TProblem; AID: integer);
//  public
//    property User: TUser read FUser;
//    property Problem: TProblem read FProblem;
//    property ID: integer read FID;
//    property Language: TSubmissionLanguage read GetLanguage;
//    property FileName: string read GetFileName;
//    property Owner: TUserInfo read GetOwner;
//    property OwnerName: integer read GetOwnerName;
//    constructor Create;
//  end;
//
//  TSubmissionCreator = class(TSubmissionBase, IMessageSubscriber)
//  protected
//    procedure MessageReceived(AMessage: TAuthorMessage);
//    constructor Create(AUser: TUser; AProblem: TProblem; AID: integer);
//  public
//    procedure AddFile(ALanguage: TSubmissionLanguage; const AFile: string);
//    procedure Stop; virtual;
//  end;
//
//  TSubmissionViewer = class(TSubmissionBase)
//  protected
//    procedure LoadResults; virtual;
//    constructor Create(AUser: TUser; AProblem: TProblem; AID: integer);
//  public
//    property Results: TTestedProblem read FResults;
//    function CanAccess(AUser: TUser): boolean; virtual;
//    destructor Destroy; override;
//  end;
//
//  TSubmissionManager = class(TMessageAuthor, IMessageSubscriber)
//  protected
//    property Storage: TAbstractDataStorage read FStorage;
//    function CreateStorage: TAbstractDataStorage; virtual;
//    procedure DeleteSubmission(AID: integer); virtual;
//    procedure MessageReceived(AMessage: TAuthorMessage);
//  public
//    function ListSubmissions(AUser: TUser): TIdList;
//    function ListSubmissions(AUser: TUser; AProblem: TProblem): TIdList;
//    constructor Create;
//    destructor Destroy; override;
//  end;
//
//  TSubmissionMessage = class(TAuthorMessage)
//  private
//    FID: integer;
//  public
//    property ID: integer read FID;
//    function AddID(AID: integer): TSubmissionMessage;
//  end;

implementation

end.

