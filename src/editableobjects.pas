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
unit editableobjects;

{$mode objfpc}{$H+}{$M+}

interface

uses
  Classes, SysUtils, TypInfo, webstrconsts, users;

type
  TEditableAccessRights = (erNone, erRead, erWrite, erOwner);
  TEditableAccessRightsSet = set of TEditableAccessRights;

const
  AccessCanReadSet = [erRead, erWrite, erOwner];
  AccessCanWriteSet = [erWrite, erOwner];

type

  { TEditorUser }

  TEditorUser = class(TUser)
  protected
    function DoGetRole: TUserRole; override;
  end;

  //TEditableObject = class;
  //TEditableManager = class;
  //
  //TEditableCustomSession = class
  //protected
  //  property Storage: TAbstractDataStorage read FStorage;
  //  {%H-}constructor Create(AManager: TEditableObjectManager; AUser: TEditorUser);
  //public
  //  property Manager: TEditableManager read FManager;
  //  property User: TEditorUser read FUser;
  //  constructor Create;
  //end;
  //
  //TEditableObjectSession = class(TEditableCustomSession)
  //protected
  //  {%H-}constructor Create(AManager: TEditableObjectManager; AObject: TEditableObject;
  //    AUser: TEditorUser);
  //public
  //  property EditableObject: TEditableObject read FEditableObject;
  //  function AccessLevel: TEditableAccessRights;
  //end;
  //
  //TEditableObjectAccessSession = class(TEditableObjectSession)
  //public
  //  function CanAddUser(Target: TUserInfo): boolean; virtual;
  //  function CanDeleteUser(Target: TUserInfo): boolean; virtual;
  //  function CanGrantUserAccess(Target: TUserInfo; AAccess: TEditableAccessRights): boolean; virtual;
  //  procedure AddUser(Target: TUserInfo);
  //  procedure DeleteUser(Target: TUserInfo);
  //  procedure GrantUserAccess(Target: TUserInfo; AAccess: TEditableAccessRights);
  //end;
  //
  //TEditableTransaction = class(TEditableObjectSession)
  //public
  //  function CanReadData: boolean; virtual;
  //  function CanWriteData: boolean; virtual;
  //  procedure Commit; virtual; abstract;
  //end;
  //
  //TEditableManagerSession = class(TEditableCustomSession)
  //public
  //  function CanCreateNewObject: boolean; virtual;
  //  function CanDeleteObject(const AName: string): boolean; virtual;
  //  function CreateNewObject(const AName: string): TEditableObject;
  //  procedure DeleteObject(const AName: string);
  //  function ListAvailableObjects: TStringList;
  //end;
  //
  //TEditableObject = class
  //protected
  //  property Manager: TEditableManager read FManager;
  //  property Storage: TAbstractDataStorage read FStorage;
  //  procedure AddUser(Target: TUserInfo); virtual;
  //  procedure DeleteUser(Target: TUserInfo); virtual;
  //  procedure GrantUserAccess(Target: TUserInfo; AAccess: TEditableAccessRights); virtual;
  //  procedure ValidateUser(AUserID: integer);
  //  procedure ValidateAllUsers;
  //  {%H-}constructor Create(const AName: string; AManager: TEditableManager);
  //public
  //  property Name: string read FName;
  //  property ID: integer read GetID;
  //  function ObjectAuthor: TEditorUser;
  //  function ObjectAuthorName: string;
  //  function GetAccessRights(const AUser: TEditorUser): TEditableAccessRights;
  //  function CreateAccessSession(AUser: TEditorUser): TEditableObjectAccessSession; virtual; abstract;
  //  function CreateTransaction(AUser: TEditorUser): TEditableTransaction; virtual; abstract;
  //  function ListUsers: TStringList; virtual;
  //  constructor Create;
  //end;
  //
  //TEditableManager = class
  //protected
  //  property Storage: TAbstractDataStorage read FStorage;
  //  function FullKeyName(const AName: string): string;
  //  function FullUserKeyName(const AName: string; AUserID: integer): string;
  //  function GetID: integer;
  //  procedure IncID;
  //  function CreateStorage: TAbstractDataStorage; virtual; abstract;
  //  function CreateObject(const AName: string): TEditableObject; virtual; abstract;
  //  function GetObjectAuthor(const AObjectName: string): TUserInfo;
  //  function CreateNewObject(AOwner: TEditorUser; const AName: string): TEditableObject; virtual;
  //  procedure DeleteObject(const AName: string); virtual;
  //public
  //  function ObjectExists(const AName: string): boolean;
  //  function GetObject(const AName: string): TEditableObject;
  //  function ListAvailableObjects(AUser: TEditorUser): TStringList; virtual;
  //  constructor Create;
  //  destructor Destroy; override;
  //end;

function AccessRightsToStr(ARights: TEditableAccessRights): string;
function StrToAccessRights(const S: string): TEditableAccessRights;

implementation

function AccessRightsToStr(ARights: TEditableAccessRights): string;
begin
  Result := GetEnumName(TypeInfo(TEditableAccessRights), Ord(ARights));
end;

function StrToAccessRights(const S: string): TEditableAccessRights;
var
  R: TEditableAccessRights;
begin
  for R in TEditableAccessRights do
    if AccessRightsToStr(R) = S then
      Exit(R);
  raise EConvertError.Create(SNoSuchAccessRights);
end;

{ TEditorUser }

function TEditorUser.DoGetRole: TUserRole;
begin
  Result := urEditor;
end;

initialization
  UserClass[urEditor] := TEditorUser;

end.

