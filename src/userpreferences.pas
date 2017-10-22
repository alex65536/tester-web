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
unit userpreferences;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, datastorages, submissionlanguages, webstrconsts, allusers,
  users, tswebobservers, tswebutils;

type
  TUserPreferencesManager = class;

  { TUserPreferences }

  TUserPreferences = class
  private
    FLastLanguage: TSubmissionLanguage;
    FManager: TUserPreferencesManager;
    FStorage: TAbstractDataStorage;
    FUser: TUser;
  protected
    property Storage: TAbstractDataStorage read FStorage;
    function UserSectionName: string;
    procedure DoReload; virtual;
    procedure DoCommit; virtual;
    {%H-}constructor Create(AManager: TUserPreferencesManager; AUser: TUser);
  public
    property Manager: TUserPreferencesManager read FManager;
    property User: TUser read FUser;
    property LastLanguage: TSubmissionLanguage read FLastLanguage write FLastLanguage;
    procedure Reload;
    procedure Commit;
    procedure Validate; virtual;
    procedure AfterConstruction; override;
    constructor Create;
  end;

  { TUserPreferencesManager }

  TUserPreferencesManager = class(TObject, IMessageSubscriber)
  private
    FStorage: TAbstractDataStorage;
  protected
    property Storage: TAbstractDataStorage read FStorage;
    function UserSectionName(AUserInfo: TUserInfo): string;
    procedure HandleUserDeleting(AInfo: TUserInfo); virtual;
    procedure MessageReceived(AMessage: TAuthorMessage); virtual;
  public
    function GetPreferences(AUser: TUser): TUserPreferences; virtual;
    constructor Create;
    destructor Destroy; override;
  end;

function PreferencesManager: TUserPreferencesManager;

implementation

var
  FPreferencesManager: TUserPreferencesManager = nil;

function PreferencesManager: TUserPreferencesManager;
begin
  Result := FPreferencesManager;
end;

{ TUserPreferencesManager }

function TUserPreferencesManager.UserSectionName(AUserInfo: TUserInfo): string;
begin
 Result := 'users.' + Id2Str(AUserInfo.ID);
end;

procedure TUserPreferencesManager.HandleUserDeleting(AInfo: TUserInfo);
begin
  Storage.DeletePath(UserSectionName(AInfo));
end;

procedure TUserPreferencesManager.MessageReceived(AMessage: TAuthorMessage);
begin
  if AMessage is TUserDeletingMessage then
    HandleUserDeleting((AMessage as TUserDeletingMessage).Info);
end;

function TUserPreferencesManager.GetPreferences(AUser: TUser): TUserPreferences;
begin
  Result := TUserPreferences.Create(Self, AUser);
end;

constructor TUserPreferencesManager.Create;
begin
  inherited Create;
  FStorage := TXmlDataStorage.Create('preferences');
  UserManager.Subscribe(Self);
end;

destructor TUserPreferencesManager.Destroy;
begin
  UserManager.Unsubscribe(Self);
  FreeAndNil(FStorage);
  inherited Destroy;
end;

{ TUserPreferences }

function TUserPreferences.UserSectionName: string;
begin
  Result := Manager.UserSectionName(User.Info);
end;

procedure TUserPreferences.DoReload;
begin
  FLastLanguage := StrToLanguage(Storage.ReadString(UserSectionName + '.lastLanguage',
    LanguageToStr(slPascal)));
end;

procedure TUserPreferences.DoCommit;
begin
  Storage.WriteString(UserSectionName + '.lastLanguage', LanguageToStr(FLastLanguage));
end;

constructor TUserPreferences.Create(AManager: TUserPreferencesManager;
  AUser: TUser);
begin
  inherited Create;
  FManager := AManager;
  FUser := AUser;
  FStorage := Manager.Storage;
end;

procedure TUserPreferences.Reload;
begin
  DoReload;
end;

procedure TUserPreferences.Commit;
begin
  Validate;
  DoCommit;
end;

procedure TUserPreferences.Validate;
begin
  // do nothing
end;

procedure TUserPreferences.AfterConstruction;
begin
  inherited AfterConstruction;
  Reload;
end;

constructor TUserPreferences.Create;
begin
  // we don't want the preferences to be created publicly!
  raise EInvalidOperation.CreateFmt(SCreationPublic, [ClassName]);
end;

initialization
  FPreferencesManager := TUserPreferencesManager.Create;

finalization
  FreeAndNil(FPreferencesManager);

end.

