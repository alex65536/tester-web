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
unit serverconfig;

{$mode objfpc}{$H+}

interface

uses
  serverevents, Classes, SysUtils, datastorages, webstrconsts;

type

  { TTesterServerConfig }

  TTesterServerConfig = class(TPersistent, IFPObserver)
  protected const
    ConfigStorageName = 'config';
  private
    FConstructing: boolean;
    FStorage: TAbstractDataStorage;
    function GetCrypto_HashLen: integer;
    function GetCrypto_SaltLen: integer;
    function GetCrypto_SCrypt_LogN: integer;
    function GetCrypto_SCrypt_R: integer;
    function GetCrypto_SCrypt_P: integer;
    function GetLocation_DataDir: string;
    function GetLocation_TemplatesDir: string;
    function GetOwner_DefaultFirstName: string;
    function GetOwner_DefaultLastName: string;
    function GetOwner_DefaultPassword: string;
    function GetOwner_DefaultUsername: string;
    function GetSession_AliveTime: integer;
    function GetSession_IDLength: integer;
  protected
    procedure FPOObservedChanged(ASender: TObject;
      Operation: TFPObservedOperation; Data: Pointer);
    procedure DefaultSettings; virtual;
    function CreateStorageConfig: TAbstractDataStorage; virtual;
  public
    // crypto parameters
    // WARNING: changing them in config will lead to loss of ALL USERS PASSWORDS, because scrypt parameters will change.
    // TODO : Move it somewhere out of config (?)
    property Crypto_SaltLen: integer read GetCrypto_SaltLen;
    property Crypto_HashLen: integer read GetCrypto_HashLen;
    property Crypto_SCrypt_LogN: integer read GetCrypto_SCrypt_LogN;
    property Crypto_SCrypt_R: integer read GetCrypto_SCrypt_R;
    property Crypto_SCrypt_P: integer read GetCrypto_SCrypt_P;

    // data & templates location
    property Location_DataDir: string read GetLocation_DataDir;
    property Location_TemplatesDir: string read GetLocation_TemplatesDir;

    // session parameters
    property Session_IDLength: integer read GetSession_IDLength;
    property Session_AliveTime: integer read GetSession_AliveTime;

    // owner default settings
    // WARNING: don't store your actual password here, use "Change password"
    // option right after your first login!
    // This settings must be set only for INITIAL owner profile setup.
    // It is also good to change this on something random after first login.
    property Owner_DefaultUsername: string read GetOwner_DefaultUsername;
    property Owner_DefaultPassword: string read GetOwner_DefaultPassword;
    property Owner_DefaultFirstName: string read GetOwner_DefaultFirstName;
    property Owner_DefaultLastName: string read GetOwner_DefaultLastName;

    constructor Create;
    procedure Reload;
    destructor Destroy; override;
  end;

function Config: TTesterServerConfig;

implementation

var
  FConfig: TTesterServerConfig;

function Config: TTesterServerConfig;
begin
  Result := FConfig;
end;

{ TTesterServerConfig }

function TTesterServerConfig.GetCrypto_SaltLen: integer;
begin
  Result := FStorage.ReadInteger('crypto.saltLen', 64);
end;

function TTesterServerConfig.GetCrypto_HashLen: integer;
begin
  Result := FStorage.ReadInteger('crypto.hashLen', 64);
end;

function TTesterServerConfig.GetCrypto_SCrypt_LogN: integer;
begin
  Result := FStorage.ReadInteger('crypto.scrypt.logN', 14);
end;

function TTesterServerConfig.GetCrypto_SCrypt_R: integer;
begin
  Result := FStorage.ReadInteger('crypto.scrypt.r', 8);
end;

function TTesterServerConfig.GetCrypto_SCrypt_P: integer;
begin
  Result := FStorage.ReadInteger('crypto.scrypt.p', 1);
end;

function TTesterServerConfig.GetLocation_DataDir: string;
begin
  Result := FStorage.ReadString('location.dataDir', '..' + PathDelim + 'data');
end;

function TTesterServerConfig.GetLocation_TemplatesDir: string;
begin
  Result := FStorage.ReadString('location.templatesDir', '..' + PathDelim + 'templates');
end;

function TTesterServerConfig.GetOwner_DefaultFirstName: string;
begin
  Result := FStorage.ReadString('owner.defaults.firstName', 'Admin');
end;

function TTesterServerConfig.GetOwner_DefaultLastName: string;
begin
  Result := FStorage.ReadString('owner.defaults.lastName', 'Admin');
end;

function TTesterServerConfig.GetOwner_DefaultPassword: string;
begin
  Result := FStorage.ReadString('owner.defaults.password', 'admin');
end;

function TTesterServerConfig.GetOwner_DefaultUsername: string;
begin
  Result := FStorage.ReadString('owner.defaults.userName', 'admin');
end;

function TTesterServerConfig.GetSession_AliveTime: integer;
begin
  Result := FStorage.ReadInteger('session.aliveTime', 60);
end;

function TTesterServerConfig.GetSession_IDLength: integer;
begin
  Result := FStorage.ReadInteger('session.idLength', 32);
end;

procedure TTesterServerConfig.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  ASender := ASender; // to prevent hints
  if not (Operation in [ooAddItem, ooDeleteItem, ooChange]) then
    Exit;
  if FConstructing then
    Exit;
  FPONotifyObservers(Self, Operation, Data);
end;

procedure TTesterServerConfig.DefaultSettings;
begin
  with FStorage do
  begin
    WriteString('crypto.notice', SCryptoParamsNotice);
    WriteInteger('crypto.saltLen', GetCrypto_SaltLen);
    WriteInteger('crypto.hashLen', GetCrypto_HashLen);
    WriteInteger('crypto.scrypt.logN', GetCrypto_SCrypt_LogN);
    WriteInteger('crypto.scrypt.r', GetCrypto_SCrypt_R);
    WriteInteger('crypto.scrypt.p', GetCrypto_SCrypt_P);

    WriteInteger('session.aliveTime', Session_AliveTime);
    WriteInteger('session.idLength', Session_IDLength);

    WriteString('location.dataDir', Location_DataDir);
    WriteString('location.templatesDir', Location_TemplatesDir);

    WriteString('owner.defaults.notice', SOwnerParamsNotice);
    WriteString('owner.defaults.firstName', Owner_DefaultFirstName);
    WriteString('owner.defaults.lastName', Owner_DefaultLastName);
    WriteString('owner.defaults.password', Owner_DefaultPassword);
    WriteString('owner.defaults.userName', Owner_DefaultUsername);
  end;
end;

function TTesterServerConfig.CreateStorageConfig: TAbstractDataStorage;
begin
  Result := TIniDataStorage.Create(ConfigStorageName);
end;

constructor TTesterServerConfig.Create;
begin
  FConstructing := True;
  FStorage := CreateStorageConfig;
  FStorage.FPOAttachObserver(Self);
  FStorage.Reload;
  DefaultSettings;
  FStorage.Commit;
  FConstructing := False;
end;

procedure TTesterServerConfig.Reload;
begin
  FStorage.Reload;
end;

destructor TTesterServerConfig.Destroy;
begin
  FStorage.Commit;
  FreeAndNil(FStorage);
  inherited Destroy;
end;

initialization
  FConfig := TTesterServerConfig.Create;

finalization
  FreeAndNil(FConfig);

end.

