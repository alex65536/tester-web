{
  This file is part of Tester Web

  Copyright (C) 2017-2018 Alexander Kernozhitsky <sh200105@mail.ru>

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
  Classes, SysUtils, datastorages, webstrconsts;

const
  {$IfDef Windows}
  ExeExt = '.exe';
  {$Else}
  ExeExt = '';
  {$EndIf}

type

  { TTesterServerConfig }

  TTesterServerConfig = class(TPersistent, IFPObserver)
  protected const
    ConfigStorageName = 'config';
  private
    FConstructing: boolean;
    FStorage: TAbstractDataStorage;
    function GetContest_MaxDurationMinutes: integer;
    function GetCrypto_HashLen: integer;
    function GetCrypto_SaltLen: integer;
    function GetCrypto_SCrypt_LogN: integer;
    function GetCrypto_SCrypt_R: integer;
    function GetCrypto_SCrypt_P: integer;
    function GetFiles_DefaultSrcSize: integer;
    function GetFiles_MaxArchiveFileCount: integer;
    function GetFiles_MaxArchiveSize: integer;
    function GetFiles_MaxSrcSize: integer;
    function GetFiles_MaxStatementsSize: integer;
    function GetFiles_MaxUnpackedArchiveSize: integer;
    function GetLocation_DataDir: string;
    function GetLocation_TemplatesDir: string;
    function GetLocation_TsRunExe: string;
    function GetOwner_DefaultFirstName: string;
    function GetOwner_DefaultLastName: string;
    function GetOwner_DefaultPassword: string;
    function GetOwner_DefaultUsername: string;
    function GetProblem_DefaultPropsFile: string;
    function GetServer_Address: string;
    function GetServer_Port: integer;
    function GetServer_QueueSize: integer;
    function GetSession_AliveTimeMinutes: integer;
    function GetSession_IDLength: integer;
    function GetSession_TokenLength: integer;
    function GetStorages_CommitIntervalSeconds: integer;
    function GetStrings_MaxNameLength: integer;
    function GetStrings_MaxPasswordLength: integer;
    function GetStrings_MaxTitleLength: integer;
    function GetStrings_MinNameLength: integer;
    function GetStrings_MinPasswordLength: integer;
    function GetStrings_MinTitleLength: integer;
    function GetTesting_MaxPoolSize: integer;
  protected
    property Storage: TAbstractDataStorage read FStorage;
    procedure FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation;
      Data: Pointer);
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
    property Location_TsRunExe: string read GetLocation_TsRunExe;

    // session parameters
    property Session_IDLength: integer read GetSession_IDLength;
    property Session_TokenLength: integer read GetSession_TokenLength;
    property Session_AliveTimeMinutes: integer read GetSession_AliveTimeMinutes;

    // storage parameters
    property Storages_CommitIntervalSeconds: integer
      read GetStorages_CommitIntervalSeconds;

    // owner default settings
    // WARNING: don't store your actual password here, use "Change password"
    // option right after your first login!
    // This settings must be set only for INITIAL owner profile setup.
    // It is also good to change this on something random after first login.
    property Owner_DefaultUsername: string read GetOwner_DefaultUsername;
    property Owner_DefaultPassword: string read GetOwner_DefaultPassword;
    property Owner_DefaultFirstName: string read GetOwner_DefaultFirstName;
    property Owner_DefaultLastName: string read GetOwner_DefaultLastName;

    // files default settings (all sizes are in KBytes!)
    property Files_MaxArchiveSize: integer read GetFiles_MaxArchiveSize;
    property Files_MaxArchiveFileCount: integer read GetFiles_MaxArchiveFileCount;
    property Files_MaxUnpackedArchiveSize: integer read GetFiles_MaxUnpackedArchiveSize;
    property Files_MaxStatementsSize: integer read GetFiles_MaxStatementsSize;
    property Files_MaxSrcSize: integer read GetFiles_MaxSrcSize;
    property Files_DefaultSrcSize: integer read GetFiles_DefaultSrcSize;

    // problem default settings
    property Problem_DefaultPropsFile: string read GetProblem_DefaultPropsFile;

    // server settings
    property Server_Address: string read GetServer_Address;
    property Server_Port: integer read GetServer_Port;
    property Server_QueueSize: integer read GetServer_QueueSize;

    // testing options
    property Testing_MaxPoolSize: integer read GetTesting_MaxPoolSize;

    // contest options
    property Contest_MaxDurationMinutes: integer read GetContest_MaxDurationMinutes;

    // strings options
    property Strings_MinNameLength: integer read GetStrings_MinNameLength;
    property Strings_MaxNameLength: integer read GetStrings_MaxNameLength;
    property Strings_MinTitleLength: integer read GetStrings_MinTitleLength;
    property Strings_MaxTitleLength: integer read GetStrings_MaxTitleLength;
    property Strings_MinPasswordLength: integer read GetStrings_MinPasswordLength;
    property Strings_MaxPasswordLength: integer read GetStrings_MaxPasswordLength;

    constructor Create;
    procedure Reload;
    destructor Destroy; override;
  end;

implementation

uses
  tswebcrypto;

{ TTesterServerConfig }

function TTesterServerConfig.GetCrypto_SaltLen: integer;
begin
  Result := FStorage.ReadInteger('crypto.saltLen', 64);
end;

function TTesterServerConfig.GetCrypto_HashLen: integer;
begin
  Result := FStorage.ReadInteger('crypto.hashLen', 64);
end;

function TTesterServerConfig.GetContest_MaxDurationMinutes: integer;
begin
  Result := FStorage.ReadInteger('contest.maxDurationMinutes', 10080);
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

function TTesterServerConfig.GetFiles_DefaultSrcSize: integer;
begin
  Result := FStorage.ReadInteger('files.defaultSrcSize', 64);
end;

function TTesterServerConfig.GetFiles_MaxArchiveFileCount: integer;
begin
  Result := FStorage.ReadInteger('files.maxArchiveFileCount', 2048);
end;

function TTesterServerConfig.GetFiles_MaxArchiveSize: integer;
begin
  Result := FStorage.ReadInteger('files.maxArchiveSize', 65536);
end;

function TTesterServerConfig.GetFiles_MaxSrcSize: integer;
begin
  Result := FStorage.ReadInteger('files.maxSrcSize', 2048);
end;

function TTesterServerConfig.GetFiles_MaxStatementsSize: integer;
begin
  Result := FStorage.ReadInteger('files.maxStatementsSize', 4096);
end;

function TTesterServerConfig.GetFiles_MaxUnpackedArchiveSize: integer;
begin
  Result := FStorage.ReadInteger('files.maxUnpackedArchiveSize', 262144);
end;

function TTesterServerConfig.GetLocation_DataDir: string;
begin
  Result := FStorage.ReadString('location.dataDir', '..' + PathDelim + 'data');
end;

function TTesterServerConfig.GetLocation_TemplatesDir: string;
begin
  Result := FStorage.ReadString('location.templatesDir', '..' + PathDelim + 'templates');
end;

function TTesterServerConfig.GetLocation_TsRunExe: string;
begin
  Result := FStorage.ReadString('location.tsRunExe', '.' + PathDelim + 'tsrun' + ExeExt);
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
  Result := FStorage.ReadString('owner.defaults.password', '');
  if Result = '' then
    Result := RandomSequenceBase64(12);
end;

function TTesterServerConfig.GetOwner_DefaultUsername: string;
begin
  Result := FStorage.ReadString('owner.defaults.userName', 'admin');
end;

function TTesterServerConfig.GetProblem_DefaultPropsFile: string;
begin
  Result := FStorage.ReadString('problem.defaultPropsFile', 'props.json');
end;

function TTesterServerConfig.GetServer_Address: string;
begin
  Result := FStorage.ReadString('server.address', '');
end;

function TTesterServerConfig.GetServer_Port: integer;
begin
  Result := FStorage.ReadInteger('server.port', 8080);
end;

function TTesterServerConfig.GetServer_QueueSize: integer;
begin
  Result := FStorage.ReadInteger('server.queueSize', 5);
end;

function TTesterServerConfig.GetSession_AliveTimeMinutes: integer;
begin
  Result := FStorage.ReadInteger('session.aliveTimeMinutes', 180);
end;

function TTesterServerConfig.GetSession_IDLength: integer;
begin
  Result := FStorage.ReadInteger('session.idLength', 32);
end;

function TTesterServerConfig.GetSession_TokenLength: integer;
begin
  Result := FStorage.ReadInteger('session.tokenLength', 32);
end;

function TTesterServerConfig.GetStorages_CommitIntervalSeconds: integer;
begin
  Result := FStorage.ReadInteger('storages.commitIntervalSeconds', 60);
end;

function TTesterServerConfig.GetStrings_MaxNameLength: integer;
begin
  Result := FStorage.ReadInteger('strings.maxNameLength', 24);
end;

function TTesterServerConfig.GetStrings_MaxPasswordLength: integer;
begin
  Result := FStorage.ReadInteger('strings.maxPasswordLength', 64);
end;

function TTesterServerConfig.GetStrings_MaxTitleLength: integer;
begin
  Result := FStorage.ReadInteger('strings.maxTitleLength', 42);
end;

function TTesterServerConfig.GetStrings_MinNameLength: integer;
begin
  Result := FStorage.ReadInteger('strings.minNameLength', 3);
end;

function TTesterServerConfig.GetStrings_MinPasswordLength: integer;
begin
  Result := FStorage.ReadInteger('strings.minPasswordLength', 8);
end;

function TTesterServerConfig.GetStrings_MinTitleLength: integer;
begin
  Result := FStorage.ReadInteger('strings.minTitleLength', 2);
end;

function TTesterServerConfig.GetTesting_MaxPoolSize: integer;
begin
  Result := FStorage.ReadInteger('testing.maxPoolSize', 2);
end;

procedure TTesterServerConfig.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  ASender := ASender; // to prevent hints
  if not (Operation in [ooAddItem, ooDeleteItem, ooChange, ooCustom]) then
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

    WriteInteger('session.aliveTimeMinutes', Session_AliveTimeMinutes);
    WriteInteger('session.idLength', Session_IDLength);
    WriteInteger('session.tokenLength', Session_TokenLength);

    WriteInteger('storages.commitIntervalSeconds', Storages_CommitIntervalSeconds);

    WriteString('location.dataDir', Location_DataDir);
    WriteString('location.templatesDir', Location_TemplatesDir);
    WriteString('location.tsRunExe', GetLocation_TsRunExe);

    WriteString('owner.defaults.notice', SOwnerParamsNotice);
    WriteString('owner.defaults.firstName', Owner_DefaultFirstName);
    WriteString('owner.defaults.lastName', Owner_DefaultLastName);
    WriteString('owner.defaults.password', Owner_DefaultPassword);
    WriteString('owner.defaults.userName', Owner_DefaultUsername);

    WriteString('files.notice', SFilesNotice);
    WriteInteger('files.maxArchiveSize', Files_MaxArchiveSize);
    WriteInteger('files.maxArchiveFileCount', GetFiles_MaxArchiveFileCount);
    WriteInteger('files.maxSrcSize', Files_MaxSrcSize);
    WriteInteger('files.defaultSrcSize', Files_DefaultSrcSize);
    WriteInteger('files.maxStatementsSize', Files_MaxStatementsSize);
    WriteInteger('files.maxUnpackedArchiveSize', Files_MaxUnpackedArchiveSize);

    WriteString('problem.defaultPropsFile', Problem_DefaultPropsFile);

    WriteString('server.address', Server_Address);
    WriteInteger('server.port', Server_Port);
    WriteInteger('server.queueSize', Server_QueueSize);

    WriteInteger('testing.maxPoolSize', Testing_MaxPoolSize);

    WriteInteger('contest.maxDurationMinutes', GetContest_MaxDurationMinutes);

    WriteInteger('strings.minNameLength', Strings_MinNameLength);
    WriteInteger('strings.maxNameLength', Strings_MaxNameLength);
    WriteInteger('strings.minTitleLength', Strings_MinTitleLength);
    WriteInteger('strings.maxTitleLength', Strings_MaxTitleLength);
    WriteInteger('strings.minPasswordLength', Strings_MinPasswordLength);
    WriteInteger('strings.maxPasswordLength', Strings_MaxPasswordLength);
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
  FStorage.CommitOnDestroy := False;
  FStorage.FPOAttachObserver(Self);
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
  FreeAndNil(FStorage);
  inherited Destroy;
end;

end.
