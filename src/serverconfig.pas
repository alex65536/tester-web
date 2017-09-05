unit serverconfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, datastorages, webstrconsts;

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

    // session parameters
    property Session_IDLength: integer read GetSession_IDLength;
    property Session_AliveTime: integer read GetSession_AliveTime;

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

