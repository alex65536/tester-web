unit serverconfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, datastorages;

type

  { TTesterServerConfig }

  TTesterServerConfig = class(TPersistent, IFPObserver)
  protected const
    ConfigStorageName = 'config';
  private
    FConstructing: boolean;
    FStorage: TAbstractDataStorage;
    function GetTestInt: integer;
  protected
    procedure FPOObservedChanged(ASender: TObject;
      Operation: TFPObservedOperation; Data: Pointer);
    procedure DefaultSettings; virtual;
    function CreateStorageConfig: TAbstractDataStorage; virtual;
  public
    // temp prop, TO REMOVE IT
    property TestInt: integer read GetTestInt;

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

function TTesterServerConfig.GetTestInt: integer;
begin
  Result := FStorage.ReadInteger('tsrv.test.int', -1);
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
    // just temp code
    WriteInteger('tsrv.test.int', GetTestInt);
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

