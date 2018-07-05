program tswebmkcfg;

uses
  SysUtils, serverconfig, hash_3rdparty;

function GetApplicationName: string;
begin
  Result := 'tsweb';
end;

var
  Config: TTesterServerConfig;
begin
  OnGetApplicationName := @GetApplicationName;
  Config := TTesterServerConfig.Create;
  try
    WriteLn('Server configuration updated');
    WriteLn('--------------------');
    WriteLn('Server owner login and password:');
    WriteLn('login: ', Config.Owner_DefaultUsername);
    WriteLn('password: ', Config.Owner_DefaultPassword);
    WriteLn('--------------------');
    WriteLn('It is recommended to change them after the first login.');
  finally
    FreeAndNil(Config);
  end;
end.

