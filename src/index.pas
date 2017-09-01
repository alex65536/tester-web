unit index;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, httpdefs, fpHTTP, fpWeb;

type
  TIndexModule = class(TFPWebModule)
    // ToDo : Write it later !!!
  end;

var
  IndexModule: TIndexModule;

implementation

{$R *.lfm}

initialization
  RegisterHTTPModule('index', TIndexModule);
end.

