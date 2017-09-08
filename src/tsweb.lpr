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
program tsweb;

{$mode objfpc}{$H+}

uses
  heaptrc,
  tswebsessions,
  hash_3rdparty,
  fphttpapp,
  index,
  htmlpreprocess,
  escaping,
  webstrconsts,
  SysUtils,
  datastorages,
  serverconfig,
  tswebcrypto,
  Types,
  dateutils,
  fpwebfile,
  htmlpages,
  tswebhtmlpages;

{

  This leads to hard-to-find bugs, because config is created before
this is assigned.

TODO : Move DoGetApplicationName somewhere where it can be included as first unit!

function DoGetApplicationName: string;
begin
  Result := 'tsweb';
end;

}

var
  VarName: string;
  Storage: TVariableStorage;
  Preprocessor: THtmlPreprocessor;
  Content: TIndentTaggedStrings;

begin
  // testing HTML preprocessor
  WriteLn('To quit, enter "quit" instead of VarName');
  Storage := TTreeVariableStorage.Create;
  Preprocessor := THtmlPreprocessor.Create;
  try
    Preprocessor.Storages.Add(Storage);
    while 42 = 42 { True } do
    begin
      Write('VarName (will be loaded from ../test/VarName.htpp): ');
      ReadLn(VarName);
      if VarName = 'quit' then
        Break;
      try
        Content := TIndentTaggedStrings.Create;
        try
          Preprocessor.PreprocessFile('..' + PathDelim + 'test' +
            PathDelim + VarName + '.htpp', Content);
          Storage.SetItemAsStrings(VarName, Content);
          WriteLn('Inserted, contents (raw) = ' + LineEnding + Content.RawText);
        finally
          FreeAndNil(Content);
        end;
      except
        on E: Exception do
          WriteLn(E.ClassName, ' : ', E.Message);
      end;
    end;
  finally
    FreeAndNil(Storage);
    FreeAndNil(Preprocessor);
  end;

  //OnGetApplicationName := @DoGetApplicationName;

  // temp code, to test template page everywhere
  {MimeTypesFile := '/etc/mime.types';
  RegisterFileLocation('templates', '../templates');
  RegisterFileLocation('data', '../data');

  Application.Title := 'Tester Web';
  Application.Port := 8080;
  Application.DefaultModuleName := 'index';
  Application.Initialize;
  Application.Run; }
end.
