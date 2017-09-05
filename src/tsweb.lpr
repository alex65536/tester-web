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
  iniwebsession,
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
  dateutils, tswebsessions;

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
  {Salt: ansistring;
  Start, Finish: TDateTime;}
  AStorage: TAbstractDataStorage;

begin
  AStorage := TIniDataStorage.Create('demo');
  try
    AStorage.WriteString('w', '-1');
    AStorage.WriteString('a', '0');
    AStorage.WriteString('a.a', '1');
    AStorage.WriteString('a.b', '2');
    AStorage.WriteString('a.c', '3');
    AStorage.WriteString('a.c.a', '4');
    AStorage.WriteString('a.c.b', '5');
    AStorage.WriteString('a.d.a', '6');
    AStorage.WriteString('a.d.b', '7');
    AStorage.WriteString('a.d.c.hello', '8');
    AStorage.WriteString('b.a', '9');
    AStorage.WriteString('b.b', 'A');
    AStorage.WriteString('b.b.a', 'B');
    AStorage.WriteString('b.c.a', 'C');
    AStorage.WriteString('c.a.hello', 'D');

    with AStorage.GetRootElements do
    begin
      WriteLn(Text);
      Free;
    end;

    AStorage.DeletePath('a.d');

    AStorage.Commit;
  finally
    FreeAndNil(AStorage);
  end;

  //OnGetApplicationName := @DoGetApplicationName;

  {Salt := GenSalt;
  WriteLn(Salt);

  Start := Now;
  WriteLn('Beg: ', FormatDateTime('hh:nn:ss.zzzz', Start));

  WriteLn(HashPassword('Ubuntu GNU/Linux', Salt));

  Finish := Now;
  WriteLn('End: ', FormatDateTime('hh:nn:ss.zzzz', Finish));

  WriteLn('Time = ', MilliSecondsBetween(Start, Finish), ' ms');}

  Application.Title := 'Tester Web';
  {Application.Port := 8080;
  Application.Initialize;
  Application.Run;}
end.
