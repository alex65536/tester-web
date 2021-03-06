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
unit serverevents;

// WARNING : This unit should be added as early as possible!

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tswebobservers;

type
  TTerminateMethod = procedure of object;

  TIdleMessage = class(TAuthorMessage);

  { TIdleMessenger }

  TIdleMessenger = class(TMessageAuthor)
  public
    procedure OnIdle(Sender: TObject);
  end;

var
  OnServerTerminate: TTerminateMethod = nil;
  IdleMessenger: TIdleMessenger;

implementation

function DoGetApplicationName: string;
begin
  Result := 'tsweb';
end;

{ TIdleMessenger }

procedure TIdleMessenger.OnIdle(Sender: TObject);
begin
  Broadcast(TIdleMessage.Create.AddSender(Sender).Lock);
end;

initialization
  OnGetApplicationName := @DoGetApplicationName;
  IdleMessenger := TIdleMessenger.Create;

finalization
  FreeAndNil(IdleMessenger);

end.

