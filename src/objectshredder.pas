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
unit objectshredder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, serverevents, tswebobservers;

type

  { TObjectShredder }

  TObjectShredder = class(IMessageSubscriber)
  private
    FList: TList;
  protected
    procedure Shred; virtual;
    procedure MessageReceived(AMessage: TAuthorMessage);
  public
    procedure Add(AObject: TObject);
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TObjectShredder }

procedure TObjectShredder.Shred;
var
  Obj: TObject;
begin
  while FList.Count > 0 do
  begin
    Obj := TObject(FList.Last);
    FList.Delete(FList.Count - 1);
    FreeAndNil(Obj);
  end;
end;

procedure TObjectShredder.MessageReceived(AMessage: TAuthorMessage);
begin
  if AMessage is TIdleMessage then
    Shred;
end;

procedure TObjectShredder.Add(AObject: TObject);
begin
  FList.Add(AObject);
end;

constructor TObjectShredder.Create;
begin
  FList := TList.Create;
  IdleMessenger.Subscribe(Self);
end;

destructor TObjectShredder.Destroy;
begin
  IdleMessenger.Unsubscribe(Self);
  Shred;
  FreeAndNil(FList);
  inherited Destroy;
end;

end.

