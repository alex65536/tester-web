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
unit tswebobservers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TAuthorMessage }

  TAuthorMessage = class
  private
    FSender: TObject;
  public
    property Sender: TObject read FSender;
    constructor Create(ASender: TObject);
  end;

  TDestroyAuthorMessage = class(TAuthorMessage);

  {$interfaces CORBA}

  { IMessageSubscriber }

  IMessageSubscriber = interface
    ['{650BC690-067F-4865-9F38-8487AF01BFFA}']
    procedure MessageReceived(AMessage: TAuthorMessage);
  end;

  { IMessageAuthor }

  IMessageAuthor = interface
    ['{9C9F3115-F5D1-444F-B29A-75D8408A116F}']
    procedure Subscribe(ASubscriber: TObject);
    procedure Unsubscribe(ASubscriber: TObject);
    procedure Broadcast(AMessage: TAuthorMessage);
  end;
  {$interfaces COM}

  { TMessageAuthor }

  TMessageAuthor = class(IMessageAuthor)
  private
    FSubscribers: TList;
  public
    procedure Subscribe(ASubscriber: TObject);
    procedure Unsubscribe(ASubscriber: TObject);
    procedure Broadcast(AMessage: TAuthorMessage);
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TMessageAuthor }

procedure TMessageAuthor.Subscribe(ASubscriber: TObject);
begin
  if FSubscribers.IndexOf(ASubscriber) < 0 then
    FSubscribers.Add(ASubscriber);
end;

procedure TMessageAuthor.Unsubscribe(ASubscriber: TObject);
begin
  FSubscribers.Remove(ASubscriber);
end;

procedure TMessageAuthor.Broadcast(AMessage: TAuthorMessage);
var
  P: pointer;
begin
  try
    for P in FSubscribers do
      (TObject(P) as IMessageSubscriber).MessageReceived(AMessage);
  finally
    FreeAndNil(AMessage);
  end;
end;

constructor TMessageAuthor.Create;
begin
  FSubscribers := TList.Create;
end;

destructor TMessageAuthor.Destroy;
begin
  Broadcast(TDestroyAuthorMessage.Create(Self));
  inherited;
end;

{ TAuthorMessage }

constructor TAuthorMessage.Create(ASender: TObject);
begin
  FSender := ASender;
end;

end.

