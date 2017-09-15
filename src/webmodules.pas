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
unit webmodules;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, htmlpages, fphttp, HTTPDefs, fgl;

type

  { TTesterWebModule }

  TTesterWebModule = class(TSessionHTTPModule)
  private
    FRequest: TRequest;
    FResponse: TResponse;
  protected
    procedure DoBeforeRequest; virtual;
    procedure DoAfterRequest; virtual;
    procedure DoSessionCreated; virtual;
    procedure DoInsideRequest; virtual; abstract;
  public
    property Request: TRequest read FRequest;
    property Response: TResponse read FResponse;
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
    procedure AfterConstruction; override;
  end;

  THandlerWebModule = class;

  { TWebModuleHandler }

  TWebModuleHandler = class
  private
    FParent: THandlerWebModule;
  public
    property Parent: THandlerWebModule read FParent write FParent;
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse;
      var Handled: boolean); virtual; abstract;
    constructor Create;
  end;

  { TEventWebModuleHandler }

  TEventWebModuleHandler = class(TWebModuleHandler)
  private
    FOnRequest: TWebActionEvent;
  public
    property OnRequest: TWebActionEvent read FOnRequest write FOnRequest;
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse;
      var Handled: boolean); override;
  end;

  { TWebModuleHandlerList }

  TWebModuleHandlerList = class
  private type
    TInternalList = specialize TFPGObjectList<TWebModuleHandler>;
  private
    FParent: THandlerWebModule;
    FList: TInternalList;
    function GetCount: integer;
    function GetItems(I: integer): TWebModuleHandler;
  public
    property Parent: THandlerWebModule read FParent;
    property Count: integer read GetCount;
    property Items[I: integer]: TWebModuleHandler read GetItems; default;
    procedure Add(AHandler: TWebModuleHandler);
    procedure Insert(Index: integer; AHandler: TWebModuleHandler);
    procedure Delete(Index: integer);
    procedure Remove(AHandler: TWebModuleHandler);
    constructor Create(AParent: THandlerWebModule);
    destructor Destroy; override;
  end;

  { THandlerWebModule }

  THandlerWebModule = class(TTesterWebModule)
  private
    FHandlers: TWebModuleHandlerList;
  protected
    procedure InternalHandleRequest; virtual; abstract;
    procedure DoInsideRequest; override;
  public
    property Handlers: TWebModuleHandlerList read FHandlers;
    procedure AddEventHandler(AEvent: TWebActionEvent);
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;
  end;

  { THtmlPageWebModule }

  THtmlPageWebModule = class(THandlerWebModule)
  protected
    function DoCreatePage: THtmlPage; virtual; abstract;
    procedure DoPageAfterConstruction({%H-}APage: THtmlPage); virtual;
    function CreatePage: THtmlPage;
    procedure InternalHandleRequest; override;
  end;

implementation

{ THtmlPageWebModule }

procedure THtmlPageWebModule.DoPageAfterConstruction(APage: THtmlPage);
begin
  // do nothing
end;

function THtmlPageWebModule.CreatePage: THtmlPage;
begin
  Result := DoCreatePage;
  try
    Result.Request := Request;
    Result.Response := Response;
    Result.Session := Session;
    DoPageAfterConstruction(Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure THtmlPageWebModule.InternalHandleRequest;
var
  APage: THtmlPage;
begin
  APage := CreatePage;
  try
    APage.UpdateRequest;
    APage.UpdateResponse;
  finally
    FreeAndNil(APage);
  end;
end;

{ TTesterWebModule }

procedure TTesterWebModule.DoBeforeRequest;
begin
  // do nothing
end;

procedure TTesterWebModule.DoAfterRequest;
begin
  // do nothing
end;

procedure TTesterWebModule.DoSessionCreated;
begin
  // do nothing
end;

procedure TTesterWebModule.HandleRequest(ARequest: TRequest; AResponse: TResponse);
begin
  inherited HandleRequest(ARequest, AResponse);
  FRequest := ARequest;
  FResponse := AResponse;
  try
    DoBeforeRequest;
    CheckSession(ARequest);
    InitSession(AResponse);
    try
      DoSessionCreated;
      DoInsideRequest;
      UpdateSession(AResponse);
      if not AResponse.ContentSent then
        AResponse.SendContent;
    finally
      DoneSession;
      DoAfterRequest;
    end;
  finally
    FRequest := nil;
    FResponse := nil;
  end;
end;

procedure TTesterWebModule.AfterConstruction;
begin
  inherited AfterConstruction;
  CreateSession := True;
end;

{ TEventWebModuleHandler }

procedure TEventWebModuleHandler.HandleRequest(ARequest: TRequest;
  AResponse: TResponse; var Handled: boolean);
begin
  if Assigned(FOnRequest) then
    FOnRequest(Self, ARequest, AResponse, Handled);
end;

{ TWebModuleHandler }

constructor TWebModuleHandler.Create;
begin
  // do nothing
end;

{ TWebModuleHandlerList }

function TWebModuleHandlerList.GetCount: integer;
begin
  Result := FList.Count;
end;

function TWebModuleHandlerList.GetItems(I: integer): TWebModuleHandler;
begin
  Result := FList.Items[I];
end;

procedure TWebModuleHandlerList.Add(AHandler: TWebModuleHandler);
begin
  AHandler.Parent := Parent;
  FList.Add(AHandler);
end;

procedure TWebModuleHandlerList.Insert(Index: integer; AHandler: TWebModuleHandler);
begin
  AHandler.Parent := Parent;
  FList.Insert(Index, AHandler);
end;

procedure TWebModuleHandlerList.Delete(Index: integer);
begin
  FList[Index].Parent := nil;
  FList.Delete(Index);
end;

procedure TWebModuleHandlerList.Remove(AHandler: TWebModuleHandler);
var
  Index: integer;
begin
  Index := FList.IndexOf(AHandler);
  if Index >= 0 then
    Delete(Index);
end;

constructor TWebModuleHandlerList.Create(AParent: THandlerWebModule);
begin
  FParent := AParent;
  FList := TInternalList.Create(True);
end;

destructor TWebModuleHandlerList.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

{ THandlerWebModule }

procedure THandlerWebModule.DoInsideRequest;
var
  Handled: boolean;
  I: integer;
begin
  // handle web handlers
  Handled := False;
  for I := 0 to FHandlers.Count - 1 do
  begin
    FHandlers[I].HandleRequest(Request, Response, Handled);
    if Handled then
      Break;
  end;
  // if not handled, run HTML page rendering
  if not Handled then
    InternalHandleRequest;
end;

procedure THandlerWebModule.AddEventHandler(AEvent: TWebActionEvent);
var
  AHandler: TEventWebModuleHandler;
begin
  AHandler := TEventWebModuleHandler.Create;
  AHandler.OnRequest := AEvent;
  FHandlers.Add(AHandler);
end;

constructor THandlerWebModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  FHandlers := TWebModuleHandlerList.Create(Self);
end;

destructor THandlerWebModule.Destroy;
begin
  FreeAndNil(FHandlers);
  inherited Destroy;
end;

end.
