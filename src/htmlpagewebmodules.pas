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
unit htmlpagewebmodules;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, htmlpages, fphttp, HTTPDefs, fgl;

type
  THtmlPageWebModule = class;

  { TWebModuleHandler }

  TWebModuleHandler = class
  private
    FParent: THtmlPageWebModule;
  public
    property Parent: THtmlPageWebModule read FParent;
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse;
      var Handled: boolean); virtual; abstract;
    constructor Create(AParent: THtmlPageWebModule); virtual;
  end;

  TWebModuleHandlerClass = class of TWebModuleHandler;

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
    FParent: THtmlPageWebModule;
    FList: TInternalList;
    function GetCount: integer;
    function GetItems(I: integer): TWebModuleHandler;
  public
    property Parent: THtmlPageWebModule read FParent;
    property Count: integer read GetCount;
    property Items[I: integer]: TWebModuleHandler read GetItems; default;
    function Add(AClass: TWebModuleHandlerClass): TWebModuleHandler;
    function Insert(Index: integer; AClass: TWebModuleHandlerClass): TWebModuleHandler;
    procedure Delete(Index: integer);
    procedure Remove(AHandler: TWebModuleHandler);
    constructor Create(AParent: THtmlPageWebModule);
    destructor Destroy; override;
  end;

  { THtmlPageWebModule }

  THtmlPageWebModule = class(TSessionHTTPModule)
  private
    FHandlers: TWebModuleHandlerList;
    FRequest: TRequest;
    FResponse: TResponse;
  protected
    function DoCreatePage: THtmlPage; virtual; abstract;
    procedure DoPageAfterConstruction({%H-}APage: THtmlPage); virtual;
    function CreatePage: THtmlPage;
    procedure InternalHandleRequest; virtual;
    procedure DoBeforeRequest; virtual;
    procedure DoAfterRequest; virtual;
    procedure AddEventHandler(AEvent: TWebActionEvent);
  public
    property Handlers: TWebModuleHandlerList read FHandlers;
    property Request: TRequest read FRequest;
    property Response: TResponse read FResponse;
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
    procedure AfterConstruction; override;
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;
  end;

implementation

{ TEventWebModuleHandler }

procedure TEventWebModuleHandler.HandleRequest(ARequest: TRequest;
  AResponse: TResponse; var Handled: boolean);
begin
  if Assigned(FOnRequest) then
    FOnRequest(Self, ARequest, AResponse, Handled);
end;

{ TWebModuleHandler }

constructor TWebModuleHandler.Create(AParent: THtmlPageWebModule);
begin
  FParent := AParent;
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

function TWebModuleHandlerList.Add(AClass: TWebModuleHandlerClass): TWebModuleHandler;
begin
  Result := AClass.Create(FParent);
  FList.Add(Result);
end;

function TWebModuleHandlerList.Insert(Index: integer;
  AClass: TWebModuleHandlerClass): TWebModuleHandler;
begin
  Result := AClass.Create(FParent);
  FList.Insert(Index, Result);
end;

procedure TWebModuleHandlerList.Delete(Index: integer);
begin
  FList.Delete(Index);
end;

procedure TWebModuleHandlerList.Remove(AHandler: TWebModuleHandler);
begin
  FList.Remove(AHandler);
end;

constructor TWebModuleHandlerList.Create(AParent: THtmlPageWebModule);
begin
  FParent := AParent;
  FList := TInternalList.Create(True);
end;

destructor TWebModuleHandlerList.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

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

procedure THtmlPageWebModule.DoBeforeRequest;
begin
  // do nothing
end;

procedure THtmlPageWebModule.DoAfterRequest;
begin
  // do nothing
end;

procedure THtmlPageWebModule.AddEventHandler(AEvent: TWebActionEvent);
var
  AHandler: TEventWebModuleHandler;
begin
  AHandler := FHandlers.Add(TEventWebModuleHandler) as TEventWebModuleHandler;
  AHandler.OnRequest := AEvent;
end;

procedure THtmlPageWebModule.HandleRequest(ARequest: TRequest; AResponse: TResponse);
var
  Handled: boolean;
  I: integer;
begin
  inherited HandleRequest(ARequest, AResponse);
  FRequest := ARequest;
  FResponse := AResponse;
  try
    DoBeforeRequest;
    CheckSession(ARequest);
    InitSession(AResponse);
    try
      // handle web handlers
      Handled := False;
      for I := 0 to FHandlers.Count - 1 do
      begin
        FHandlers[I].HandleRequest(ARequest, AResponse, Handled);
        if Handled then
          Break;
      end;
      // if not handled, run HTML page rendering
      if not Handled then
        InternalHandleRequest;
      // update session & send content
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

procedure THtmlPageWebModule.AfterConstruction;
begin
  inherited AfterConstruction;
  CreateSession := True;
end;

constructor THtmlPageWebModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  FHandlers := TWebModuleHandlerList.Create(Self);
end;

destructor THtmlPageWebModule.Destroy;
begin
  FreeAndNil(FHandlers);
  inherited Destroy;
end;

end.
