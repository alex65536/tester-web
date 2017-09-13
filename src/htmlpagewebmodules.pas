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
  Classes, SysUtils, htmlpages, fphttp, HTTPDefs;

type
  THtmlPageWebModule = class;

  { TWebModuleHandler }

  TWebModuleHandler = class(TCollectionItem)
  private
    FParent: THtmlPageWebModule;
  public
    property Parent: THtmlPageWebModule read FParent;
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse; var Handled: boolean); virtual; abstract;
    constructor Create(ACollection: TCollection); override;
  end;

  { TWebModuleHandlerList }

  TWebModuleHandlerList = class(TCollection)
  private
    FParent: THtmlPageWebModule;
    function GetItem(Index: integer): TWebModuleHandler;
    procedure SetItem(Index: integer; AValue: TWebModuleHandler);
  public
    property Parent: THtmlPageWebModule read FParent;
    function Add: TWebModuleHandler;
    function Insert(Index: integer): TWebModuleHandler;
    property Items[Index: integer]: TWebModuleHandler read GetItem write SetItem; default;
    constructor Create(AParent: THtmlPageWebModule);
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
  public
    property Handlers: TWebModuleHandlerList read FHandlers;
    property Request: TRequest read FRequest;
    property Response: TResponse read FResponse;
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
    procedure AfterConstruction; override;
    constructor CreateNew(AOwner: TComponent; CreateMode: Integer); override;
    destructor Destroy; override;
  end;

implementation

{ TWebModuleHandler }

constructor TWebModuleHandler.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  if ACollection = nil then
    FParent := nil
  else
    FParent := (ACollection as TWebModuleHandlerList).Parent;
end;

{ TWebModuleHandlerList }

function TWebModuleHandlerList.GetItem(Index: integer): TWebModuleHandler;
begin
  Result := (inherited GetItem(Index)) as TWebModuleHandler;
end;

procedure TWebModuleHandlerList.SetItem(Index: integer;
  AValue: TWebModuleHandler);
begin
  inherited SetItem(Index, AValue);
end;

function TWebModuleHandlerList.Add: TWebModuleHandler;
begin
  Result := (inherited Add) as TWebModuleHandler;
end;

function TWebModuleHandlerList.Insert(Index: integer): TWebModuleHandler;
begin
  Result := (inherited Insert(Index)) as TWebModuleHandler;
end;

constructor TWebModuleHandlerList.Create(AParent: THtmlPageWebModule);
begin
  inherited Create(TWebModuleHandler);
  FParent := AParent;
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

procedure THtmlPageWebModule.HandleRequest(ARequest: TRequest;
  AResponse: TResponse);
var
  Handled: boolean;
  I: integer;
begin
  inherited HandleRequest(ARequest, AResponse);
  FRequest := ARequest;
  FResponse := AResponse;
  try
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

constructor THtmlPageWebModule.CreateNew(AOwner: TComponent; CreateMode: Integer);
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

