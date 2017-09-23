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
unit tswebeditablemodules;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, webmodules, tsmiscwebmodules, editableobjects, HTTPDefs,
  users, authwebmodules, tswebpagesbase;

type

  { TEditableModuleHook }

  TEditableModuleHook = class
  private
    FParent: THtmlPageWebModule;
  public
    property Parent: THtmlPageWebModule read FParent;
    function Manager: TEditableManager; virtual; abstract;
    function RedirectIfNoAccess: string; virtual; abstract;
    constructor Create(AParent: THtmlPageWebModule);
  end;

  {$interfaces CORBA}

  { IEditableWebModule }

  IEditableWebModule = interface
    ['{3930B3B7-0A9F-41AF-929A-D416E801EE75}']
    function Manager: TEditableManager;
  end;
  {$interfaces COM}

  { TEditableHtmlPageWebModule }

  TEditableHtmlPageWebModule = class(THtmlPageWebModule)
  public
    procedure AfterConstruction; override;
  end;

  { TEditableRedirectIfNoAccessHandler }

  TEditableRedirectIfNoAccessHandler = class(TWebModuleHandler)
  public
    procedure HandleRequest({%H-}ARequest: TRequest; AResponse: TResponse;
      var Handled: boolean); override;
  end;

  { TEditableWebModule }

  TEditableWebModule = class(TPostUserWebModule, IEditableWebModule)
  private
    FHook: TEditableModuleHook;
  protected
    property Hook: TEditableModuleHook read FHook;
    function Manager: TEditableManager;
    function CreateHook: TEditableModuleHook; virtual; abstract;
  public
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

  { TEditableCreateNewWebModule }

  TEditableCreateNewWebModule = class(TEditableWebModule)
  protected
    procedure DoHandlePost(ARequest: TRequest); override;
  end;

  { TEditableInsideWebModule }

  TEditableInsideWebModule = class(TEditableWebModule)
  public
    procedure AfterConstruction; override;
  end;

  { TEditableAccessWebModule }

  TEditableAccessWebModule = class(TEditableInsideWebModule)
  protected
    procedure DoHandlePost(ARequest: TRequest); override;
  end;

  { TEditableEditWebModule }

  TEditableEditWebModule = class(TEditableInsideWebModule)
  protected
    procedure DoHandlePost(ARequest: TRequest); override;
  end;

  { TEditableDeleteWebModule }

  TEditableDeleteWebModule = class(TConfirmPasswordWebModule, IEditableWebModule)
  protected
    function Manager: TEditableManager; virtual; abstract;
    procedure ConfirmationSuccess(var ACanRedirect: boolean;
      var {%H-}ARedirect: string); override;
  end;

function EditableObjectFromRequest(ARequest: TRequest; AManager: TEditableManager): TEditableObject;
function EditableObjectFromRequest(AModule: THtmlPageWebModule): TEditableObject;

implementation

function EditableObjectFromRequest(ARequest: TRequest; AManager: TEditableManager): TEditableObject;
var
  ObjectName: string;
begin
  ObjectName := ARequest.QueryFields.Values['object'];
  Result := AManager.GetObject(ObjectName);
end;

function EditableObjectFromRequest(AModule: THtmlPageWebModule): TEditableObject;
begin
  Result := EditableObjectFromRequest(AModule.Request, (AModule as IEditableWebModule).Manager);
end;

{ TEditableDeleteWebModule }

procedure TEditableDeleteWebModule.ConfirmationSuccess(var ACanRedirect: boolean;
  var ARedirect: string);
var
  ManagerSession: TEditableManagerSession;
begin
  ManagerSession := Manager.CreateManagerSession(User as TEditorUser);
  try
    ManagerSession.DeleteObject(Request.QueryFields.Values['object']);
    ACanRedirect := True;
    ARedirect := DocumentRoot + '/';
    // TODO : redirect to object home page (/problems or /contests, ...)
  finally
    FreeAndNil(ManagerSession);
  end;
end;

{ TEditableEditWebModule }

procedure TEditableEditWebModule.DoHandlePost(ARequest: TRequest);
var
  Transation: TEditableTransaction;
  EditableObject: TEditableObject;
begin
  EditableObject := EditableObjectFromRequest(Self);
  try
    Transation := EditableObject.CreateTransaction(User as TEditorUser);
    try
      Transation.Title := ARequest.ContentFields.Values['title'];
      Transation.Commit;
    finally
      FreeAndNil(Transation);
    end;
  finally
    FreeAndNil(EditableObject);
  end;
end;

{ TEditableInsideWebModule }

procedure TEditableInsideWebModule.AfterConstruction;
begin
  // we need two redirectors
  // first to redirect back users that have no access
  // second to redirect self-deleted ones
  Handlers.Add(TEditableRedirectIfNoAccessHandler.Create);
  inherited AfterConstruction;
  Handlers.Add(TEditableRedirectIfNoAccessHandler.Create);
end;

{ TEditableRedirectIfNoAccessHandler }

procedure TEditableRedirectIfNoAccessHandler.HandleRequest(ARequest: TRequest;
  AResponse: TResponse; var Handled: boolean);
var
  EditableObject: TEditableObject;
  User: TEditorUser;
  Redirect: boolean;
begin
  Redirect := False;
  try
    EditableObject := EditableObjectFromRequest(Parent as THtmlPageWebModule);
    try
      User := (Parent as TPostUserWebModule).User as TEditorUser;
      Redirect := EditableObject.GetAccessRights(User) = erNone;
    finally
      FreeAndNil(EditableObject);
    end;
  except
    on E: EEditableAction do
      Redirect := True
    else
      raise;
  end;
  if Redirect then
  begin
    AResponse.Location := (Parent as TEditableWebModule).Hook.RedirectIfNoAccess;
    AResponse.Code := 303;
    Handled := True;
  end;
end;

{ TEditableModuleHook }

constructor TEditableModuleHook.Create(AParent: THtmlPageWebModule);
begin
  FParent := AParent;
end;

{ TEditableAccessWebModule }

procedure TEditableAccessWebModule.DoHandlePost(ARequest: TRequest);
var
  AccessSession: TEditableObjectAccessSession;
  Target: TUserInfo;

  procedure HandleAddUser;
  begin
    AccessSession.AddUser(Target);
  end;

  procedure HandleDeleteUser;
  begin
    AccessSession.DeleteUser(Target);
  end;

  procedure HandleAccessChangeUser;
  var
    NewAccess: TEditableAccessRights;
  begin
    NewAccess := StrToAccessRights(ARequest.ContentFields.Values['access-type']);
    AccessSession.GrantAccessRights(Target, NewAccess);
  end;

var
  EditableObject: TEditableObject;
  QueryType: string;
begin
  EditableObject := EditableObjectFromRequest(Self);
  try
    AccessSession := EditableObject.CreateAccessSession(User as TEditorUser);
    try
      Target := UserManager.GetUserInfo(ARequest.ContentFields.Values['user']);
      try
        QueryType := ARequest.ContentFields.Values['query'];
        if QueryType = 'add-user' then
          HandleAddUser
        else if QueryType = 'delete-user' then
          HandleDeleteUser
        else if QueryType = 'access-change-user' then
          HandleAccessChangeUser;
      finally
        FreeAndNil(Target);
      end;
    finally
      FreeAndNil(AccessSession);
    end;
  finally
    FreeAndNil(EditableObject);
  end;
end;

{ TEditableCreateNewWebModule }

procedure TEditableCreateNewWebModule.DoHandlePost(ARequest: TRequest);
var
  MgrSession: TEditableManagerSession;
  MgrName, MgrTitle: string;
begin
  MgrSession := Manager.CreateManagerSession(User as TEditorUser);
  try
    MgrName := ARequest.ContentFields.Values['name'];
    MgrTitle := ARequest.ContentFields.Values['title'];
    MgrSession.CreateNewObject(MgrName, MgrTitle).Free;
  finally
    FreeAndNil(MgrSession);
  end;
end;

{ TEditableWebModule }

function TEditableWebModule.Manager: TEditableManager;
begin
  Result := Hook.Manager;
end;

constructor TEditableWebModule.CreateNew(AOwner: TComponent;
  CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  FHook := CreateHook;
end;

procedure TEditableWebModule.AfterConstruction;
begin
  inherited AfterConstruction;
  Handlers.Insert(0, TDeclineNotLoggedWebModuleHandler.Create(EditorsSet));
end;

destructor TEditableWebModule.Destroy;
begin
  FreeAndNil(FHook);
  inherited Destroy;
end;

{ TEditableHtmlPageWebModule }

procedure TEditableHtmlPageWebModule.AfterConstruction;
begin
  inherited AfterConstruction;
  Handlers.Insert(0, TDeclineNotLoggedWebModuleHandler.Create(EditorsSet));
end;

end.

