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

{$mode objfpc}{$H+}{$macro on}

interface

uses
  Classes, SysUtils, webmodules, tsmiscwebmodules, editableobjects, HTTPDefs,
  users, authwebmodules, tswebpagesbase, webstrconsts;

type
  TEditableModuleHook = class;

  {$interfaces CORBA}
  IEditableWebModule = interface
    ['{3930B3B7-0A9F-41AF-929A-D416E801EE75}']
    function Manager: TEditableManager;
  end;

  IEditableModuleHook = interface
    ['{B788C26E-E9B2-4C14-B162-9E833D90295F}']
    function Hook: TEditableModuleHook;
  end;
  {$interfaces COM}

  { TEditableRedirectIfNoAccessHandler }

  TEditableRedirectIfNoAccessHandler = class(TWebModuleHandler)
  public
    procedure HandleRequest({%H-}ARequest: TRequest; AResponse: TResponse;
      var Handled: boolean); override;
  end;

  { TEditableModuleHook }

  TEditableModuleHook = class
  private
    FParent: THtmlPageWebModule;
    FInside: boolean;
  public
    property Parent: THtmlPageWebModule read FParent;
    property Inside: boolean read FInside;
    function Manager: TEditableManager; virtual; abstract;
    function EditableObject: TEditableObject;
    function EditableObjectName: string;
    function ObjectsRoot: string; virtual; abstract;
    procedure BeginAddHandlers; virtual;
    procedure EndAddHandlers; virtual;
    constructor Create(AParent: THtmlPageWebModule; AInside: boolean); virtual;
  end;

  TEditableModuleHookClass = class of TEditableModuleHook;

  // Here you can find modules implementing hooks for different module types.
  // We will use macros to do that.
  // Interface part

  {$define HOOKABLE_MODULE_CLASS := TEditablePageWebModule}
  {$define HOOKABLE_MODULE_BASE  := TUserWebModule}
  {$I editablehookedmodule_h.inc}

  {$define HOOKABLE_MODULE_CLASS := TEditablePostWebModule}
  {$define HOOKABLE_MODULE_BASE  := TPostUserWebModule}
  {$I editablehookedmodule_h.inc}

  {$define HOOKABLE_MODULE_CLASS := TEditableConfirmPasswordWebModule}
  {$define HOOKABLE_MODULE_BASE  := TConfirmPasswordWebModule}
  {$I editablehookedmodule_h.inc}

  {$undef HOOKABLE_MODULE_CLASS}
  {$undef HOOKABLE_MODULE_BASE}

  { TEditableObjectPostWebModule }

  TEditableObjectPostWebModule = class(TEditablePostWebModule)
  private
    FEditableObject: TEditableObject;
  protected
    property EditableObject: TEditableObject read FEditableObject;
    function Inside: boolean; override;
    procedure DoInsideHandlePost(ARequest: TRequest); virtual; abstract;
    procedure DoHandlePost(ARequest: TRequest); override;
  end;

  { TEditableObjListWebModule }

  TEditableObjListWebModule = class(TEditablePageWebModule)
  protected
    function Inside: boolean; override;
  end;

  { TEditableCreateNewWebModule }

  TEditableCreateNewWebModule = class(TEditablePostWebModule)
  protected
    function Inside: boolean; override;
    procedure DoHandlePost(ARequest: TRequest); override;
    function CanRedirect: boolean; override;
    function RedirectLocation: string; override;
  end;

  { TEditableDeleteWebModule }

  TEditableDeleteWebModule = class(TEditableConfirmPasswordWebModule)
  protected
    function Inside: boolean; override;
    procedure ConfirmationSuccess(var ACanRedirect: boolean;
      var ARedirect: string); override;
  end;

  { TEditableAccessWebModule }

  TEditableAccessWebModule = class(TEditableObjectPostWebModule)
  protected
    procedure DoInsideHandlePost(ARequest: TRequest); override;
  end;

  { TEditableViewWebModule }

  TEditableViewWebModule = class(TEditablePageWebModule)
  protected
    function Inside: boolean; override;
  end;

  { TEditableEditWebModule }

  TEditableEditWebModule = class(TEditableObjectPostWebModule)
  protected
    procedure DoInsideEdit(ATransaction: TEditableTransaction); virtual;
    procedure DoInsideHandlePost({%H-}ARequest: TRequest); override;
  end;

function EditableObjectNameFromRequest(ARequest: TRequest): string;
function EditableObjectFromRequest(ARequest: TRequest;
  AManager: TEditableManager): TEditableObject;

implementation

function EditableObjectNameFromRequest(ARequest: TRequest): string;
begin
  Result := ARequest.QueryFields.Values['object'];
end;

function EditableObjectFromRequest(ARequest: TRequest;
  AManager: TEditableManager): TEditableObject;
begin
  Result := AManager.GetObject(EditableObjectNameFromRequest(ARequest));
end;

{ TEditableObjectPostWebModule }

function TEditableObjectPostWebModule.Inside: boolean;
begin
  Result := True;
end;

procedure TEditableObjectPostWebModule.DoHandlePost(ARequest: TRequest);
begin
  FEditableObject := Hook.EditableObject;
  try
    DoHandlePost(ARequest);
  finally
    FreeAndNil(FEditableObject);
  end;
end;

{ TEditableEditWebModule }

procedure TEditableEditWebModule.DoInsideEdit(ATransaction: TEditableTransaction);
begin
  ATransaction.Title := Request.ContentFields.Values['title'];
end;

procedure TEditableEditWebModule.DoInsideHandlePost(ARequest: TRequest);
var
  Transaction: TEditableTransaction;
begin
  Transaction := EditableObject.CreateTransaction(User as TEditorUser);
  try
    DoInsideEdit(Transaction);
    Transaction.Commit;
  finally
    FreeAndNil(Transaction);
  end;
end;

{ TEditableViewWebModule }

function TEditableViewWebModule.Inside: boolean;
begin
  Result := True;
end;

{ TEditableAccessWebModule }

procedure TEditableAccessWebModule.DoInsideHandlePost(ARequest: TRequest);
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
  QueryType: string;
begin
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
end;

{ TEditableDeleteWebModule }

function TEditableDeleteWebModule.Inside: boolean;
begin
  Result := True;
end;

procedure TEditableDeleteWebModule.ConfirmationSuccess(var ACanRedirect: boolean;
  var ARedirect: string);
var
  ManagerSession: TEditableManagerSession;
begin
  ManagerSession := Manager.CreateManagerSession(User as TEditorUser);
  try
    ManagerSession.DeleteObject(Hook.EditableObjectName);
    ACanRedirect := True;
    ARedirect := Hook.ObjectsRoot;
  finally
    FreeAndNil(ManagerSession);
  end;
end;

{ TEditableCreateNewWebModule }

function TEditableCreateNewWebModule.Inside: boolean;
begin
  Result := False;
end;

procedure TEditableCreateNewWebModule.DoHandlePost(ARequest: TRequest);
var
  ManagerSession: TEditableManagerSession;
  ObjName, ObjTitle: string;
begin
  ManagerSession := Manager.CreateManagerSession(User as TEditorUser);
  try
    ObjName := ARequest.ContentFields.Values['name'];
    ObjTitle := ARequest.ContentFields.Values['title'];
    ManagerSession.CreateNewObject(ObjName, ObjTitle).Free;
  finally
    FreeAndNil(ManagerSession);
  end;
end;

function TEditableCreateNewWebModule.CanRedirect: boolean;
begin
  Result := True;
end;

function TEditableCreateNewWebModule.RedirectLocation: string;
begin
  Result := Hook.ObjectsRoot;
end;

{ TEditableObjListWebModule }

function TEditableObjListWebModule.Inside: boolean;
begin
  Result := False;
end;

// Here you can find modules implementing hooks for different module types.
// We will use macros to do that.
// Implementation part

{$define HOOKABLE_MODULE_CLASS := TEditablePageWebModule}
{$I editablehookedmodule.inc}

{$define HOOKABLE_MODULE_CLASS := TEditablePostWebModule}
{$I editablehookedmodule.inc}

{$define HOOKABLE_MODULE_CLASS := TEditableConfirmPasswordWebModule}
{$I editablehookedmodule.inc}

{$undef HOOKABLE_MODULE_CLASS}

{ TEditableModuleHook }

function TEditableModuleHook.EditableObject: TEditableObject;
begin
  if not Inside then
    raise EInvalidOperation.Create(SCannotAccessThroughOutside);
  Result := EditableObjectFromRequest(Parent.Request, Manager);
end;

function TEditableModuleHook.EditableObjectName: string;
begin
  if not Inside then
    raise EInvalidOperation.Create(SCannotAccessThroughOutside);
  Result := EditableObjectNameFromRequest(Parent.Request);
end;

procedure TEditableModuleHook.BeginAddHandlers;
begin
  if Inside then
    Parent.Handlers.Add(TEditableRedirectIfNoAccessHandler.Create);
end;

procedure TEditableModuleHook.EndAddHandlers;
begin
  // we need two redirectors
  // first to redirect back users that have no access
  // second to redirect self-deleted ones
  if Inside then
    Parent.Handlers.Add(TEditableRedirectIfNoAccessHandler.Create);
  Parent.Handlers.Insert(0, TDeclineNotLoggedWebModuleHandler.Create(EditorsSet));
end;

constructor TEditableModuleHook.Create(AParent: THtmlPageWebModule; AInside: boolean);
begin
  FParent := AParent;
  FInside := AInside;
end;

{ TEditableRedirectIfNoAccessHandler }

procedure TEditableRedirectIfNoAccessHandler.HandleRequest(ARequest: TRequest;
  AResponse: TResponse; var Handled: boolean);
var
  Hook: TEditableModuleHook;
  EditableObject: TEditableObject;
  User: TEditorUser;
  Redirect: boolean;
begin
  Hook := (Parent as IEditableModuleHook).Hook;
  Redirect := False;
  try
    EditableObject := Hook.EditableObject;
    try
      User := (Parent as IUserWebModule).User as TEditorUser;
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
    AResponse.Location := Hook.ObjectsRoot;
    AResponse.Code := 303;
    Handled := True;
  end;
end;

end.
