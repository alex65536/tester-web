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

  { IEditableWebModule }

  IEditableWebModule = interface
    ['{3930B3B7-0A9F-41AF-929A-D416E801EE75}']
    function Manager: TEditableManager;
    function NeedAccessRights: TEditableAccessRightsSet;
  end;

  { IEditableModuleHook }

  IEditableModuleHook = interface
    ['{B788C26E-E9B2-4C14-B162-9E833D90295F}']
    function Hook: TEditableModuleHook;
  end;
  {$interfaces COM}

  TEditableNoAccessHandlerBehaviour = (ahbError, ahbRedirect);

  { TEditableNoAccessHandler }

  TEditableNoAccessHandler = class(TWebModuleHandler)
  private
    FBehaviour: TEditableNoAccessHandlerBehaviour;
    FNeedAccessRigths: TEditableAccessRightsSet;
  public
    property Behaviour: TEditableNoAccessHandlerBehaviour read FBehaviour;
    property NeedAccessRigths: TEditableAccessRightsSet read FNeedAccessRigths;
    procedure HandleRequest({%H-}ARequest: TRequest; AResponse: TResponse;
      var Handled: boolean); override;
    constructor Create(ABehaviour: TEditableNoAccessHandlerBehaviour;
      ANeedAccessRigths: TEditableAccessRightsSet);
  end;

  { TEditableModuleHook }

  TEditableModuleHook = class
  private
    FParent: THtmlPageWebModule;
    FInside: boolean;
  protected
    function NeedAccessRights: TEditableAccessRightsSet;
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
    procedure DoSessionCreated; override;
    procedure DoAfterRequest; override;
    function CanRedirect: boolean; override;
    function RedirectLocation: string; override;
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
    procedure DoHandlePost(ARequest: TRequest); override;
  end;

  { TEditableViewWebModule }

  TEditableViewWebModule = class(TEditablePageWebModule)
  protected
    function Inside: boolean; override;
  end;

  { TEditableEditWebModule }

  TEditableEditWebModule = class(TEditableObjectPostWebModule)
  protected
    function NeedAccessRights: TEditableAccessRightsSet; override;
    procedure DoInsideEdit(ATransaction: TEditableTransaction); virtual;
    procedure DoHandlePost({%H-}ARequest: TRequest); override;
    function CanRedirect: boolean; override;
  end;

  { TEditableSettingsWebModule }

  TEditableSettingsWebModule = class(TEditableObjectPostWebModule)
  protected
    function Inside: boolean; override;
    procedure DoHandlePost(ARequest: TRequest); override;
  end;

function EditableObjectNameFromRequest(ARequest: TRequest;
  const AFieldName: string = 'object'): string;
function EditableObjectFromRequest(ARequest: TRequest; AManager: TEditableManager;
  const AFieldName: string = 'object'): TEditableObject;

implementation

function EditableObjectNameFromRequest(ARequest: TRequest;
  const AFieldName: string): string;
begin
  Result := ARequest.QueryFields.Values[AFieldName];
end;

function EditableObjectFromRequest(ARequest: TRequest;
  AManager: TEditableManager; const AFieldName: string): TEditableObject;
begin
  Result := AManager.GetObject(EditableObjectNameFromRequest(ARequest, AFieldName));
end;

{ TEditableSettingsWebModule }

function TEditableSettingsWebModule.Inside: boolean;
begin
  Result := True;
end;

procedure TEditableSettingsWebModule.DoHandlePost(ARequest: TRequest);
var
  NewName: string;
  ManagerSession: TEditableManagerSession;
begin
  if ARequest.ContentFields.Values['query'] = 'clone' then
  begin
    NewName := ARequest.ContentFields.Values['new-name'];
    ManagerSession := Manager.CreateManagerSession(User);
    try
      ManagerSession.CloneObject(EditableObject, NewName);
      Success := SSuccessfulClone;
    finally
      FreeAndNil(ManagerSession);
    end;
  end;
end;

{ TEditableObjectPostWebModule }

function TEditableObjectPostWebModule.Inside: boolean;
begin
  Result := True;
end;

procedure TEditableObjectPostWebModule.DoSessionCreated;
begin
  inherited DoSessionCreated;
  FEditableObject := Hook.EditableObject;
end;

procedure TEditableObjectPostWebModule.DoAfterRequest;
begin
  FreeAndNil(FEditableObject);
  inherited DoAfterRequest;
end;

function TEditableObjectPostWebModule.CanRedirect: boolean;
begin
  Result := True;
end;

function TEditableObjectPostWebModule.RedirectLocation: string;
begin
  Result := Request.URI;
end;

{ TEditableEditWebModule }

function TEditableEditWebModule.NeedAccessRights: TEditableAccessRightsSet;
begin
  Result := AccessCanWriteSet;
end;

procedure TEditableEditWebModule.DoInsideEdit(ATransaction: TEditableTransaction);
begin
  ATransaction.Title := Request.ContentFields.Values['title'];
end;

procedure TEditableEditWebModule.DoHandlePost(ARequest: TRequest);
var
  Transaction: TEditableTransaction;
begin
  Transaction := EditableObject.CreateTransaction(User as TEditorUser);
  try
    DoInsideEdit(Transaction);
    Transaction.Commit;
    Success := SObjectEditSuccessful;
  finally
    FreeAndNil(Transaction);
  end;
end;

function TEditableEditWebModule.CanRedirect: boolean;
begin
  Result := False;
end;

{ TEditableViewWebModule }

function TEditableViewWebModule.Inside: boolean;
begin
  Result := True;
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

function TEditableModuleHook.NeedAccessRights: TEditableAccessRightsSet;
begin
  Result := (Parent as IEditableWebModule).NeedAccessRights;
end;

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
    Parent.Handlers.Add(TEditableNoAccessHandler.Create(ahbError, NeedAccessRights));
end;

procedure TEditableModuleHook.EndAddHandlers;
begin
  // we need two NoAccessHandlers
  // first to throw away users that have no access
  // second to redirect self-deleted ones
  if Inside then
    Parent.Handlers.Add(TEditableNoAccessHandler.Create(ahbRedirect, NeedAccessRights));
  Parent.Handlers.Insert(0, TDeclineNotLoggedWebModuleHandler.Create(EditorsSet));
end;

constructor TEditableModuleHook.Create(AParent: THtmlPageWebModule; AInside: boolean);
begin
  FParent := AParent;
  FInside := AInside;
end;

{ TEditableNoAccessHandler }

procedure TEditableNoAccessHandler.HandleRequest(ARequest: TRequest;
  AResponse: TResponse; var Handled: boolean);
var
  Hook: TEditableModuleHook;
  EditableObject: TEditableObject;
  User: TEditorUser;
  Good: boolean;

  procedure SendRedirect;
  begin
    AResponse.Location := Hook.ObjectsRoot;
    AResponse.Code := 303;
    Handled := True;
  end;

begin
  Hook := (Parent as IEditableModuleHook).Hook;
  Good := True;
  try
    EditableObject := Hook.EditableObject;
    try
      User := (Parent as IUserWebModule).User as TEditorUser;
      Good := EditableObject.GetAccessRights(User) in NeedAccessRigths;
    finally
      FreeAndNil(EditableObject);
    end;
  except
    on E: EEditableAction do
    begin
      if Behaviour = ahbError then
        raise
      else
        Good := False;
    end
    else
      raise;
  end;
  if not Good then
  begin
    case Behaviour of
      ahbError: raise EEditableAccessDenied.Create(SAccessDenied);
      ahbRedirect: SendRedirect;
    end;
  end;
end;

constructor TEditableNoAccessHandler.Create(ABehaviour: TEditableNoAccessHandlerBehaviour;
  ANeedAccessRigths: TEditableAccessRightsSet);
begin
  inherited Create;
  FBehaviour := ABehaviour;
  FNeedAccessRigths := ANeedAccessRigths;
end;

end.
