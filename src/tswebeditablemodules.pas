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
  users;

type
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

  { TEditablePostWebModule }

  TEditablePostWebModule = class(TPostUserWebModule, IEditableWebModule)
  protected
    function Manager: TEditableManager; virtual; abstract;
  public
    procedure AfterConstruction; override;
  end;

  { TEditableCreateNewWebModule }

  TEditableCreateNewWebModule = class(TEditablePostWebModule)
  protected
    procedure DoHandlePost(ARequest: TRequest); override;
  end;

  { TEditableAccessWebModule }

  TEditableAccessWebModule = class(TEditablePostWebModule)
    procedure DoHandlePost(ARequest: TRequest); override;
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

{ TEditablePostWebModule }

procedure TEditablePostWebModule.AfterConstruction;
begin
  inherited AfterConstruction;
  Handlers.Add(TDeclineNotLoggedWebModuleHandler.Create(EditorsSet));
end;

{ TEditableHtmlPageWebModule }

procedure TEditableHtmlPageWebModule.AfterConstruction;
begin
  inherited AfterConstruction;
  Handlers.Add(TDeclineNotLoggedWebModuleHandler.Create(EditorsSet));
end;

end.

