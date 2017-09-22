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
  Classes, SysUtils, webmodules, tsmiscwebmodules, editableobjects, HTTPDefs;

type

  { TEditableHtmlPageWebModule }

  TEditableHtmlPageWebModule = class(THtmlPageWebModule)
  public
    procedure AfterConstruction; override;
  end;

  { TEditableCreateNewWebModule }

  TEditableCreateNewWebModule = class(TPostUserWebModule)
  protected
    function Manager: TEditableManager; virtual; abstract;
    procedure DoHandlePost(ARequest: TRequest); override;
  public
    procedure AfterConstruction; override;
  end;

  { TEditableAccessWebModule }

  TEditableAccessWebModule = class(TPostUserWebModule)
  protected
    function Manager: TEditableManager; virtual; abstract;
    procedure DoHandlePost(ARequest: TRequest); override;
  public
    procedure AfterConstruction; override;
  end;

implementation

{ TEditableAccessWebModule }

procedure TEditableAccessWebModule.DoHandlePost(ARequest: TRequest);
begin
  // TODO : Write it !!!
end;

procedure TEditableAccessWebModule.AfterConstruction;
begin
  inherited AfterConstruction;
  Handlers.Add(TDeclineNotLoggedWebModuleHandler.Create(EditorsSet));
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

procedure TEditableCreateNewWebModule.AfterConstruction;
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

