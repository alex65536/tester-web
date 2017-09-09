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
  Classes, SysUtils, htmlpages, fpWeb, HTTPDefs;

type

  { THtmlPageWebModule }

  THtmlPageWebModule = class(TFPWebModule)
  protected
    function DoCreatePage: THtmlPage; virtual; abstract;
    procedure DoPageAfterConstruction({%H-}APage: THtmlPage); virtual;
    function CreatePage: THtmlPage;
    procedure DoOnRequest({%H-}ARequest: TRequest; {%H-}AResponse: TResponse;
      var AHandled: Boolean); override;
  public
    procedure AfterConstruction; override;
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

procedure THtmlPageWebModule.DoOnRequest(ARequest: TRequest;
  AResponse: TResponse; var AHandled: Boolean);
var
  APage: THtmlPage;
begin
  APage := CreatePage;
  try
    APage.UpdateRequest;
    APage.UpdateResponse;
    AHandled := True;
  finally
    FreeAndNil(APage);
  end;
end;

procedure THtmlPageWebModule.AfterConstruction;
begin
  inherited AfterConstruction;
  CreateSession := True;
end;

end.

