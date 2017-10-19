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
unit tswebsolvemodules;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, webmodules, tswebsolvepages, fphttp, htmlpages, tswebeditablemodules,
  tsmiscwebmodules, tswebsolveelements, contests, users, editableobjects,
  HTTPDefs, tswebpagesbase, tswebmanagers;

type

  { TSolveContestListModule }

  TSolveContestListModule = class(THtmlPageWebModule)
  public
    function DoCreatePage: THtmlPage; override;
  end;

  { TRedirectIfNoContestAccessWebHandler }

  TRedirectIfNoContestAccessWebHandler = class(TWebModuleHandler)
  public
    procedure HandleRequest({%H-}ARequest: TRequest; AResponse: TResponse;
      var Handled: boolean); override;
  end;

  { TSolveBaseContestWebModule }

  TSolveBaseContestWebModule = class(TUserWebModule, IContestSolveModule)
  private
    FContest: TContest;
  protected
    procedure DoSessionCreated; override;
    procedure DoAfterRequest; override;
  public
    function Contest: TContest;
    procedure AfterConstruction; override;
  end;

  { TSolvePostContestWebModule }

  TSolvePostContestWebModule = class(TPostUserWebModule, IContestSolveModule)
  private
    FContest: TContest;
  protected
    procedure DoSessionCreated; override;
    procedure DoAfterRequest; override;
  public
    function Contest: TContest;
    procedure AfterConstruction; override;
  end;

  { TSolveProblemListWebModule }

  TSolveProblemListWebModule = class(TSolveBaseContestWebModule)
  protected
    function DoCreatePage: THtmlPage; override;
  end;

function SolveContestFromRequest(ARequest: TRequest): TContest;
function SolveContestNameFromRequest(ARequest: TRequest): string;

implementation

function SolveContestFromRequest(ARequest: TRequest): TContest;
begin
  Result := EditableObjectFromRequest(ARequest, ContestManager, 'contest') as TContest;
end;

function SolveContestNameFromRequest(ARequest: TRequest): string;
begin
  Result := EditableObjectNameFromRequest(ARequest, 'contest');
end;

{ TSolveProblemListWebModule }

function TSolveProblemListWebModule.DoCreatePage: THtmlPage;
begin
  Result := TSolveProblemListPage.Create;
end;

{ TSolvePostContestWebModule }

procedure TSolvePostContestWebModule.DoSessionCreated;
begin
  inherited DoSessionCreated;
  FContest := SolveContestFromRequest(Request);
end;

procedure TSolvePostContestWebModule.DoAfterRequest;
begin
  FreeAndNil(FContest);
  inherited DoAfterRequest;
end;

function TSolvePostContestWebModule.Contest: TContest;
begin
  Result := FContest;
end;

procedure TSolvePostContestWebModule.AfterConstruction;
begin
  inherited AfterConstruction;
  Handlers.Add(TRedirectIfNoContestAccessWebHandler.Create);
end;

{ TSolveBaseContestWebModule }

procedure TSolveBaseContestWebModule.DoSessionCreated;
begin
  inherited DoSessionCreated;
  FContest := SolveContestFromRequest(Request);
end;

procedure TSolveBaseContestWebModule.DoAfterRequest;
begin
  FreeAndNil(FContest);
  inherited DoAfterRequest;
end;

function TSolveBaseContestWebModule.Contest: TContest;
begin
  Result := FContest;
end;

procedure TSolveBaseContestWebModule.AfterConstruction;
begin
  inherited AfterConstruction;
  Handlers.Add(TRedirectIfNoContestAccessWebHandler.Create);
end;

{ TRedirectIfNoContestAccessWebHandler }

procedure TRedirectIfNoContestAccessWebHandler.HandleRequest(
  ARequest: TRequest; AResponse: TResponse; var Handled: boolean);
var
  User: TUser;
  Contest: TContest;
  Transaction: TTestContestTransaction;
  Redirect: boolean;
begin
  User := (Parent as IUserWebModule).User;
  Contest := (Parent as IContestSolveModule).Contest;
  Redirect := False;
  try
    Transaction := Contest.CreateTestTransaction(User);
    try
      Redirect := not Transaction.CanReadData;
    finally
      FreeAndNil(Transaction);
    end;
  except
    on E: EEditableAction do
      Redirect := True
    else
      raise;
  end;
  if Redirect then
  begin
    AResponse.Location := DocumentRoot;
    AResponse.Code := 303;
    Handled := True;
  end;
end;

{ TSolveContestListModule }

function TSolveContestListModule.DoCreatePage: THtmlPage;
begin
  Result := TSolveListPage.Create;
end;

initialization
  RegisterHTTPModule('solve', TSolveContestListModule, True);
  RegisterHTTPModule('solve-contest', TSolveProblemListWebModule, True);

end.

