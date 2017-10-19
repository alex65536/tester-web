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
unit tswebsolvefeatures;

{$mode objfpc}{$H+}

interface

// TODO : For each in-contest page, view contest status and title bar!!!

uses
  SysUtils, tswebfeatures, tswebsolveelements, contests, tswebmanagers, userpages,
  editableobjects, users, htmlpages;

type

  { TSolveContestListFeature }

  TSolveContestListFeature = class(TTesterPageFeature)
  public
    procedure Satisfy; override;
  end;

  { TSolveContestBaseFeature }

  TSolveContestBaseFeature = class(TTesterPageFeature)
  public
    procedure Satisfy; override;
  end;

  { TSolveProblemListFeature }

  TSolveProblemListFeature = class(TTesterPageFeature)
  public
    procedure Satisfy; override;
    procedure DependsOn(ADependencies: THtmlPageFeatureList); override;
  end;

implementation

{ TSolveContestBaseFeature }

procedure TSolveContestBaseFeature.Satisfy;
begin
  with Parent.Variables do
  begin
    ItemsAsText['solveContest'] := (Parent as IContestSolvePage).ContestName;
  end;
end;

{ TSolveProblemListFeature }

procedure TSolveProblemListFeature.Satisfy;
var
  Contest: TContest;
  User: TUser;
  Transaction: TTestContestTransaction;
  List: TSolveProblemList;
begin
  Contest := (Parent as IContestSolvePage).Contest;
  User := (Parent as TUserPage).User;
  Transaction := Contest.CreateTestTransaction(User);
  try
    List := TSolveProblemList.Create(Parent, Transaction);
    try
      Parent.AddElementPagePart('solveProblemList', List);
    finally
      FreeAndNil(List);
    end;
  finally
    FreeAndNil(Transaction);
  end;
end;

procedure TSolveProblemListFeature.DependsOn(ADependencies: THtmlPageFeatureList);
begin
  inherited DependsOn(ADependencies);
  ADependencies.Add(TSolveContestBaseFeature);
end;

{ TSolveContestListFeature }

procedure TSolveContestListFeature.Satisfy;
var
  ManagerSession: TEditableManagerSession;
  List: TSolveContestList;
begin
  ManagerSession := ContestManager.CreateManagerSession((Parent as TUserPage).User);
  try
    List := TSolveContestList.Create(Parent, ManagerSession as TContestManagerSession);
    try
      Parent.AddElementPagePart('solveList', List);
    finally
      FreeAndNil(List);
    end;
  finally
    FreeAndNil(ManagerSession);
  end;
end;

end.

