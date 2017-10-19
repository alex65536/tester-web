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
unit tswebsolveelements;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contests, contestproblems, standings, htmlpreprocess,
  htmlpages, tswebpagesbase;

type
  {$interfaces CORBA}

  { IContestPage }

  IContestPage = interface
    ['{7C18C5C5-C400-4CEB-866C-6EB91084F07E}']
    function Contest: TContest;
  end;
  {$interfaces COM}

  { TSolveContestListItem }

  TSolveContestListItem = class(TTesterHtmlPageElement)
  private
    FTransaction: TContestViewTransaction;
  protected
    procedure DoFillVariables; override;
    procedure DoGetSkeleton(Strings: TIndentTaggedStrings); override;
  public
    property Transaction: TContestViewTransaction read FTransaction;
    constructor Create(AParent: THtmlPage; ATransaction: TContestViewTransaction);
    destructor Destroy; override;
  end;

  { TSolveContestList }

  TSolveContestList = class(TTesterHtmlListedPageElement)
  private
    FManagerSession: TContestManagerSession;
  protected
    procedure DoFillList;
    procedure DoFillVariables; override;
    procedure DoGetSkeleton(Strings: TIndentTaggedStrings); override;
  public
    property ManagerSession: TContestManagerSession read FManagerSession;
    constructor Create(AParent: THtmlPage; ASession: TContestManagerSession);
  end;

implementation

{ TSolveContestListItem }

procedure TSolveContestListItem.DoFillVariables;
begin
  with Storage do
  begin
    ItemsAsText['solveName'] := Transaction.EditableObject.Name;
    ItemsAsText['solveTitle'] := Transaction.Title;
    if Transaction.Status = csNotStarted then
      ItemsAsText['solveNameLink'] := '~+#solveInactiveName;'
    else
      ItemsAsText['solveNameLink'] := '~+#solveActiveName;';
    ItemsAsText['solveDuration'] := ;
  end;
{<td>~+solveNameLink;</td>
<td>~solveStartTime;</td>
<td>~solveDuration;</td>
<td>~solveStatus;</td>
<td>~solveAccessType;</td> }
end;

procedure TSolveContestListItem.DoGetSkeleton(Strings: TIndentTaggedStrings);
begin

end;

constructor TSolveContestListItem.Create(AParent: THtmlPage;
  ATransaction: TContestViewTransaction);
begin

end;

destructor TSolveContestListItem.Destroy;
begin
  inherited Destroy;
end;

{ TSolveContestList }

procedure TSolveContestList.DoFillList;
begin

end;

procedure TSolveContestList.DoFillVariables;
begin

end;

procedure TSolveContestList.DoGetSkeleton(Strings: TIndentTaggedStrings);
begin

end;

constructor TSolveContestList.Create(AParent: THtmlPage;
  ASession: TContestManagerSession);
begin

end;

end.

