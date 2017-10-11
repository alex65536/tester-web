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
unit tswebdatetimefeatures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, htmlpages, tswebpagesbase, tswebfeatures, webstrconsts,
  htmlpreprocess, dateutils;

type
  {$interfaces CORBA}

  { IDateTimePage }

  IDateTimePage = interface
    ['{32BA301D-6562-42A8-B43E-FAB4E42FEC88}']
    function GetDateTime: TDateTime;
  end;
  {$interfaces COM}

  { TMonthOption }

  TMonthOption = class(TTesterHtmlPageElement)
  private
    FIndex: integer;
    FSelected: boolean;
  protected
    procedure DoFillVariables; override;
    procedure DoGetSkeleton(Strings: TIndentTaggedStrings); override;
  public
    property Index: integer read FIndex;
    property Selected: boolean read FSelected;
    constructor Create(AParent: THtmlPage; AIndex: integer; ASelected: boolean);
  end;

  { TMonthList }

  TMonthList = class(TTesterHtmlListedPageElement)
  private
    FSelectedMonth: integer;
  protected
    procedure DoFillList;
    procedure DoFillVariables; override;
    procedure DoGetSkeleton(Strings: TIndentTaggedStrings); override;
  public
    property SelectedMonth: integer read FSelectedMonth;
    constructor Create(AParent: THtmlPage; ASelectedMonth: integer);
  end;

  { TDateEditFeature }

  TDateEditFeature = class(TTesterPageFeature)
  public
    procedure Satisfy; override;
  end;

  { TTimeEditFeature }

  TTimeEditFeature = class(TTesterPageFeature)
  public
    procedure Satisfy; override;
  end;

implementation

{ TTimeEditFeature }

procedure TTimeEditFeature.Satisfy;
var
  Time: TDateTime;
begin
  Time := (Parent as IDateTimePage).GetDateTime;
  with Parent.Variables do
  begin
    ItemsAsText['timeCurHour'] := IntToStr(HourOf(Time));
    ItemsAsText['timeCurMinute'] := IntToStr(MinuteOf(Time));
    ItemsAsText['timeCurSecond'] := IntToStr(SecondOf(Time));
  end;
  LoadPagePart('', 'timeEdit');
end;

{ TDateEditFeature }

procedure TDateEditFeature.Satisfy;
var
  List: TMonthList;
  Date: TDateTime;
begin
  Date := (Parent as IDateTimePage).GetDateTime;
  with Parent.Variables do
  begin
    ItemsAsText['dateCurDay'] := IntToStr(DayOf(Date));
    ItemsAsText['dateCurYear'] := IntToStr(YearOf(Date));
  end;
  List := TMonthList.Create(Parent, MonthOf(Date));
  try
    Parent.AddElementPagePart('dateEdit', List);
  finally
    FreeAndNil(List);
  end;
end;

{ TMonthList }

procedure TMonthList.DoFillList;
var
  I: integer;
begin
  for I := 1 to 12 do
    List.Add(TMonthOption.Create(Parent, I, I = SelectedMonth));
end;

procedure TMonthList.DoFillVariables;
begin
  AddListToVariable('dateEditMonths');
end;

procedure TMonthList.DoGetSkeleton(Strings: TIndentTaggedStrings);
begin
  Strings.LoadFromFile(TemplateLocation('', 'dateEdit'));
end;

constructor TMonthList.Create(AParent: THtmlPage; ASelectedMonth: integer);
begin
  inherited Create(AParent);
  FSelectedMonth := ASelectedMonth;
  DoFillList;
end;

{ TMonthOption }

procedure TMonthOption.DoFillVariables;
begin
  with Storage do
  begin
    ItemsAsText['monthNum'] := IntToStr(Index);
    if Selected then
      ItemsAsText['monthSel'] := ' selected';
    ItemsAsText['monthStr'] := SMonths[Index];
  end;
end;

procedure TMonthOption.DoGetSkeleton(Strings: TIndentTaggedStrings);
begin
  Strings.LoadFromFile(TemplateLocation('', 'dateEditMonth'));
end;

constructor TMonthOption.Create(AParent: THtmlPage; AIndex: integer;
  ASelected: boolean);
begin
  inherited Create(AParent);
  FIndex := AIndex;
  FSelected := ASelected;
end;

end.

