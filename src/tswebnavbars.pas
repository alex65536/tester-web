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
unit tswebnavbars;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, navbars, htmlpages, htmlpreprocess, tswebpagesbase;

type

  { TTesterNavBarElement }

  TTesterNavBarElement = class(TNavBarElement)
  protected
    procedure DoFillVariables; override;
    procedure DoGetSkeleton(Strings: TIndentTaggedStrings); override;
  end;

  { TTesterNavBar }

  TTesterNavBar = class(TNavBar)
  protected
    procedure DoGetSkeleton(Strings: TIndentTaggedStrings); override;
    function DoCreateElement(AParent: THtmlPage): TNavBarElement; override;
  end;

implementation

{ TTesterNavBar }

procedure TTesterNavBar.DoGetSkeleton(Strings: TIndentTaggedStrings);
begin
  LoadTemplateStrings(Strings, '', 'nav');
end;

function TTesterNavBar.DoCreateElement(AParent: THtmlPage): TNavBarElement;
begin
  Result := TTesterNavBarElement.Create(AParent);
end;

{ TTesterNavBarElement }

procedure TTesterNavBarElement.DoFillVariables;
begin
  inherited DoFillVariables;
  Storage.ItemsAsText['navActive'] := ' class="active"';
end;

procedure TTesterNavBarElement.DoGetSkeleton(Strings: TIndentTaggedStrings);
begin
  LoadTemplateStrings(Strings, '', 'navItem');
end;

end.

