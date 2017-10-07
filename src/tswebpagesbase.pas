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
unit tswebpagesbase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, userpages, htmlpreprocess, LazFileUtils, serverconfig,
  users, htmlpages;

type

  { TTesterHtmlPage }

  TTesterHtmlPage = class(TUserPage)
  private
    FTitle: string;
  protected
    procedure DoGetSkeleton(Strings: TIndentTaggedStrings); override;
  public
    property Title: string read FTitle write FTitle;
    function GenerateUserLink(AInfo: TUserInfo): string;
    function GenerateUserLink(const Username: string): string;
  end;

  { TTesterHtmlPageElement }

  TTesterHtmlPageElement = class(THtmlPageElement)
  private
    function GetParent: TTesterHtmlPage;
  public
    property Parent: TTesterHtmlPage read GetParent;
  end;

  { TTesterHtmlListedPageElement }

  TTesterHtmlListedPageElement = class(THtmlListedPageElement)
  private
    function GetParent: TTesterHtmlPage;
  public
    property Parent: TTesterHtmlPage read GetParent;
  end;

  {$interfaces CORBA}
  IPostHtmlPage = interface
    ['{48726FDF-4025-4D9C-A462-1D88AEE0DF89}']
    function GetError: string;
    function GetSuccess: string;
    procedure SetError(AValue: string);
    procedure SetSuccess(AValue: string);
    property Error: string read GetError write SetError;
    property Success: string read GetSuccess write SetSuccess;
  end;
  {$interfaces COM}

  { TContentHtmlPage }

  TContentHtmlPage = class(TTesterHtmlPage)
  protected
    procedure DoGetInnerContents(Strings: TIndentTaggedStrings); virtual; abstract;
  public
    procedure GetInnerContents(Strings: TIndentTaggedStrings);
  end;

var
  DocumentRoot: string = '';
  DataRoot: string = '/data';

function GetTemplateLocation(const ALocation, AName: string): string;
procedure LoadTemplateStrings(AStrings: TIndentTaggedStrings; const ALocation,
  AName: string);

implementation

function GetTemplateLocation(const ALocation, AName: string): string;
var
  Dir: string;
begin
  Dir := AppendPathDelim(Config.Location_TemplatesDir) + ALocation;
  Result := AppendPathDelim(Dir) + AName + '.html';
end;

procedure LoadTemplateStrings(AStrings: TIndentTaggedStrings; const ALocation,
  AName: string);
begin
  AStrings.LoadFromFile(GetTemplateLocation(ALocation, AName));
end;

{ TTesterHtmlListedPageElement }

function TTesterHtmlListedPageElement.GetParent: TTesterHtmlPage;
begin
  Result := (inherited Parent) as TTesterHtmlPage;
end;

{ TTesterHtmlPageElement }

function TTesterHtmlPageElement.GetParent: TTesterHtmlPage;
begin
  Result := (inherited Parent) as TTesterHtmlPage;
end;

{ TContentHtmlPage }

procedure TContentHtmlPage.GetInnerContents(Strings: TIndentTaggedStrings);
begin
  DoGetInnerContents(Strings);
end;

{ TTesterHtmlPage }

procedure TTesterHtmlPage.DoGetSkeleton(Strings: TIndentTaggedStrings);
begin
  LoadTemplateStrings(Strings, '', 'skeleton');
end;

function TTesterHtmlPage.GenerateUserLink(AInfo: TUserInfo): string;
var
  Storage: TTreeVariableStorage;
  Strings: TIndentTaggedStrings;
begin
  Storage := TTreeVariableStorage.Create(Preprocessor.Storages);
  try
    Preprocessor.Storages.Add(Storage);
    with Storage do
    begin
      ItemsAsText['linkUserName'] := AInfo.Username;
      ItemsAsText['linkUserRoleLower'] := UserRoleToStr(AInfo.Role).Substring(2).ToLower;
    end;
    Strings := TIndentTaggedStrings.Create;
    try
      LoadTemplateStrings(Strings, '', 'userNameLink');
      Result := Preprocessor.Preprocess(Strings);
    finally
      FreeAndNil(Strings);
    end;
  finally
    FreeAndNil(Storage);
  end;
end;

function TTesterHtmlPage.GenerateUserLink(const Username: string): string;
var
  Info: TUserInfo;
begin
  Info := UserManager.GetUserInfo(Username);
  try
    Result := GenerateUserLink(Info);
  finally
    FreeAndNil(Info);
  end;
end;

end.

