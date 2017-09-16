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
unit tswebpages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tswebauthfeatures, tswebfeatures, tswebpagesbase, navbars,
  webstrconsts, htmlpreprocess;

type

  { TDefaultHtmlPageBase }

  TDefaultHtmlPageBase = class(TContentHtmlPage, IPageNavBar)
  private
    FNavBar: TNavBar;
  protected
    procedure AddFeatures; override;
    function CreateNavBar: TNavBar; virtual; abstract;
    procedure DoSetVariables; override;
    function GetNavBar: TNavBar;
  public
    procedure Clear; override;
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TDefaultHtmlPage }

  TDefaultHtmlPage = class(TDefaultHtmlPageBase)
  protected
    procedure AddFeatures; override;
  end;

  { TAuthHtmlPage }

  TAuthHtmlPage = class(TDefaultHtmlPageBase, IAuthHtmlPage)
  private
    FError: string;
    FSuccess: string;
  protected
    procedure AddFeatures; override;
    procedure SetError(AValue: string);
    function GetError: string;
    procedure SetSuccess(AValue: string);
    function GetSuccess: string;
    procedure DoGetInnerContents(Strings: TIndentTaggedStrings); override;
  end;

  { TLoginHtmlPage }

  TLoginHtmlPage = class(TAuthHtmlPage)
  protected
    procedure AddFeatures; override;
  public
    procedure AfterConstruction; override;
  end;

  { TRegisterHtmlPage }

  TRegisterHtmlPage = class(TAuthHtmlPage)
  protected
    procedure AddFeatures; override;
  public
    procedure AfterConstruction; override;
  end;

  { TConfirmPasswordHtmlPage }

  TConfirmPasswordHtmlPage = class(TAuthHtmlPage)
  protected
    procedure AddFeatures; override;
  public
    procedure AfterConstruction; override;
  end;

  { TSettingsHtmlPage }

  TSettingsHtmlPage = class(TAuthHtmlPage)
  protected
    procedure AddFeatures; override;
    procedure DoGetInnerContents(Strings: TIndentTaggedStrings); override;
  public
    procedure AfterConstruction; override;
  end;

implementation

{ TSettingsHtmlPage }

procedure TSettingsHtmlPage.AddFeatures;
begin
  inherited AddFeatures;
  AddFeature(TUserBarFeature);
  AddFeature(TSettingsPageFeature);
end;

procedure TSettingsHtmlPage.DoGetInnerContents(Strings: TIndentTaggedStrings);
begin
  Strings.Text := '~#+settings;';
end;

procedure TSettingsHtmlPage.AfterConstruction;
begin
  inherited AfterConstruction;
  Title := SUpdateSettingsTitle;
end;

{ TConfirmPasswordHtmlPage }

procedure TConfirmPasswordHtmlPage.AddFeatures;
begin
  inherited AddFeatures;
  AddFeature(TUserBarFeature);
  AddFeature(TAuthConfirmPasswordFeature);
end;

procedure TConfirmPasswordHtmlPage.AfterConstruction;
begin
  inherited AfterConstruction;
  Title := SConfirmPasswordTitle;
end;

{ TDefaultHtmlPage }

procedure TDefaultHtmlPage.AddFeatures;
begin
  inherited AddFeatures;
  AddFeature(TUserBarFeature);
end;

{ TRegisterHtmlPage }

procedure TRegisterHtmlPage.AddFeatures;
begin
  inherited AddFeatures;
  AddFeature(TAuthRegisterFormFeature);
end;

procedure TRegisterHtmlPage.AfterConstruction;
begin
  inherited AfterConstruction;
  Title := SRegisterTitle;
end;

{ TLoginHtmlPage }

procedure TLoginHtmlPage.AddFeatures;
begin
  inherited AddFeatures;
  AddFeature(TAuthLoginFormFeature);
end;

procedure TLoginHtmlPage.AfterConstruction;
begin
  inherited AfterConstruction;
  Title := SLoginTitle;
end;

{ TAuthHtmlPage }

procedure TAuthHtmlPage.AddFeatures;
begin
  inherited AddFeatures;
  AddFeature(TAuthFormFeature);
end;

procedure TAuthHtmlPage.SetError(AValue: string);
begin
  FError := AValue;
end;

function TAuthHtmlPage.GetError: string;
begin
  Result := FError;
end;

procedure TAuthHtmlPage.SetSuccess(AValue: string);
begin
  FSuccess := AValue;
end;

function TAuthHtmlPage.GetSuccess: string;
begin
  Result := FSuccess;
end;

procedure TAuthHtmlPage.DoGetInnerContents(Strings: TIndentTaggedStrings);
begin
  Strings.Text := '~#+authContent;';
end;

{ TDefaultHtmlPageBase }

procedure TDefaultHtmlPageBase.AddFeatures;
begin
  inherited AddFeatures;
  AddFeature(THeaderFeature);
  AddFeature(TNavBarFeature);
  AddFeature(TContentFeature);
  AddFeature(TFooterFeature);
end;

procedure TDefaultHtmlPageBase.DoSetVariables;
begin
  FNavBar.ActiveCaption := Title;
  inherited DoSetVariables;
end;

function TDefaultHtmlPageBase.GetNavBar: TNavBar;
begin
  Result := FNavBar;
end;

procedure TDefaultHtmlPageBase.Clear;
begin
  inherited Clear;
  FNavBar.Clear;
end;

constructor TDefaultHtmlPageBase.Create;
begin
  inherited Create;
  FNavBar := CreateNavBar;
end;

destructor TDefaultHtmlPageBase.Destroy;
begin
  FreeAndNil(FNavBar);
  inherited Destroy;
end;

end.

