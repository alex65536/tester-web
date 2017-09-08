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
unit htmlpages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, htmlpreprocess, fgl, AvgLvlTree, webstrconsts;

type
  EHtmlPage = class(Exception);

  { THtmlPage }

  THtmlPage = class
  private
    FContent: string;
    FPreprocessor: THtmlPreprocessor;
    FRendered: boolean;
  protected
    procedure DoSetVariables; virtual; abstract;
    procedure DoGetSkeleton(Strings: TIndentTaggedStrings); virtual; abstract;
  public
    property Preprocessor: THtmlPreprocessor read FPreprocessor;
    property Content: string read FContent;
    procedure Render;
    procedure Clear; virtual;
    constructor Create;
    destructor Destroy; override;
  end;

  TFeaturedHtmlPage = class;
  THtmlPageFeature = class;
  THtmlPageFeatureClass = class of THtmlPageFeature;
  THtmlPageFeatureList = specialize TFPGList<THtmlPageFeatureClass>;

  THtmlPageFeatureState = (hpfsUnsatisfied, hpfsProcessing, hpfsSatisfied);

  { THtmlPageFeatureMap }

  THtmlPageFeatureMap = class
  private
    FParent: TFeaturedHtmlPage;
    FMap: TPointerToPointerTree;
  public
    property Parent: TFeaturedHtmlPage read FParent;
    function AddFeature(AClass: THtmlPageFeatureClass): THtmlPageFeature;
    function GetFeature(AClass: THtmlPageFeatureClass): THtmlPageFeature;
    procedure AddAndSatisfy(AClass: THtmlPageFeatureClass);
    procedure UnsatisfyAll;
    constructor Create(AParent: TFeaturedHtmlPage);
    destructor Destroy; override;
  end;

  { THtmlPageFeature }

  THtmlPageFeature = class
  private
    FParent: TFeaturedHtmlPage;
    FState: THtmlPageFeatureState;
  public
    property Parent: TFeaturedHtmlPage read FParent;
    property State: THtmlPageFeatureState read FState write FState;
    procedure Satisfy; virtual; abstract;
    procedure DependsOn(ADependencies: THtmlPageFeatureList); virtual; abstract;
    constructor Create(AParent: TFeaturedHtmlPage); virtual;
  end;

  { TFeaturedHtmlPage }

  TFeaturedHtmlPage = class(THtmlPage)
  private
    FMap: THtmlPageFeatureMap;
  protected
    FVariables: TVariableStorage;
    FPageParts: TVariableStorage;
    procedure AddFeature(AClass: THtmlPageFeatureClass);
    procedure AddFeatures; virtual;
    procedure DoSetVariables; override;
    procedure StartupVariablesInitialize; virtual;
  public
    property Variables: TVariableStorage read FVariables;
    property PageParts: TVariableStorage read FPageParts;
    procedure Clear; override;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TFeaturedHtmlPage }

procedure TFeaturedHtmlPage.AddFeature(AClass: THtmlPageFeatureClass);
begin
  FMap.AddAndSatisfy(AClass);
end;

procedure TFeaturedHtmlPage.AddFeatures;
begin
  // do nothing
end;

procedure TFeaturedHtmlPage.DoSetVariables;
begin
  AddFeatures;
end;

procedure TFeaturedHtmlPage.StartupVariablesInitialize;
begin
  // do nothing
end;

procedure TFeaturedHtmlPage.Clear;
begin
  FVariables.Clear;
  FPageParts.Clear;
  FMap.UnsatisfyAll;
  StartupVariablesInitialize;
  inherited Clear;
end;

constructor TFeaturedHtmlPage.Create;
begin
  inherited Create;
  FMap := THtmlPageFeatureMap.Create(Self);
  FVariables := TTreeVariableStorage.Create;
  FPageParts := TTreeVariableStorage.Create;
  Preprocessor.Storages.Add(FVariables);
  Preprocessor.Storages.Add(FPageParts);
  StartupVariablesInitialize;
end;

destructor TFeaturedHtmlPage.Destroy;
begin
  FreeAndNil(FMap);
  FreeAndNil(FVariables);
  FreeAndNil(FPageParts);
  inherited Destroy;
end;

{ THtmlPageFeature }

constructor THtmlPageFeature.Create(AParent: TFeaturedHtmlPage);
begin
  FState := hpfsUnsatisfied;
  FParent := AParent;
end;

{ THtmlPageFeatureMap }

function THtmlPageFeatureMap.AddFeature(AClass: THtmlPageFeatureClass): THtmlPageFeature;
begin
  if FMap.Contains(AClass) then
    Result := THtmlPageFeature(FMap[AClass])
  else
  begin
    Result := AClass.Create(FParent);
    FMap[AClass] := Result;
  end;
end;

function THtmlPageFeatureMap.GetFeature(AClass: THtmlPageFeatureClass): THtmlPageFeature;
begin
  if FMap.Contains(AClass) then
    Result := THtmlPageFeature(FMap[AClass])
  else
    Result := nil;
end;

procedure THtmlPageFeatureMap.AddAndSatisfy(AClass: THtmlPageFeatureClass);
var
  Feature: THtmlPageFeature;
  Dependencies: THtmlPageFeatureList;
  DependFeature: THtmlPageFeatureClass;
begin
  // add feature
  Feature := AddFeature(AClass);

  // check its state
  if Feature.State = hpfsSatisfied then
    Exit;
  if Feature.State = hpfsProcessing then
    raise EHtmlPage.Create(SLoopDependencies);

  try
    // satisfy dependencies recursively
    Feature.State := hpfsProcessing;
    Dependencies := THtmlPageFeatureList.Create;
    try
      Feature.DependsOn(Dependencies);
      for DependFeature in Dependencies do
        AddAndSatisfy(DependFeature);
    finally
      FreeAndNil(Dependencies);
    end;

    // satisfy self
    Feature.Satisfy;
    Feature.State := hpfsSatisfied;
  except
    Feature.State := hpfsUnsatisfied;
    raise;
  end;
end;

procedure THtmlPageFeatureMap.UnsatisfyAll;
var
  It: PPointerToPointerItem;
begin
  for It in FMap do
    with THtmlPageFeature(It^.Value) do
      State := hpfsUnsatisfied;
end;

constructor THtmlPageFeatureMap.Create(AParent: TFeaturedHtmlPage);
begin
  FParent := AParent;
  FMap := TPointerToPointerTree.Create;
end;

destructor THtmlPageFeatureMap.Destroy;
begin
  FreeAndNil(FMap);
  inherited Destroy;
end;

{ THtmlPage }

procedure THtmlPage.Render;
var
  Skeleton: TIndentTaggedStrings;
begin
  if FRendered then
    Exit;
  try
    Skeleton := TIndentTaggedStrings.Create;
    try
      DoSetVariables;
      DoGetSkeleton(Skeleton);
      FContent := FPreprocessor.Preprocess(Skeleton);
    finally
      FreeAndNil(Skeleton);
    end;
  except
    Clear;
    raise;
  end;
  FRendered := True;
end;

procedure THtmlPage.Clear;
begin
  FRendered := False;
end;

constructor THtmlPage.Create;
begin
  FPreprocessor := THtmlPreprocessor.Create;
  FRendered := False;
end;

destructor THtmlPage.Destroy;
begin
  FreeAndNil(FPreprocessor);
  inherited Destroy;
end;

end.

