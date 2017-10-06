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
unit submissioninfo;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, webstrconsts, testerprimitives, typinfo, problemstats, strverdicts,
  testresults;

type
  TTesterWebVerdict = (
    wvUnknown,
    wvTestFailed,
    wvWaiting,
    wvCompiling,
    wvRunning,
    wvFinishing
    );

const
  STesterWebVerdicts: array [TTesterWebVerdict] of string = (
    SVerdictUnknown,
    SVerdictTestFailed,
    SVerdictWaiting,
    SVerdictCompiling,
    SVerdictRunning,
    SVerdictFinishing
    );

type

  { TSubmissionInfo }

  TSubmissionInfo = class
  private
    FResults: TTestedProblem;
    FFinished: boolean;
    FSuccess: boolean;
    FVerdictKind: string;
    FTestCase: integer;
    FTime: TProblemTime;
    FMemory: TProblemMemory;
    FScore: double;
    procedure GetVerdictTestCase(out AVerdictKind: string; out ATestCase: integer);
    procedure GetTimeMemoryScore(out ATime: TProblemTime; out AMemory: TProblemMemory;
      out AScore: double);
  public
    property Results: TTestedProblem read FResults write FResults;
    property Finished: boolean read FFinished write FFinished;
    property Success: boolean read FSuccess write FSuccess;

    property VerdictKind: string read FVerdictKind;
    property TestCase: integer read FTestCase; // -1 if no test case
    property Time: TProblemTime read FTime;
    property Memory: TProblemMemory read FMemory;
    property Score: double read FScore;

    procedure RetrieveInfo;
  end;

function CompilerVerdictToStr(AVerdict: TCompilerVerdict): string;
function TestVerdictToStr(AVerdict: TTestVerdict): string;
function TesterWebVerdictToStr(AVerdict: TTesterWebVerdict): string;

function VerdictKindToStr(const AVerdictKind: string): string;

implementation

function CompilerVerdictToStr(AVerdict: TCompilerVerdict): string;
begin
  Result := GetEnumName(TypeInfo(TCompilerVerdict), Ord(AVerdict));
end;

function TestVerdictToStr(AVerdict: TTestVerdict): string;
begin
  Result := GetEnumName(TypeInfo(TTestVerdict), Ord(AVerdict));
end;

function TesterWebVerdictToStr(AVerdict: TTesterWebVerdict): string;
begin
  Result := GetEnumName(TypeInfo(TTesterWebVerdict), Ord(AVerdict));
end;

function VerdictKindToStr(const AVerdictKind: string): string;
var
  C: TCompilerVerdict;
  V: TTestVerdict;
  W: TTesterWebVerdict;
begin
  Result := '';
  // check for compile verdicts
  for C in TCompilerVerdict do
    if CompilerVerdictToStr(C) = AVerdictKind then
      Exit(SCompilerVerdicts[C]);
  // check for run verdicts
  for V in TTestVerdict do
    if TestVerdictToStr(V) = AVerdictKind then
      Exit(STestVerdicts[V]);
  // check for tsWeb verdicts
  for W in TTesterWebVerdict do
    if TesterWebVerdictToStr(W) = AVerdictKind then
      Exit(STesterWebVerdicts[W]);
end;

{ TSubmissionInfo }

procedure TSubmissionInfo.GetVerdictTestCase(out AVerdictKind: string; out
  ATestCase: integer);

  function ProcessFinished(out ATestCase: integer): string;
  var
    I: integer;
  begin
    with Results do
    begin
      ATestCase := -1;
      // check for compile errors
      if CompileVerdict <> cvSuccess then
        Exit(CompilerVerdictToStr(CompileVerdict));
      // check for errors on tests
      for I := 0 to TestResultsCount - 1 do
      begin
        ATestCase := I;
        if TestResults[I].Verdict <> veAccepted then
          Exit(TestVerdictToStr(TestResults[I].Verdict));
      end;
      // return "accepted"
      ATestCase := -1;
      Result := TestVerdictToStr(veAccepted);
    end;
  end;

  function ProcessUnfinished(out ATestCase: integer): string;
  var
    I: integer;
  begin
    with Results do
    begin
      ATestCase := -1;
      // check if compiled
      if CompileVerdict = cvWaiting then
        Exit(TesterWebVerdictToStr(wvCompiling));
      // check if ran all tests
      for I := 0 to TestResultsCount - 1 do
      begin
        ATestCase := I;
        if TestResults[I].Verdict = veWaiting then
          Exit(TesterWebVerdictToStr(wvRunning));
      end;
      // say it's finishing
      ATestCase := -1;
      Result := TesterWebVerdictToStr(wvFinishing);
    end;
  end;

begin
  if Results = nil then
  begin
    ATestCase := -1;
    if not Finished then
      AVerdictKind := TesterWebVerdictToStr(wvWaiting)
    else if Success then
      AVerdictKind := TesterWebVerdictToStr(wvUnknown)
    else
      AVerdictKind := TesterWebVerdictToStr(wvTestFailed);
    Exit;
  end;
  if (not Finished) or (not Success) then
  begin
    AVerdictKind := ProcessUnfinished(ATestCase);
    if not Success then
      AVerdictKind := TesterWebVerdictToStr(wvTestFailed);
  end
  else
    AVerdictKind := ProcessFinished(ATestCase);
end;

procedure TSubmissionInfo.GetTimeMemoryScore(out ATime: TProblemTime; out
  AMemory: TProblemMemory; out AScore: double);
var
  Stats: TProblemStats;
begin
  ATime := 0;
  AMemory := 0;
  AScore := 0.0;
  if Results = nil then
    Exit;
  Stats := TProblemStats.Create(Results);
  try
    if Stats.CanCountTime then
      ATime := Stats.MaxTime;
    if Stats.CanCountMemory then
      AMemory := Stats.MaxMemory;
    AScore := Results.TotalScore;
  finally
    FreeAndNil(Stats);
  end;
end;

procedure TSubmissionInfo.RetrieveInfo;
begin
  GetVerdictTestCase(FVerdictKind, FTestCase);
  GetTimeMemoryScore(FTime, FMemory, FScore);
end;

end.
