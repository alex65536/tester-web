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
  SysUtils, webstrconsts, submissions, testerprimitives, typinfo, problemstats,
  submissionlanguages, strverdicts;

type
  TTesterWebVerdict = (
    wvUnknown,
    wvTestFailed,
    wvCompiling,
    wvRunning,
    wvFinishing
    );

const
  STesterWebVerdicts: array [TTesterWebVerdict] of string = (
    SVerdictUnknown,
    SVerdictTestFailed,
    SVerdictCompiling,
    SVerdictRunning,
    SVerdictFinishing
    );

type

  { TSubmissionInfo }

  TSubmissionInfo = class
  private
    FSubmission: TViewSubmission;
    FID: integer;
    FSubmitTime: TDateTime;
    FOwnerName: string;
    FLanguage: string;
    FVerdictKind: string;
    FTestCase: integer;
    FTime: TProblemTime;
    FMemory: TProblemMemory;
    FScore: double;
    function GetVerdictStr: string;
    procedure GetVerdictTestCase(out AVerdictKind: string; out ATestCase: integer);
    procedure GetTimeMemoryScore(out ATime: TProblemTime; out AMemory: TProblemMemory;
      out AScore: double);
  public
    property Submission: TViewSubmission read FSubmission;
    property ID: integer read FID;
    property SubmitTime: TDateTime read FSubmitTime;
    property OwnerName: string read FOwnerName;
    property Language: string read FLanguage;
    property VerdictKind: string read FVerdictKind;
    property VerdictStr: string read GetVerdictStr;
    property TestCase: integer read FTestCase; // -1 if no test case
    property Time: TProblemTime read FTime;
    property Memory: TProblemMemory read FMemory;
    property Score: double read FScore;
    constructor Create(ASubmission: TViewSubmission);
  end;

function CompilerVerdictToStr(AVerdict: TCompilerVerdict): string;
function TestVerdictToStr(AVerdict: TTestVerdict): string;
function TesterWebVerdictToStr(AVerdict: TTesterWebVerdict): string;

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

{ TSubmissionInfo }

procedure TSubmissionInfo.GetVerdictTestCase(out AVerdictKind: string; out
  ATestCase: integer);

  function ProcessFinished(out ATestCase: integer): string;
  var
    I: integer;
  begin
    with Submission.Results do
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
    with Submission.Results do
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
  if Submission.Results = nil then
  begin
    ATestCase := -1;
    AVerdictKind := TesterWebVerdictToStr(wvUnknown);
    Exit;
  end;
  if (not Submission.Finished) or (not Submission.Success) then
  begin
    AVerdictKind := ProcessUnfinished(ATestCase);
    if not Submission.Success then
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
  if Submission.Results = nil then
    Exit;
  Stats := TProblemStats.Create(Submission.Results);
  try
    if Stats.CanCountTime then
      ATime := Stats.MaxTime;
    if Stats.CanCountMemory then
      AMemory := Stats.MaxMemory;
    AScore := Submission.Results.TotalScore;
  finally
    FreeAndNil(Stats);
  end;
end;

function TSubmissionInfo.GetVerdictStr: string;
var
  C: TCompilerVerdict;
  V: TTestVerdict;
  W: TTesterWebVerdict;
begin
  Result := '';
  // check for compile verdicts
  for C in TCompilerVerdict do
    if CompilerVerdictToStr(C) = FVerdictKind then
      Exit(SCompilerVerdicts[C]);
  // check for run verdicts
  for V in TTestVerdict do
    if TestVerdictToStr(V) = FVerdictKind then
      Exit(STestVerdicts[V]);
  // check for tsWeb verdicts
  for W in TTesterWebVerdict do
    if TesterWebVerdictToStr(W) = FVerdictKind then
      Exit(STesterWebVerdicts[W]);
end;

constructor TSubmissionInfo.Create(ASubmission: TViewSubmission);
begin
  FSubmission := ASubmission;
  FID := Submission.ID;
  FSubmitTime := Submission.SubmitTime;
  FOwnerName := Submission.OwnerName;
  FLanguage := LanguageFullCompilerNames(Submission.Language);
  GetVerdictTestCase(FVerdictKind, FTestCase);
  GetTimeMemoryScore(FTime, FMemory, FScore);
end;

end.
