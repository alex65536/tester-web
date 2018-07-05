{
  This file is part of Tester Web

  Copyright (C) 2018 Alexander Kernozhitsky <sh200105@mail.ru>

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
unit logging;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, richtextconsole, tswebdirectories, LazFileUtils, LazUTF8,
  tswebutils, webstrconsts;

type
  TLogEventSeverity = (lsInfo, lsNote, lsWarning, lsError, lsFatal);

const
  ColorsBySeverity: array [TLogEventSeverity] of TConsoleColor =
    (cclGreen, cclPurple, cclYellow, cclRed, cclRed);
  SeverityNames: array [TLogEventSeverity] of string =
    (SLogInfo, SLogNote, SLogWarning, SLogError, SLogFatal);

procedure LogWrite(ASeverity: TLogEventSeverity; const Line: string);

procedure LogInfo(const S: string);
procedure LogInfo(const S: string; const Args: array of const);

procedure LogNote(const S: string);
procedure LogNote(const S: string; const Args: array of const);

procedure LogWarning(const S: string);
procedure LogWarning(const S: string; const Args: array of const);

procedure LogError(const S: string);
procedure LogError(const S: string; const Args: array of const);

procedure LogFatal(const S: string);
procedure LogFatal(const S: string; const Args: array of const);

procedure LogException(ASeverity: TLogEventSeverity; E: Exception);

implementation

var
  LogFile: TextFile;

procedure DoLogWrite(const S: string);
begin
  Write(S);
  Write(LogFile, S);
end;

procedure DoLogWriteLn(const S: string = '');
begin
  WriteLn(S);
  WriteLn(LogFile, S);
end;

procedure DoLogFlush;
begin
  Flush(Output);
  Flush(LogFile);
end;

procedure LogWrite(ASeverity: TLogEventSeverity; const Line: string);
const
  DateTimeFmt = 'yyyy-mm-dd hh:nn:ss';
var
  S: string;
begin
  for S in AdjustLineBreaks(Line, tlbsLF).Split([#10]) do
  begin
    DoLogWrite(FormatDateTime(DateTimeFmt, Now) + ' ');
    rtcSetFgColor(ColorsBySeverity[ASeverity]);
    rtcSetBold;
    DoLogWrite('[' + SeverityNames[ASeverity] + ']');
    if ASeverity <> lsFatal then
      rtcResetStyle;
    DoLogWrite(': ' + S);
    rtcResetStyle;
    DoLogWriteLn;
  end;
  DoLogFlush;
end;

procedure LogInfo(const S: string);
begin
  LogWrite(lsInfo, S);
end;

procedure LogInfo(const S: string; const Args: array of const);
begin
  LogWrite(lsInfo, Format(S, Args));
end;

procedure LogNote(const S: string);
begin
  LogWrite(lsNote, S);
end;

procedure LogNote(const S: string; const Args: array of const);
begin
  LogWrite(lsNote, Format(S, Args));
end;

procedure LogWarning(const S: string);
begin
  LogWrite(lsWarning, S);
end;

procedure LogWarning(const S: string; const Args: array of const);
begin
  LogWrite(lsWarning, Format(S, Args));
end;

procedure LogError(const S: string);
begin
  LogWrite(lsError, S);
end;

procedure LogError(const S: string; const Args: array of const);
begin
  LogWrite(lsError, Format(S, Args));
end;

procedure LogFatal(const S: string);
begin
  LogWrite(lsFatal, S);
end;

procedure LogFatal(const S: string; const Args: array of const);
begin
  LogWrite(lsFatal, Format(S, Args));
end;

procedure OpenLogFile;
var
  FileName: string;
begin
  FileName := AppendPathDelim(tswebdirectories.InternalDirLocation) + 'server.log';
  AssignFile(LogFile, UTF8ToSys(FileName));
  if not FileExistsUTF8(FileName) then
    Rewrite(LogFile)
  else
    Append(LogFile);
end;

procedure LogException(ASeverity: TLogEventSeverity; E: Exception);
begin
  LogWrite(ASeverity, Format(SErrorMsg, [E.ClassName, E.Message]));
  LogWrite(ASeverity, SErrorStackTrace);
  LogWrite(ASeverity, DumpBackTrace);
  LogWrite(ASeverity, ' ');
end;

initialization
  OpenLogFile;

finalization
  CloseFile(LogFile);

end.

