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
unit tswebcrypto;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, base64;

procedure TrulyRandomSequence(Len: integer; var Bytes: TByteDynArray);
function Base64RandomSequence(Len: integer): ansistring;

implementation

{$IfDef Unix}
  {$Define __UnixRandomSeq__}
{$Else}
  {$IfDef Windows}
    {$Define __WinRandomSeq__}
  {$Else}
    // Don't know a way how to use cryptographically strong on this platform.
    // Using a simple pascal random().
    // We must warn this is insecure.
    {$Warning TrulyRandomSequence doesn't use a cryptographically strong random function.}
    {$Warning Using TrulyRandomSequence may be insecure on this platform}
    {$Warning Please consider it or implement something better}
    {$Define __SimpleRandomSeq__}
  {$Endif}
{$EndIf}

{$IfDef __WinRandomSeq__}
uses
  JwaWinCrypt;
{$EndIf}

function Base64RandomSequence(Len: integer): ansistring;
var
  Bytes: TByteDynArray;
  ArrLen: integer;
  DstStream: TMemoryStream;
  Encoder: TBase64EncodingStream;
begin
  // generate bytes
  ArrLen := (Len * 6) div 8 + 4;
  SetLength(Bytes, ArrLen);
  TrulyRandomSequence(ArrLen, Bytes);
  // encode them into base64
  DstStream := TMemoryStream.Create;
  try
    Encoder := TBase64EncodingStream.Create(DstStream);
    try
      Encoder.WriteBuffer(Bytes[0], ArrLen);
      Encoder.Flush;
      // push into string
      DstStream.Position := 0;
      SetLength(Result, Len);
      DstStream.ReadBuffer(Result[1], Len);
    finally
      FreeAndNil(Encoder);
    end;
  finally
    FreeAndNil(DstStream);
  end;
end;

{$IfDef __UnixRandomSeq__}
procedure TrulyRandomSequence(Len: integer; var Bytes: TByteDynArray);
var
  F: TFileStream;
begin
  // we read bytes from /dev/urandom
  F := TFileStream.Create('/dev/urandom', fmOpenRead);
  try
    F.ReadBuffer(Bytes[0], Len);
  finally
    FreeAndNil(F);
  end;
end;
{$EndIf}

{$IfDef __WinRandomSeq__}
procedure TrulyRandomSequence(Len: integer; var Bytes: TByteDynArray);
var
  Provider: HCRYPTPROV;
begin
  // we use Windows' built-in CryptGenRandom() function
  // see this forum thread:
  // http://forum.lazarus.freepascal.org/index.php/topic,35523.msg235007.html#msg235007
  Provider := 0; // to avoid hints
  if not CryptAcquireContext(Provider, nil, nil, PROV_RSA_FULL,
    CRYPT_VERIFYCONTEXT or CRYPT_SILENT) then
    RaiseLastOSError;
  try
    if not CryptGenRandom(Provider, Len, @Bytes[0]) then
      RaiseLastOSError;
  finally
    CryptReleaseContext(Provider, 0);
  end;
end;
{$EndIf}

{$IfDef __SimpleRandomSeq__}
procedure TrulyRandomSequence(Len: integer; var Bytes: TByteDynArray);
var
  I: integer;
begin
  // have nothing better than generating bytes with standard pascal's random()
  for I := 0 to Len - 1 do
    Bytes[I] := Random(256);
end;
{$EndIf}

initialization
  {$IfDef __SimpleRandomSeq}
  Randomize;
  {$EndIf}

end.
