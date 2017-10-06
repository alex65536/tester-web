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
  Classes, SysUtils, Types, base64, Math;

procedure TrulyRandomSequence(Len: integer; out Bytes: TByteDynArray);
function RandomSequenceBase64(Len: integer): string;
function RandomFileName(Len: integer): string;
function BytesToBase64(const Bytes: TByteDynArray; Len: integer): string;

function GenSalt: string;
function HashPassword(const Password, Salt: string): string;

function SlowCompareStrings(const A, B: string): boolean;

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

uses
{$IfDef __WinRandomSeq__}
  JwaWinCrypt,
{$EndIf}
  serverconfig, scrypt;

function RandomSequenceBase64(Len: integer): string;
var
  ArrLen: integer;
  Bytes: TByteDynArray;
begin
  // generate bytes
  ArrLen := (Len * 6) div 8 + 4;
  TrulyRandomSequence(ArrLen, Bytes);
  // push into string
  Result := Copy(BytesToBase64(Bytes, ArrLen), 1, Len);
end;

function RandomFileName(Len: integer): string;
begin
  Result := RandomSequenceBase64(Len);
  Result := Result.Replace('+', '_').Replace('/', '-');
end;

function BytesToBase64(const Bytes: TByteDynArray; Len: integer): string;
var
  DstStream: TMemoryStream;
  Encoder: TBase64EncodingStream;
begin
  DstStream := TMemoryStream.Create;
  try
    Encoder := TBase64EncodingStream.Create(DstStream);
    try
      Encoder.WriteBuffer(Bytes[0], Len);
      Encoder.Flush;
      // push into string
      DstStream.Position := 0;
      SetLength(Result, DstStream.Size);
      DstStream.ReadBuffer(Result[1], DstStream.Size);
    finally
      FreeAndNil(Encoder);
    end;
  finally
    FreeAndNil(DstStream);
  end;
end;

function GenSalt: string;
begin
  Result := RandomSequenceBase64(Config.Crypto_SaltLen);
end;

function HashPassword(const Password, Salt: string): string;
var
  Hash: TByteDynArray;
  HashLen, NeedLen: integer;
  N, R, P: integer;
begin
  NeedLen := Config.Crypto_HashLen;
  HashLen := (NeedLen * 6) div 8 + 4;
  N := 1 shl Config.Crypto_SCrypt_LogN;
  R := Config.Crypto_SCrypt_R;
  P := Config.Crypto_SCrypt_P;
  SetLength(Hash, HashLen);
  scrypt_kdf(@Password[1], Length(Password), @Salt[1], Length(Salt),
    N, R, P, Hash[0], HashLen);
  Result := Copy(BytesToBase64(Hash, HashLen), 1, NeedLen);
end;

function SlowCompareStrings(const A, B: string): boolean;
var
  Diff: integer;
  I: integer;
begin
  Diff := Length(A) xor Length(B);
  for I := 1 to Min(Length(A), Length(B)) do
    Diff := Diff or (Ord(A[I]) xor Ord(B[I]));
  Result := Diff = 0;
end;

{$IfDef __UnixRandomSeq__}
procedure TrulyRandomSequence(Len: integer; out Bytes: TByteDynArray);
var
  F: TFileStream;
begin
  // we read bytes from /dev/urandom
  F := TFileStream.Create('/dev/urandom', fmOpenRead);
  try
    SetLength(Bytes, Len);
    F.ReadBuffer(Bytes[0], Len);
  finally
    FreeAndNil(F);
  end;
end;
{$EndIf}

{$IfDef __WinRandomSeq__}
procedure TrulyRandomSequence(Len: integer; out Bytes: TByteDynArray);
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
    SetLength(Bytes, Len);
    if not CryptGenRandom(Provider, Len, @Bytes[0]) then
      RaiseLastOSError;
  finally
    CryptReleaseContext(Provider, 0);
  end;
end;
{$EndIf}

{$IfDef __SimpleRandomSeq__}
procedure TrulyRandomSequence(Len: integer; out Bytes: TByteDynArray);
var
  I: integer;
begin
  // have nothing better than generating bytes with standard pascal's random()
  SetLength(Bytes, Len);
  for I := 0 to Len - 1 do
    Bytes[I] := Random(256);
end;
{$EndIf}

initialization
  {$IfDef __SimpleRandomSeq}
  Randomize;
  {$EndIf}

end.
