{ ******************************************************************
  Test of UVAG random number generator

  By Alex Hay (zenjew@hotmail.com) - Adapted to TPMath by Jean Debord

  This program prints 1000 random integers, computed with the default
  initialization. The results should be identical to file uvag.txt
  ****************************************************************** }

program testuvag;

uses
 {$IFDEF USE_DLL}
  dmath;
{$ELSE}
  utypes, urandom;
{$ENDIF}

var
  I : Word;
  R : Integer;

begin
  SetRNG(RNG_UVAG);

  for I := 1 to 1000 do
    begin
      R := IRanGen;
      Write(R:15);
      if I mod 5 = 0 then Writeln;
    end;
  readln;
end.
