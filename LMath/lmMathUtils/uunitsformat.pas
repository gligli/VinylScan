{This unit formats a value with exponent prefixes (milli, pico etc) such that value in output is in the range 1..1000
and adds provided string at the end. For example, FormatUnits(1.2E-12,S) will return "1.2 pS"}
unit uunitsformat;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uTypes, uMath;

const
  DefFormat = '####0.000';
  UnitExponents : array[0..12] of Integer = (-18,-15,-12,-9,-6,-3,0,3,6,9,12,15,18);
  UnitFactors : array[0..12] of Float = (1E-18,1E-15,1E-12,1E-9,1E-6,1E-3,1,1E3,1E6,1E9,1E12,1E15,1E18);
  UnitPrefix  : array[0..12] of string = ('a','f','p','n','μ','m','','K','M','G','T','P','E');
  UnitPrefixLong : array[0..12] of String =
                 ('atto','femto','pico','nano','micro','milli','','Kilo','Mega','Giga','Tera','Peta','Exa');

  //Formats a value Val and SI units name UnitStr with SI decimal prefix such that
  // numeric value in the output string is in [-999..999] range and corresponding prefix is used.
  //E.g.: FormatUnits(12000, "Hz") returns "1.2 kHz"
  function FormatUnits(Val:float; UnitsStr:string; long:boolean=false):string;

  function FindPrefixForExponent(E:integer; long:boolean=false):string;

implementation
const
  FmtStr = '%.5G %s%s';

function FindPrefixForExponent(E:integer; long:boolean=false):string;
  var
    I:integer;
  begin
    for I := 0 to High(UnitExponents) do
      if UnitExponents[I] = E then
      begin
        if long then
          Result := UnitPrefixLong[I]
        else
          Result := UnitPrefix[I];
        Exit;
      end;
    Result := 'Too high or low value';
  end;

function FormatUnits(Val:float; UnitsStr:string; long:boolean=false):string;
var
  Mant:Extended;
  Expo, ED:integer;
  D:integer;
  Prefix : string;
begin
  if IsZero(Val,1E-32) then
  begin
    Mant := 0;
    Prefix := '';
    Result := Format(FmtStr,[Mant,Prefix,UnitsStr]);
    Exit;
  end;
  Expo := Trunc(log10(abs(Val)));
  Mant := Val / intpower(10,Expo); //mantissa, 1<Mant<10
  if SameValue(Mant,10) then
  begin
    Mant := 1;
    inc(Expo)
  end else
  if SameValue(Mant,0.1) then
  begin
    Mant := 1;
    Dec(Expo);
  end;
  if Expo < -18 then
  begin
    D := Expo + 18;
    Mant := Mant * IntPower(10,D);
    Expo := -18;
  end else
  if Expo > 18 then
  begin
    D := Expo - 18;
    Mant := Mant * IntPower(10,D);
    Expo := 18;
  end else
  begin
    ED := Expo mod 3;
    if Expo >= 0 then
    begin
      if ED <> 0 then
      begin
        Expo := Expo - ED;
        Mant := Mant * intpower(10, ED);
      end;
    end else
    begin
      if ED <> 0 then
      begin
        Expo := Expo - 3 - ED;
        Mant := Mant / intpower(10,-3-ED);
      end;
    end;
  end;
  Prefix := FindPrefixForExponent(Expo,long);
  Result := Format(FmtStr,[Mant,Prefix,UnitsStr]);
end;

end.

