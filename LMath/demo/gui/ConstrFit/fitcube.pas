unit FitCube;

{$mode objfpc}{$H+}

interface

uses
  uTypes, lmPointsVec, uIntervals, uMath, uRound, uRandom, globals;

function RFunc(X:float; B:TVector):float;
procedure Constr(MaxCon: integer; B, Con : TVector);
procedure InitFit(out Variables:TVector);
procedure ReadDataPoints(var DataPoints:TPoints);

implementation

const
  interval : TInterval = (Lo: -7; Hi: 7);
  Step = 0.1;
  a = 0.1;
  ex = 3;
  fm = 2.5;

procedure ReadDataPoints(var DataPoints:TPoints);
var
  PX: float;
  Pt:TRealPoint;
begin
  SetRNG(RNG_MT);
  PX := interval.Lo;
  DataPoints := TPoints.Create(Ceil(interval.Length/step+1));
  while PX <= interval.Hi do
  begin
    Pt.x := PX;
    Pt.Y := a*IntPower(PX,ex) + fm + RanGen3 - 0.5;
    DataPoints.Append(Pt);
    PX := PX + Step;
  end;
end;

function RFunc(X: float; B: TVector): float;
begin
  Result := B[1]*IntPower(X,ex) + B[2];
end;

procedure Constr(MaxCon: integer; B, Con: TVector);
begin
  Con[1] := 0.5 - B[1]
end;

procedure InitFit(out Variables: TVector);
begin
  DimVector(Variables,2);
  Variables[1] := 0.4; Variables[2] := 3.1;
end;

end.

