//*******************************************************//
// uRealPoints introduces operations over TRealPoint as   //
// over 2-dimentional vectors                           //
//*******************************************************//
unit uRealPoints;
{$mode objfpc}{$H+}
interface

uses uTypes;

//Comparison of TRealPoint using epsilon; epsilon for X and for Y are defined separately.
//If epsilon is -1, default value as defined by SetEpsilon will be used. If SetEpsilon was not used,
// it is MachEp
function SameValue(P1,P2:TRealPoint; epsilonX:float = -1; epsilonY:float = -1):boolean; overload; inline;

//  constructor of TRealPoint from two floats
function rpPoint(AX, AY:float):TRealPoint;

//  summation of TRealPoint
function rpSum(P1,P2:TRealPoint):TRealPoint;

//  subtraction of TRealPoint
function rpSubtr(P1, P2:TRealPoint):TRealPoint;

//  multiplication of TRealPoint by Scalar
function rpMul(P:TRealPoint; S:Float):TRealPoint;

//  dot product of TRealPoint
function rpDot(P1, P2:TRealPoint):Float;

//  length of vector, represented by TRealPoint
function rpLength(P:TRealPoint):Float;

//  distance between two TRealPoint on cartesian plane
function Distance(P1, P2:TRealPoint):Float;

operator + (P1, P2:TRealPoint) R:TRealPoint; inline;

operator - (P1,P2:TRealPoint) R:TRealPoint; inline;

operator * (P:TRealPoint;S:Float) R:TRealPoint; inline;

operator * (S:Float; P:TRealPoint) R:TRealPoint; inline;

// dot product
operator * (P1, P2:TRealPoint) R:Float; inline;

implementation

function SameValue(P1, P2: TRealPoint; epsilonX:float = -1; epsilonY:float = -1): boolean;
begin
  Result := SameValue(P1.X,P2.X,epsilonX) and SameValue(P1.Y,P2.Y,epsilonY);
end;

function rpPoint(AX, AY: float): TRealPoint;
begin
  Result.X := AX; Result.Y := AY;
end;

function rpSum(P1, P2: TRealPoint): TRealPoint;
begin
  result.X := P1.X + P2.X;
  result.Y := P1.Y + P2.Y;
end;

function rpSubtr(P1, P2: TRealPoint): TRealPoint;
begin
  result.X := P1.X - P2.X;
  result.Y := P1.Y - P2.Y;
end;

function rpMul(P: TRealPoint; S: Float): TRealPoint;
begin
  result.X := P.X*S;
  result.Y := P.Y*S;
end;

function rpDot(P1, P2: TRealPoint): Float;
begin
  result := P1.X*P2.X + P1.Y*P2.Y;
end;

function rpLength(P: TRealPoint): Float;
begin
  result := Sqrt(P*P);
end;

function Distance(P1, P2: TRealPoint): Float;
begin
  result := rpLength(P1-P2);
end;

operator + (P1, P2: TRealPoint)R: TRealPoint;
begin
  R := rpSum(P1,P2);
end;

operator - (P1, P2: TRealPoint)R: TRealPoint;
begin
  R := rpSubtr(P1,P2);
end;

operator * (P: TRealPoint; S: Float)R: TRealPoint;
begin
  R := rpMul(P,S);
end;

operator * (S: Float; P: TRealPoint)R: TRealPoint;
begin
  R := rpMul(P,S);
end;

operator * (P1, P2: TRealPoint)R: Float;
begin
  R := rpDot(P1,P2);
end;

end.

