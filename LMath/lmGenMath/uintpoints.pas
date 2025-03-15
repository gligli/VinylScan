unit uIntPoints;
//*******************************************************//
// uIntPoints introduces operations over TIntegerPoint   //
//*******************************************************//
{$mode objfpc}{$H+}
interface

uses uTypes;

//  constructor of TIntegerPoint from two integers
function ipPoint(AX, AY:integer):TIntegerPoint;

//  summation of TIntegerPoint
function ipSum(P1,P2:TIntegerPoint):TIntegerPoint;

//  subtraction of TIntegerPoint
function ipSubtr(P1, P2:TIntegerPoint):TIntegerPoint;

//  multiplication of TIntegerPoint by Integer
function ipMul(P:TIntegerPoint; S:integer):TIntegerPoint;

operator + (P1, P2:TIntegerPoint) R:TIntegerPoint; inline;

operator - (P1,P2:TIntegerPoint) R:TIntegerPoint; inline;

operator * (P:TIntegerPoint;S:integer) R:TIntegerPoint; inline;

operator * (S:integer; P:TIntegerPoint) R:TIntegerPoint; inline;

implementation

function ipPoint(AX, AY: integer): TIntegerPoint;
begin
  Result.X := AX; Result.Y := AY;
end;

function ipSum(P1, P2: TIntegerPoint): TIntegerPoint;
begin
  result.X := P1.X + P2.X;
  result.Y := P1.Y + P2.Y;
end;

function ipSubtr(P1, P2: TIntegerPoint): TIntegerPoint;
begin
  result.X := P1.X - P2.X;
  result.Y := P1.Y - P2.Y;
end;

function ipMul(P: TIntegerPoint; S: integer): TIntegerPoint;
begin
  result.X := P.X*S;
  result.Y := P.Y*S;
end;

operator + (P1, P2: TIntegerPoint)R: TIntegerPoint;
begin
  R := ipSum(P1,P2);
end;

operator - (P1, P2: TIntegerPoint)R: TIntegerPoint;
begin
  R := ipSubtr(P1,P2);
end;

operator * (P: TIntegerPoint; S: integer)R: TIntegerPoint;
begin
  R := ipMul(P,S);
end;

operator * (S: integer; P: TIntegerPoint)R: TIntegerPoint;
begin
  R := ipMul(P,S);
end;

end.
