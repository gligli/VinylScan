unit ugaussf;
{ Model of experimental data with a sum of gaussians where interval between mathematical expectancies is always
the same. Such data can occur in patch-clamp experiments.}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, utypes, uMath, unlFit, uGauss;

{X is independent variable; Params - vector of parameters:
  Params[1] - sigma,
  Params[2]..Params[NumberOfGaussians+1] - ScF (scaling factors)
  Params[NumberOfGaussians+2] - mu0
  Params[NumberOfGaussians+3] - Delta
 This function is used as RegFunc for fitting of sum of gaussian.}
function SumGaussiansF(X:Float; Params:TVector):float;

{X is independent variable; Params - vector of parameters:
  Params[1] - sigma0, Params[2] - sigma,
  Params[3]..Params[NumberOfGaussians+2] are ScF (scaling factors)
  Params[NumberOfGaussians+3] - mu0
  Params[NumberOfGaussians+4] - delta }
function SumGaussiansFS0(X:Float; Params:TVector):float; // additional sigma0; fixed Delta between maxima

function DerivGaussForDelta(X:float; Params:TVector; HaveS0:boolean):float;

function DerivGaussForMu0(X:float; Params:TVector):float;

function DerivGaussS0ForMu0(X:float; Params:TVector):float;

{ Calculates partial derivatives for all fitted params.
  Derivs[1] - with resp. to sigma
  Derivs[2]..Derivs[NumberOfGaussians+1] with respect to ScF
  Derivs[NumberOfGaussians+2] - to Mu0
  Derivs[NumberOfGaussians+3] - to Delta}
procedure DerivGaussiansF(X, Y: Float; Params, Derivs:TVector);

{ Calculates partial derivatives for all fitted params, separate sigma for Mu0
  Derivs[1] - with resp. to sigma0; Derivs[2] - to Sigma
  Derivs[3]..Derivs[NumberOfGaussians+2] with respect to ScF
  Derivs[NumberOfGaussians+3] - to Mu0
  Derivs[NumberOfGaussians+4] - to Delta}
procedure DerivGaussiansFS0(X, Y: Float; Params, Derivs:TVector);

procedure SetGaussFitF(ANumberOfGaussians:integer; AUseSigma0:boolean);

procedure DeltaFitGaussians(var ASigma, ASigma0, ADelta, AMu0: Float;
                       var ScFs: TVector; const AXV, AYV: TVector; Observ: integer);

implementation
var
  UseSigma0:boolean;
  NumParams:integer;

function SumGaussiansF(X: Float; Params: TVector): float;
var
  mu,mu0,sigma,Scf,delta:float;
  I:integer;
begin
  sigma := Params[1]; mu0 := Params[NumberOfGaussians+2];
  delta := Params[NumberOfGaussians+3];
  Result := 0;
  for I := 0 to NumberOfGaussians-1 do
  begin
    mu := mu0 + Delta * I;
    Scf := Params[I+2];
    Result := Result + ScaledGaussian(mu, sigma, ScF, X);
  end;
end;

function SumGaussiansFS0(X: Float; Params: TVector): float;
var
  mu,mu0,sigma,Scf,delta:float;
  I:integer;
begin
  mu0 := Params[NumberOfGaussians+3];
  delta := Params[NumberOfGaussians+4];
  mu := mu0; sigma := Params[1]; Scf := Params[3];
  Result := ScaledGaussian(mu,sigma,Scf,X);
  sigma := Params[2];
  for I := 1 to NumberOfGaussians-1 do
  begin
    mu := mu0 + Delta * I;
    Scf := Params[I+3];
    Result := Result + ScaledGaussian(mu, sigma, ScF, X);
  end;
end;

function DerivGaussForDelta(X:float; Params:TVector; HaveS0:boolean):float;
var
  I,B:integer;
  Sigma,Delta,ScF,Summa,Mu0,Mu:float;
begin
  if HaveS0 then
    B := 1
  else
    B := 0;
  Sigma := Params[1+B];
  Delta := Params[NumberOfGaussians+3+B];
  Mu0 := Params[NumberOfGaussians+2+B];
  Summa := 0;
  for I := 1 to NumberOfGaussians - 1 do
  begin
    Mu := Mu0+Delta*I;
    ScF := Params[2+B+I];
    Summa := Summa - ExpSig(Mu,sigma,X)*ScF*(Mu-X)*I;
  end;
  Result := Summa/IntPower(Sigma,3);
end;

function DerivGaussForMu0(X: float; Params: TVector): float;
var
  I:integer;
  Summa,Sigma,Mu,Mu0,Delta,ScF:float;
begin
  Sigma := Params[1];
  Mu0 := Params[NumberOfGaussians+2];
  Delta := Params[NumberOfGaussians+3];
  Summa := 0;
  for I := 0 to NumberOfGaussians-1 do
  begin
    Mu := Mu0+Delta*I;
    ScF := Params[2+I];
    Summa := Summa+ExpSig(Mu,sigma,X)*ScF*(X-Mu);
  end;
  Result := Summa/IntPower(Sigma,3);
end;

function DerivGaussS0ForMu0(X: float; Params: TVector): float;
var
  I:integer;
  Summa,Sigma,Mu,Mu0,Delta,ScF:float;
begin
  Sigma := Params[1];
  Mu0 := Params[NumberOfGaussians+3];
  Delta := Params[NumberOfGaussians+4];
  ScF := Params[3];
  Result := ExpSig(Mu0,Sigma,X)*ScF*(X-Mu0)/IntPower(Sigma,3);
  Summa := 0;
  Sigma := Params[2];
  for I := 1 to NumberOfGaussians-1 do
  begin
    Mu := Mu0+Delta*I;
    ScF := Params[3+I];
    Summa := Summa+ExpSig(Mu,sigma,X)*ScF*(X-Mu);
  end;
  Result := Result + Summa/IntPower(Sigma,3);
end;

procedure DerivGaussiansF(X, Y: Float; Params, Derivs: TVector);
var
  I:integer;
  mu, mu0, sigma, ScF, dSigma, Delta :float;
begin
  dsigma := 0;
  sigma := Params[1];
  mu0 := Params[NumberOfGaussians+2];
  Delta := Params[NumberOfGaussians+3];
  for I := 0 to NumberOfGaussians-1 do
  begin
    ScF := Params[I+2];
    mu := mu0+Delta*I;
    dsigma := dsigma+DerivGaussForSigma(mu,sigma,ScF,X);
    Derivs[I+2] := DerivGaussForScaler(mu, sigma, X); // with respect to scaling factor
  end;
  Derivs[1] := dSigma;
  Derivs[NumberOfGaussians+2] := DerivGaussForMu0(X,Params);
  Derivs[NumberOfGaussians+3] := DerivGaussForDelta(X,Params,false);
end;
{ Calculates partial derivatives for all fitted params, separate sigma for Mu0
  Derivs[1] - with resp. to sigma0; Derivs[2] - to Sigma
  Derivs[3]..Derivs[NumberOfGaussians+2] with respect to ScF
  Derivs[NumberOfGaussians+3] - to Mu0
  Derivs[NumberOfGaussians+4] - to Delta
}
procedure DerivGaussiansFS0(X, Y: Float; Params, Derivs: TVector);
var
  I:integer;
  mu, mu0, sigma, ScF, dSigma, Delta :float;
begin
  mu0 := Params[NumberOfGaussians+3];
  Delta := Params[NumberOfGaussians+4];
  sigma := Params[1];
  Scf := Params[3];
  Derivs[1] := DerivGaussForSigma(mu0,sigma,Scf,X);
  Derivs[3] := DerivGaussForScaler(mu0,sigma,X);
  dsigma := 0;
  sigma := Params[2];
  for I := 1 to NumberOfGaussians-1 do
  begin
    ScF := Params[I+3];
    mu := mu0+Delta*I;
    dsigma := dsigma+DerivGaussForSigma(mu,sigma,ScF,X);
    Derivs[I+3] := DerivGaussForScaler(mu, sigma, X); // with respect to scaling factor
  end;
  Derivs[2] := dSigma;
  Derivs[NumberOfGaussians+3] := DerivGaussS0ForMu0(X,Params);
  Derivs[NumberOfGaussians+4] := DerivGaussForDelta(X,Params,true);
end;

procedure SetGaussFitF(ANumberOfGaussians:integer; AUseSigma0:boolean);
begin
  NumberOfGaussians := ANumberOfGaussians;
  UseSigma0 := AUseSigma0;
  NumParams := NumberOfGaussians + 3;
  if UseSigma0 then
    Inc(NumParams);
  SetMaxParam(NumParams);
  DimVector(TheParams,NumParams);
end;

procedure DeltaFitGaussians(var ASigma, ASigma0, ADelta, AMu0: Float;
                       var ScFs: TVector; const AXV, AYV: TVector; Observ: integer);
var
  I,L:integer;
  MyTol:float;
begin
  TheParams[1] := ASigma;
  if UseSigma0 then
  begin
    TheParams[2] := ASigma;
    L := 3;
  end else
    L := 2;
  for I := L to NumberOfGaussians+L-1 do
    TheParams[I] := ScFs[I-L];
  TheParams[NumberOfGaussians+L] := AMu0;
  TheParams[NumberOfGaussians+L+1] := ADelta;
  DimMatrix(Hess,NumberOfGaussians+4, NumberOfGaussians+4);
  SetParamBounds(1,ASigma/4,ASigma*4);
  if UseSigma0 then
    SetParamBounds(2,ASigma/4,ASigma*4);
  for I := L to NumberOfGaussians+L-1 do
    SetParamBounds(I,0,1); //LevelProbabilities
  SetParamBounds(L+NumberOfGaussians+1,ADelta/2,ADelta*2);
  SetOptAlgo(NL_Marq);
  MyTol := abs(AXV[Observ]-AXV[1])/Observ/10;
  if UseSigma0 then
  begin
    NLFit(@SumGaussiansFS0,@DerivGaussiansFS0, AXV, AYV, 1, Observ, 10000, MyTol, TheParams, 1, NumParams, HESS);
    ASigma0 := TheParams[1];
    ASigma := TheParams[2];
  end
  else begin
    NLFit(@SumGaussiansF, @DerivGaussiansF, AXV, AYV, 1, Observ, 10000, MyTol, TheParams, 1, NumParams, HESS);
    ASigma := TheParams[1];
    ASigma0 := TheParams[1];
  end;
  for I := 0 to NumberOfGaussians - 1 do
    ScFs[I] := TheParams[I+L];
  AMu0 := TheParams[NumberOfGaussians+L];
  ADelta := TheParams[NumberOfGaussians+L+1];
  Finalize(TheParams);
  Finalize(Hess);
end;

end.

