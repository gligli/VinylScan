{This unit fits experimental data with a sum of gaussian distributions where mathematical expectances are different,
they are scaled by different factors, but have the same sigma, possibly with the exception of first one.
Such distributions occur in patch-clamp experiments (distribution of current values over time in a recording with
several active channels) and in chromatography (several peaks).
}
{$mode objfpc}
unit ugauss;
interface

uses
  Classes, SysUtils, utypes, uMath, uNormal, uMeanSD, uVecUtils, unlFit, uSigmaTable;

type
  ENoSigma = class(Exception)
  end;

  // Quick and rough estimate of sigma for normal distribution, if mu is known
  // and upper part of the empiric probability density curve is known.
  //uses SigmaArray
function FindSigma(var XArray, YArray:TVector; TheLength, MuPos :integer):Float;

// InvSqrt2Pi*exp((-(mu-x)*(mu-x))/(2*sigma*sigma));
function expsig(mu, sigma, X:float):float;

//value of gaussian, scaled by factor ScF
function ScaledGaussian(mu, sigma, ScF, X:float):float;

//derivative of scaled gaussian with respect to X
function DerivGaussForX(mu, sigma, ScF, X: float):float;

//derivative of scaled gaussian with respect to sigma
function DerivGaussForSigma(mu, sigma, ScF, X: float):float;

//derivative of scaled gaussian with respect to scaling factor
function DerivGaussForScaler(mu, sigma, X:float):float;

//derivative of scaled gaussian with respect to mean
function DerivGaussForMean(mu, sigma, ScF, X:float):float;

{X is independent variable; Params - vector of parameters:
  Params[1] - sigma,
  Params[2]..Params[NumberOfGaussians+1] - ScF (scaling factors)
  Params[NumberOfGaussians+2..2*NumberOfGaussians+2] - mu
 This function is used as RegFunc for fitting of sum of gaussian.}
function SumGaussians(X:Float; Params:TVector):float;

{ Sigma0 may be different from Sigma for all others and is fitted separately
X is independent variable; Params - vector of parameters:
  Params[1] - sigma0, Params[2] - sigma,
  Params[3]..Params[NumberOfGaussians+2] are ScF (scaling factors)
  Params[NumberOfGaussians+3]..[2*NumberOfGaussians+2] are mu }
function SumGaussiansS0(X:Float; Params:TVector):float;

{Params is vector of parameters, Dervis - vector of partial derivatives in X,Y point}
{used as DerivProc}
procedure DerivGaussians(X, Y: Float; Params, Derivs:TVector);

{Params is vector of parameters, Dervis - vector of partial derivatives in X,Y point}
{used as DerivProc}
procedure DerivGaussiansS0(X, Y: Float; Params, Derivs:TVector);

{Set model parameters:
ANumberOfGaussians: How many gaussians form the distribution
AUseSigma0 : if Sigma0 may be different from others
AFitMeans: if means of every gaussian are fitted or they are fixed and only sigmas and scale factors
(which gives probabilities for every gaussian) are fitted}
procedure SetGaussFit(ANumberOfGaussians:integer; AUseSigma0, AFitMeans: boolean);

{Actual fit of the model.
AMathExpect: as input, guess values for means; as output, fitted means;
ASigma, ASigma0: guessed and then found Sigma for all gaussians and, if needed, for first one;
AXV, AYV: experimental data for X and for Y (observed probability distribution density);
Observ: number of observations (High bound of AXV and AYV}
procedure SumGaussFit(var AMathExpect: TVector; var ASigma, ASigma0:Float;
                          var ScFs : TVector; const AXV, AYV:TVector; Observ:integer);
var
  NumberOfGaussians:integer;
  MathExpectances:TVector;
  TheParams:TVector;
  Hess:TMatrix;

implementation
var
  UseSigma0:boolean;
  FitLevels:boolean;
  NumParams:integer;

function FindSigma(var XArray, YArray:TVector; TheLength, MuPos :integer):Float;
var
  Maxi, Half, LeftMin, RightMin, Rat:Float;
  I,J,LeftMinPos, RightMinPos, Step:integer;
begin
  Maxi := max(YArray,0,TheLength); //maximal value which may be not in a supposed position of mu
  LeftMin := Maxi;
  LeftMinPos := MinLoc(YArray,0,MuPos);
  RightMin := Maxi;
  RightMinPos := TheLength - 1;
  for I := TheLength - 1 downto MuPos do
  if YArray[I] < RightMin then
  begin
    RightMin := YArray[I];
    RightMinPos := I;
  end;
  if LeftMin < RightMin then
  begin         //we will use a side, where Maxi - Min difference is maximal
    I := LeftMinPos;
    Step := 1;
  end else
  begin
    I := RightMinPos;
    Step := -1;
  end;
  Rat := YArray[I]/Maxi; //Now we found MinToMax ratio
  J := 59;  //and look which ratio from SigmaArray table we are using. Best is 59 (ratio = 0.4985)
  while (Rat > SigmaArray[J,1]) and (J >= 0) do //but if not available,
    dec(J);  // find least possible
  if J = 0 then Raise ENoSigma.Create('Find Sigma: Too small difference between Max and min in the distribution curve.');
  Half := Maxi*SigmaArray[J,1];
  while YArray[I] < Half do //Initially, I points to MinPos
    I := I + Step;
  if (Half - YArray[I-Step]) < (YArray[I] - Half) then
    Result := SigmaArray[J,0]*abs(XArray[MuPos] - XArray[I-Step])
  else
    Result := SigmaArray[J,0]*abs(XArray[MuPos] - XArray[I]);
end;

function ScaledGaussian(mu, sigma, ScF, X:float):float;
begin
  Result := DGaussian(X, mu, sigma)*ScF;
end;

function expsig(mu, sigma, X:float):float;
begin
  Result := InvSqrt2Pi*exp((-(mu-x)*(mu-x))/(2*sigma*sigma));
end;

function DerivGaussForX(mu, sigma, ScF, X: float): float;
begin
  Result := expsig(mu, sigma, X)*(mu-X)*(mu-X)*ScF/IntPower(Sigma,3);
end;

function DerivGaussForSigma(mu, sigma, ScF, X: float): float;
begin
  Result := expsig(mu,sigma,X)*ScF * (((mu-X)*(mu-X))/intPower(sigma,4) - 1/(sigma*sigma));
end;

function DerivGaussForScaler(mu, sigma, X:float):float;
begin
  Result := expsig(mu, sigma, X)/sigma;
end;

function DerivGaussForMean(mu, sigma, Scf, X:float):float;
begin
  Result := expSig(mu, sigma, X)*Scf*(X-mu)/IntPower(Sigma,3);
end;

function SumGaussians(X:Float; Params:TVector):float;
var
  mu, sigma, ScF:float;
  I:integer;
begin
  Result := 0;
  sigma := Params[1];
  for I := 2 to NumberOfGaussians+1 do
  begin
    ScF := Params[I];
    if FitLevels then
      mu := Params[I+NumberOfGaussians]
    else
      mu := MathExpectances[I-1];
    Result := Result + ScaledGaussian(mu, sigma, ScF, X);
  end;
end;

procedure DerivGaussians(X, Y: Float; Params, Derivs:TVector);
var
  I:integer;
  mu, sigma, ScF, dSigma :float;
begin
  dsigma := 0;
  sigma := Params[1];
  for I := 2 to NumberOfGaussians+1 do
  begin
    ScF := Params[I];
    if FitLevels then
      mu := Params[I+NumberOfGaussians]
    else
      mu := MathExpectances[I-1];
    dsigma := dsigma+DerivGaussForSigma(mu,sigma,ScF,X);
    Derivs[I] := DerivGaussForScaler(mu, sigma, X); // with respect to scaling factor
    if FitLevels then
      Derivs[I+NumberOfGaussians] := DerivGaussForMean(mu,sigma,ScF,X); //with respect to mean for this gaussian
  end;
  Derivs[1] := dSigma;
end;

function SumGaussiansS0(X:Float; Params:TVector):float; //additional sigma0
{X is independent variable; Params - vector of parameters:
  Params[1] - sigma0, Params[2] - sigma,
  Params[3]..Params[NumberOfGaussians+2] are ScF (scaling factors)
  Params[NumberOfGaussians+3]..[2*NumberOfGaussians+2] are mu }
var
  mu, sigma, ScF:float;
  I:integer;
begin
  sigma := Params[1];
  ScF := Params[3];
  if FitLevels then
    mu := Params[NumberOfGaussians+3]
  else
    mu := MathExpectances[1];
  Result := ScaledGaussian(mu, sigma, ScF, X);
  sigma := Params[2];
  for I := 4 to NumberOfGaussians+2 do
  begin
    ScF := Params[I];
    if FitLevels then
      mu := Params[I+NumberOfGaussians]
    else
      mu := MathExpectances[I-2];
    Result := Result + ScaledGaussian(mu, sigma, ScF, X);
  end;
end;

procedure DerivGaussiansS0(X, Y: Float; Params, Derivs:TVector);
var
  I:integer;
  mu, sigma, ScF, dSigma :float;
begin
  sigma := Params[1];
  ScF := Params[3];
  mu := Params[NumberOfGaussians+3];
  derivs[1] := DerivGaussForSigma(mu,sigma,ScF,X);
  Derivs[3] := DerivGaussForScaler(mu, sigma, X); // with respect to scaling factor
  if FitLevels then
    Derivs[NumberOfGaussians+3] := DerivGaussForMean(mu,sigma,ScF,X); //with respect to mean for this gaussian
  sigma := Params[2];
  dsigma := 0;
  for I := 4 to NumberOfGaussians+2 do
  begin
    ScF := Params[I];
    mu := Params[I+NumberOfGaussians];
    dsigma := dsigma+DerivGaussForSigma(mu,sigma,ScF,X);
    Derivs[I] := DerivGaussForScaler(mu, sigma, X); // with respect to scaling factor
    if FitLevels then
      Derivs[I+NumberOfGaussians] := DerivGaussForMean(mu,sigma,ScF,X); //with respect to mean for this gaussian
  end;
  Derivs[2] := dSigma;
end;

procedure SetGaussFit(ANumberOfGaussians:integer; AUseSigma0, AFitMeans: boolean);
begin
  FitLevels := AFitMeans;
  NumberOfGaussians := ANumberOfGaussians;
  if NumberOfGaussians > 1 then
    UseSigma0 := AUseSigma0
  else
    UseSigma0 := false;
  if FitLevels then
    NumParams := 2*NumberOfGaussians + 1
  else
    NumParams := NumberOfGaussians + 1;
  if UseSigma0 then
    Inc(NumParams);
  SetMaxParam(NumParams);
  DimVector(TheParams,(2*NumberOfGaussians)+2); // and not NumParams: we use means anyway, even if do not fit them!
end;

procedure SumGaussFit(var AMathExpect: TVector; var ASigma, ASigma0:Float;
                          var ScFs : TVector; const AXV, AYV:TVector; Observ:integer);
var
  I,L:integer;
  MyTol:float;
begin
  if not FitLevels then
  begin
    DimVector(MathExpectances, NumberOfGaussians);
    for I := 1 to NumberOfGaussians do
      MathExpectances[I] := AMathExpect[I-1]; // AMathExpect counts from 0; MathExpectances from 1
  end;
  TheParams[1] := ASigma;
  if UseSigma0 then
  begin
    TheParams[2] := ASigma;
    L := 3;
  end else
    L := 2;
  for I := L to NumberOfGaussians+L-1 do
  begin
    TheParams[I] := ScFs[I-L];
    TheParams[I+NumberOfGaussians] := AMathExpect[I-L];
  end;
  DimMatrix(Hess,2*NumberOfGaussians+3, 2*NumberOfGaussians+3);
  SetParamBounds(1,ASigma/4,ASigma*4);
  if UseSigma0 then
    SetParamBounds(2,ASigma/4,ASigma*4);
  for I := L to NumberOfGaussians+L-1 do
  begin
    SetParamBounds(I,0,1); //LevelProbabilities
    if FitLevels then
      SetParamBounds(I+NumberOfGaussians,AXV[1],AXV[Observ]); //Position of Level must be inside data
  end;
  SetOptAlgo(NL_Marq);
  MyTol := abs(AXV[Observ]-AXV[1])/Observ/10;
  if UseSigma0 then
  begin
    NLFit(@SumGaussiansS0,@DerivGaussiansS0, AXV, AYV, 1, Observ, 10000, MyTol, TheParams, 1, NumParams, HESS);
    ASigma0 := TheParams[1];
    ASigma := TheParams[2];
  end
  else begin
    NLFit(@SumGaussians, @DerivGaussians, AXV, AYV, 1, Observ, 10000, MyTol, TheParams, 1, NumParams, HESS);
    ASigma := TheParams[1];
    ASigma0 := TheParams[1];
  end;
  for I := 0 to NumberOfGaussians - 1 do
  begin
    ScFs[I] := TheParams[I+L];
    if FitLevels then
      AMathExpect[I] := TheParams[I+NumberOfGaussians+L];
  end;
  if not FitLevels then
    Finalize(MathExpectances);
  Finalize(TheParams);
  Finalize(Hess);
end;

end.

