{This unit defines several distributions and instruments to model experimental data with these distributions.
Defined are binomial, exponential, hypoexponential and hyperexponential distributions.
}
unit udistribs;

{$mode objfpc}{$H+}

interface

uses
  UTypes, uErrors, uMath, unlFit, uMinMax, uMeanSD;

type
  TBinomialDistribFunction = function(k, n:integer; q:Float):Float;

  // returns binomial probability density for value k in test with n trials and
  // q probability of success in one trial. If k>n returns 0.
  function dBinom(k,n:integer;q:Float):Float;

  function ExponentialDistribution(beta, X:float):float;

  //N defines number of phases; Params:zero-based TVector[2*N] contains pairs of parameters for each phase:
  //probability and time (not rate!) constant. Sum of all probabilities must be 1.
  function HyperExponentialDistribution(N:integer; var Params:TVector; X:float):float;

  //estimates parameters of hyperexponential distribution with 2 phases using Marqardt algorithm
  procedure Fit2HyperExponents(var Xs, Ys:TVector; Ub:integer; out P1, beta1, beta2:float);

  //PDF of the hypoexponential distribution with 2 phases; beta1, beta2 are time constants (not rate constants!)
  function HypoExponentialDistribution2(beta1, beta2, X:float):float;

  // Analytic estimate of the parameters of hypoexponential distribution
  // from known mean and variation coefficients
  procedure EstimateHypoExponentialDistribution(M,CV:float; out beta1, beta2:float);

  // Iterative fit of hypoexponential distribution.
  procedure Fit2Hypoexponents(var Xs, Ys: TVector; Ub:integer; out beta1, beta2:float);

implementation
{dBinom = (n!/(k!*(n-k)!) * q^k*(1-q)^(n-k)    }
function dBinom(k,n:integer;q:Float):Float;
var
   a : integer;
   r : extended;
   i,j:integer;
begin
   if (k > n) or (k < 0) then
   begin
     Result := 0;
     exit;
   end;
   if (q < 0) or (q > 1) then
   begin
     Result := DefaultVal(FDomain,0,'DBinom: Probability out of [0;1] range.');
     Exit;
   end;
   if k < (n div 2) then  // Here we find max((n-k),k)
     a := n-k  // and assign it to "a" thus minimising value of intermideate result
   else
     a := k;
   r := 1;
   j := 1;
   for i := a + 1 to n do  // Then in the cycle we find n!/(max((n-k),k)!
   begin                   // by multipliing a*(a+1)*...*n
     r := r * i;
     r := r / j;           // and simultaneously dividing by min((n-k),k)!
     inc(j);
   end;
   Result := r * intpower(q,k) * intpower((1-q),(n-k));
end;

function ExponentialDistribution(beta, X:float):float;
begin
  if (X >= 0) and IsPositive(beta) then
    Result := (1/beta)*Expo(-X/beta)
  else
    Result := 0;
end;

function HyperExponentialDistribution(N:integer; var Params:TVector; X:float):float;
var
  I:integer;
  S:float;
begin
  S := 0;
  for I := 0 to N-1 do
    S := S + Params[I*N];
  if not SameValue(1,S,0.01) then
  begin
    Result := DefaultVal(FDomain,0,'HyperExp. fit: sum of probabilities is not 1.');
    Exit;
  end;
  S := 0;
  for I := 0 to N-1 do
    S := S + Params[I*N]*ExponentialDistribution(Params[I*N+1],X);
  Result := S;
end;

function DerivHyperExpForP(X:float; Params:TVector):float;
begin
  Result := exp(-X/Params[2])/Params[2] - exp(-X/Params[3])/Params[3];
end;

function DerivHyperExpForBeta1(X:float; Params:TVector):float;
begin
  Result := exp(-X/Params[2])*Params[1]*(X-Params[2])/(Params[2]*Params[2]*Params[2]);
end;

function DerivHyperExpForBeta2(X:float; Params:TVector):float;
begin
  Result := exp(-X/Params[3])*(1-Params[1])*(X-Params[3])/(Params[3]*Params[3]*Params[3]);
end;

function HyperExp2(X:float; Params:TVector):float;
begin
  Result := Params[1]/Params[2]*exp(-X/Params[2]) + (1-Params[1])/Params[3]*exp(-X/Params[3]);
end;

procedure HyperExpDerivs(X,Y:float; Params, Derivs:TVector);
begin
  Derivs[1] := DerivHyperExpForP(X,Params);
  Derivs[2] := DerivHyperExpForBeta1(X,Params);
  Derivs[3] := DerivHyperExpForBeta2(X,Params);
end;

procedure Fit2HyperExponents(var Xs, Ys:TVector; Ub:integer; out P1, beta1, beta2:float);
var
  MyTol:float;
  ExpParams:TVector;
  HESS:TMatrix;
begin
  DimVector(ExpParams,3);
  DimMatrix(Hess,3,3);
  SetParamBounds(1,0,1);
  SetParamBounds(2,MinNum*100,MaxNum/10);
  SetParamBounds(3,MinNum*100,MaxNum/10);
  ExpParams[1] := 0.6;
  ExpParams[2] := mean(Xs,1,Ub);
  ExpParams[3] := ExpParams[2]/0.7;
  SetOptAlgo(NL_Marq);
  MyTol := 0.001;
  NLFit(@HyperExp2, @HyperExpDerivs, Xs, Ys, 1, Ub, 10000, MyTol, ExpParams, 1, 3, HESS);
  P1 := ExpParams[1]; beta1 := ExpParams[2]; beta2 := ExpParams[3];
  Finalize(ExpParams);
  Finalize(Hess);
end;

function HypoExponentialDistribution2(beta1, beta2, X:float): float;
begin
  if (beta1 <> beta2) and not IsZero(beta1) and not IsZero(beta2) then
    Result := (exp(-x/beta1) - exp(-x/beta2)) / (beta1-beta2)
  else
    Result := 0;
end;

procedure EstimateHypoExponentialDistribution(M, CV: float; out beta1, beta2: float);
var
  CSq:float;
begin
  SetErrCode(matOK);
  CSq := CV*CV;
  if (CV > 0.999) then
    SetErrCode(FDomain,'HypoExponential distribution: CV too high')
  else if (CSq < 0.51) then
    SetErrCode(FDomain,'Hypoexponential distribution: CV too low');
  if MathErr <> matOK then
  begin
    beta1 := 0; beta2 := 0;
  end else
  begin
    CSq := Sqrt(1+2*(CSq-1));
    beta1 := M/2*(1+CSq);
    beta2 := M/2*(1-CSq);
    SetErrCode(matOK);
  end;
end;

function HypoExpG(X:float; Params:TVector):float;
begin
  Result := (exp(-X/Params[1]) - exp(-x/(Params[1]-Params[2])))/Params[2];
end;

function DerivHypoExpGForBeta(X:float; Params:TVector):float;
var
  beta, g : float;
begin
  beta := Params[1]; g := Params[2];
  Result := (x*exp(-X/beta)/sqr(beta) - x*exp(-x/(beta - g))/sqr(beta - g)) / g;
end;

function DerivHypoExpGForG(X:float; Params:TVector):float;
var
  beta, g : float;
begin
  beta := Params[1]; g := Params[2];
  Result := x*exp(-x/(beta-g))/sqr(beta-g)/g-(exp(-x/beta) - exp(-x/(beta - g)))/sqr(g);
end;

procedure HypoExpDerivs(X,Y:float; Params, Derivs:TVector);
begin
  Derivs[1] := DerivHypoExpGForBeta(X,Params);
  Derivs[2] := DerivHypoExpGForG(X,Params);
end;

procedure Fit2HypoExponents(var Xs, Ys:TVector; Ub:integer; out beta1, beta2:float);
var
  MyTol:float;
  ExpParams:TVector; //beta1, g (diff beta1-beta2)
  HESS:TMatrix;
  I,IndMax:integer;
  PosMax,MaxVal:float;
begin
  DimVector(ExpParams,2);
  DimMatrix(Hess,2,2);
  MaxVal := 0;
  IndMax := 1;
  for I := 1 to Ub do
  begin
    if Ys[I] > MaxVal then
    begin
      MaxVal := Ys[I];
      IndMax := I;
    end;
  end;
  PosMax := Xs[IndMax];
  SetParamBounds(1,PosMax/1000,PosMax*1000);
  SetParamBounds(2,PosMax/1000,PosMax*0.999);
  ExpParams[1] := PosMax;
  ExpParams[2] := PosMax/2;
  SetOptAlgo(NL_Simp);
  MyTol := 0.001;
  NLFit(@HypoExpG, @HypoExpDerivs, Xs, Ys, 1, Ub, 10000, MyTol, ExpParams, 1, 2, HESS);
  beta1 := ExpParams[1]; beta2 := ExpParams[1]-ExpParams[2];
end;

end.
