 //CHEBYSHEV FILTER AND OTHER RECURSIVE FILTERS - RECURSION COEFFICIENT CALCULATION
 
unit uFindFilterCoeffs;
interface
uses uTypes, uErrors, uMath, uTrigo, uMeanSD, uMatrix, uConvolutions, uVectorHelper;

type
  TPT = 0..3;
  TPTArray = array[TPT] of Float; //< used in Gauss filter
  TRecursCoeffs = array[1..2] of Float; //< used by narrowband filters
  TInCoeffs = array[0..2] of Float;  //< used by narrowband filters

procedure FindChebyshevCoeffs(ASamplingRate, ACutFreq: float; AHighPass : boolean; PR: float;
   NPoles: integer; out A,B:TVector);

procedure GSFindParams(ASamplingRate:Float; ACutFreq: Float; out Sigma, BL, Q : Float; out Bs:TPTArray);

procedure FindNarrowBandParams(CentralFreq, BW, SamplingRate : Float; out K, R, CoF : float);

procedure FindBandPassCoeffs(K, R, Cof : Float; out A:TInCoeffs; out B:TRecursCoeffs);

procedure FindNotchCoeffs(K, R, Cof : Float; out A:TInCoeffs; out B:TRecursCoeffs);

implementation
type
  TOne = array[0..2] of Float;

procedure GSFindParams(ASamplingRate:Float; ACutFreq: Float; out Sigma, BL, Q : Float; out Bs:TPTArray);
var
  Q2, Q3 : Float;
begin
  Sigma := ASamplingrate*0.83/(TwoPi*ACutFreq);
  if Sigma >= 2.5 then
    Q := 0.98711 * Sigma - 0.96330
  else
    Q := 3.97156 - 4.14554 * Sqrt(1 - 0.26891 * Sigma);
  Q2 := Q*Q;
  Q3 := Q2*Q;
  Bs[0] := 1.57825 + 2.44413*Q + 1.4281*Q2 + 0.422205*Q3;
  Bs[1] := 2.44413*Q + 2.85619*Q2 + 1.26661*Q3;
  Bs[2] := -1.4281*Q2 - 1.26661*Q3;
  Bs[3] := 0.422205*Q3;
  BL    := 1 - (Bs[1] + Bs[2] + Bs[3])/Bs[0];
end;

procedure FindNarrowBandParams(CentralFreq, BW, SamplingRate : Float; out K, R, CoF : float);
var
  FrRatio, BWRatio: float;
begin
  FrRatio := CentralFreq/SamplingRate;
  BWratio := BW/SamplingRate;
  R := 1-3*BWRatio;
  CoF := 2*cos(TwoPi*FRRatio);
  K := (1 - R*CoF + R*R)/(2 - CoF);
end;

procedure FindBandPassCoeffs(K, R, Cof : Float; out A:TInCoeffs; out B:TRecursCoeffs);
begin
  A[0] := 1 - K;
  A[1] := (K-R)*CoF;
  A[2] := R*R-K;
  B[1] := R*CoF;
  B[2] := -R*R;
end;

procedure FindNotchCoeffs(K, R, Cof : Float; out A:TInCoeffs; out B:TRecursCoeffs);
begin
  A[0] := K;
  A[1] := CoF * (-K);
  A[2] := K;
  B[1] := R*CoF;
  B[2] := -R*R;
end;

procedure ChebyshevParams(FC, PR : float; P, NP : integer; AHighPass: boolean; out AOne:TOne; out BOne:TOne);
var
  RP, IP, ES, VX, KX, T, W, M, D, K, X0, X1, X2, Y1, Y2 : float;
  Tquad : float;
// Calculate the pole location on the unit circle
begin
  RP := -cos(PiDiv2/NP + (P-1)*Pi/NP);
  IP :=  sin(PiDiv2/NP + (P-1)*Pi/NP);
// Warp from a circle to an ellipse
  IF PR <> 0 then 
  begin
    ES := Sqrt(Sqr(100/(100-PR)) - 1);
    VX := 1/NP*Log(1/ES + Sqrt(1/Sqr(ES) + 1));
    KX := 1/NP*Log(1/ES + Sqrt(1/Sqr(ES) - 1));
    KX := (EXP(KX) + EXP(-KX))/2;
    RP := RP * ((EXP(VX) - EXP(-VX)) /2 )/KX;
    IP := IP * ((EXP(VX) + EXP(-VX)) /2 )/KX;
  end;
  //s-domain to z-domain conversion
  T  := 2 * Tan(1/2);
  TQuad := T*T;
  W  := TwoPi*FC;
  M  := RP*RP + IP*IP;
  D  := 4 - 4*RP*T + M*TQuad;
  X0 := TQuad/D;
  X1 := 2*TQuad/D;
  X2 := TQuad/D;
  Y1 := (8 - 2*M*TQuad)/D;
  Y2 := (-4 - 4*RP*T - M*TQuad)/D;

 // LP TO LP, or LP TO HP transform
  IF AHighPass THEN 
    K := -Cos(W/2 + 1/2) / Cos(W/2 - 1/2)
  else
    K :=  Sin(1/2 - W/2) / Sin(1/2 + W/2);
  D  := 1 + Y1*K - Y2*K*K;
  AOne[0] := (X0 - X1*K + X2*K*K)/D;
  AOne[1] := (-2*X0*K + X1 + X1*K*K - 2*X2*K)/D;
  AOne[2] := (X0*K*K- X1*K + X2)/D;
  BOne[0] := 1;
  BOne[1] := (2*K + Y1 + Y1*K*K - 2*Y2*K)/D;
  BOne[2] := (-(K*K) - Y1*K + Y2)/D;
  IF AHighPass then
  begin
    AOne[1] := -AOne[1];
    BOne[1] := -BOne[1];
  end;
end;

// AHighPass: if we need high or low pass filter; PR percent of ripple in step response, 0 to 29
// NPoles must be even
procedure FindChebyshevCoeffs(ASamplingRate, ACutFreq: float; AHighPass : boolean; PR: float; 
   NPoles: integer; out A,B:TVector);
var
  TA, TB : TVector;
  FC: Float;
  I,J:integer;
  SA, SB, Gain: float;
  AOne:TOne;
  BOne:TOne;
  NP2:integer;
begin
  if NPoles Mod 2 <> 0 then
  begin
    A := nil;
    B := nil;
    SetErrCode(lmPolesNumError);
    exit;
  end;
  DimVector(A,NPoles); DimVector(TA,NPoles);
  DimVector(B,NPoles); DimVector(TB,NPoles);
  NP2 := NPoles + 2;
  A[0] := 1.0;
  B[0] := 1.0;

  FC := ACutFreq/ASamplingRate;
  FOR I := 1 to NPoles div 2 do //LOOP FOR EACH POLE-PAIR
  begin
    ChebyshevParams(FC, PR, I, NPoles, AHighPass, AOne, BOne);
    for J := 1 to 2 do
      BOne[J] := -BOne[J];
    TA.FillWithArr(0,A);
    TB.FillWithArr(0,B);
    Convolve(TA[0..(I-1)*2],AOne,A);
    Convolve(TB[0..(I-1)*2],BOne,B);
  end;

  B[0] := 0; //Finish combining coefficients B[0] := 0;
  B := B*(-1);
  SA := 0; SB := 0;
  FOR I := 0 to NPoles do
  begin
    if AHighPass and ((I mod 2) = 1) then
    begin
      SA := SA - A[I];
      SB := SB - B[I];
    end else
    begin
      SA := SA + A[I];
      SB := SB + B[I];
    end;
  end;
  GAIN := SA / (1 - SB);
  FOR I := 0 TO NPoles do
    A[I] := A[I] / GAIN;
//The final recursion coefficients are in A[ ] and B[ ]
end;

end.

