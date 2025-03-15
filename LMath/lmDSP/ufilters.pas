unit uFilters;

{$mode objfpc}{$H+}

interface

uses
  uTypes, uErrors, uMedian, uMeanSD, uVectorHelper, uFindFilterCoeffs;

procedure GaussFilter(var Data:array of float; ASamplingRate: Float;  ACutFreq: Float);

procedure MovingAverageFilter(var Data:array of float; WinLength:integer);

procedure MedianFilter(var Data:array of float; WinLength:integer);

// notch filter rejects AFreqReject, aBW is rejected bandwidth, measured at 0.5 power (0.7 amplitude)
procedure NotchFilter(var Data:array of float; ASamplingRate: Float; AFreqReject: Float; ABW: Float);

// notch filter passes AFreqReject, aBW is rejected bandwidth, measured at 0.5 power (0.7 amplitude)
procedure BandPassFilter(var Data:array of float; ASamplingRate: Float; AFreqPass: Float; ABW: Float);

procedure HighPassFilter(var Data:array of float; ASamplingRate: Float; ACutFreq: Float);

// NPoles is number of Poles, for stability we do not recomment values > 10; PRipple is allowed value of ripple
// in the passband in %, may be 0 (which means that Chebyshev filter becomes Butterworth filter) or
// 0.5 <= PRipple <= 29.
procedure ChebyshevFilter(var Data:array of float; ASamplingRate: Float; ACutFreq: Float;
                              NPoles: integer; PRipple: float; AHighPass:boolean);

//finds effective cutoff frequency of cascade of 2 gaussian filters
function GaussCascadeFreq(Freq1, Freq2:Float):Float;

// finds risetime (10-90%) of a gaussian filter with given cut-off frequency
function GaussRiseTime(Freq:Float):Float;

//risetime of moving average filter (0-100%)
function MovAvRiseTime(SamplingRate:Float; WLength:integer):Float;

//cut-off freq. of moving average filter, given sampling rate and window length
function MoveAvCutOffFreq(SamplingRate:Float; WLength:integer):Float;

//find required window length from desired cut-off freq. and sampling rate
function MoveAvFindWindow(SamplingRate, CutOffFreq:Float):Integer;

implementation

{%REGION Moving Average}
function MovAvRiseTime(SamplingRate: Float; WLength: integer): Float;
begin
  if (SamplingRate <= 0) or (WLength < 1) then
  begin
    SetErrCode(FDomain);
    Result := 0;
    Exit;
  end;
  Result := WLength/SamplingRate;
end;

function MoveAvCutOffFreq(SamplingRate: Float; WLength: integer): Float;
begin
  if (SamplingRate <= 0) or (WLength < 1) then
  begin
    SetErrCode(FDomain);
    Result := 0;
    Exit;
  end;
  Result := 0.44292/Sqrt(WLength*WLength-1)*SamplingRate;
end;

function MoveAvFindWindow(SamplingRate, CutOffFreq: Float): Integer;
var
  F:Float;
begin
  F := CutOffFreq/SamplingRate;
  if (SamplingRate <= 0) or (CutOffFreq <= 0) then
     SetErrCode(FDomain);
  if F > 0.5 then
    SetErrCode(lmTooHighFreqError);
  if MathErr <> MatOK then
  begin
    Result := 0;
    exit;
  end;
  Result := Round(Sqrt(0.196196+F*F)/F);
end;

procedure MovingAverageFilter(var Data:array of float; WinLength:integer);
var
  Buffer : Float;
  First  :float;
  IndNext: integer;
  I      :integer;
  Ub : integer;
begin
  Ub := high(Data);
  if WinLength > Ub then
    SetErrCode(lmDSPFilterWinError);
  if MathErr <> MatOK then
    Exit;
  IndNext := WinLength;  // points on first element after window
  Buffer := Sum(Data[0..IndNext-1]);
  for I := 0 to Ub - WinLength do
  begin
    First := Data[I];
    Data[I] := Buffer / WinLength;
    Buffer := Buffer - First;
    Buffer := Buffer + Data[IndNext];
    Inc(IndNext);
  end;
  for I := Ub - WinLength + 1 to Ub do
  begin
    First := Data[I];
    Data[I] := Buffer / WinLength;
    Buffer := Buffer - First;
    Buffer := Buffer + Data[Ub];
  end;
end;

{%ENDREGION}

{%REGION Gaussian Filter}

procedure GSForwardFilter(var Data:array of float; Bs:TPTArray; Bl:Float);
var
  WD : TPTArray;
  Pt : array [TPT] of TPT;
  I  : integer;
  J  : TPT;
begin
  WD[0] := Data[0];
  Pt[0] := 0;
  for I := 1 to 3 do
  begin
    WD[I] := WD[0];  //data are padded before beginning with Data[0]
    Pt[I] := I;
  end;
  for I := 0 to High(Data) - 1 do
  begin
    Data[I] := WD[Pt[3]];
    WD[Pt[3]] := BL*Data[I+1]+(WD[Pt[2]]*Bs[1]+WD[Pt[1]]*Bs[2]+Bs[3]*WD[Pt[0]])/Bs[0];
    for J := 0 to 3 do
      if Pt[J] < 3 then
        Pt[J] := Succ(Pt[J])
      else
        Pt[J] := 0;
  end;
  Data[High(Data)] := WD[Pt[3]];
end;

procedure GSBackwardFilter(var Data:array of float; Bs:TPTArray; BL: Float);
var
  WD : TPTArray;
  Pt : array [TPT] of TPT;
  I  : integer;
  J  : TPT;
  Ub : integer;
begin
  Ub := High(Data);
  WD[0] := Data[Ub];
  Pt[0] := 0;
  for J := 1 to 3 do
  begin
    Pt[J] := J;
    WD[J] := WD[0];
  end;
  for I := Ub downto 1 do
  begin
    Data[I] := WD[Pt[3]];
    WD[Pt[3]] := BL*Data[I-1]+(WD[Pt[2]]*Bs[1]+WD[Pt[1]]*Bs[2]+Bs[3]*WD[Pt[0]])/Bs[0];
    for J := 0 to 3 do
      if Pt[J] < High(TPT) then
        Pt[J] := Succ(Pt[J])
      else
        Pt[J] := Low(TPT);
  end;
  Data[0] := WD[Pt[3]];
end;

procedure GaussFilter(var Data:array of float; ASamplingRate: Float;  ACutFreq: Float);
var
  Sigma : Float;
      Q : Float;
     Bs : TPTarray;
     BL : Float;
begin
  if ACutFreq / ASamplingRate > 0.5 then
  begin
    SetErrCode(lmTooHighFreqError);
    exit;
  end;
  GSFindParams(ASamplingRate,ACutFreq,Sigma,BL,Q,Bs);
  GSForwardFilter(Data,Bs,BL);
  GSBackwardFilter(Data,Bs,BL);
end;

function GaussCascadeFreq(Freq1, Freq2: Float): Float;
begin
  if (Freq1 < 0) or (Freq2 < 0) then
  begin
    SetErrCode(FDomain);
    Result := 0;
    Exit;
  end;
  if Freq1 = 0 then
    Result := Freq2
  else if Freq2 = 0 then
    Result := Freq1
  else
    Result := 1/Sqrt(1/Freq1/Freq1 + 1/Freq2/Freq2);
end;

function GaussRiseTime(Freq: Float): Float;
begin
  if Freq < 0 then
  begin
    SetErrCode(FDomain);
    Result := 0;
    Exit;
  end;
  if Freq = 0 then
    Result := MaxNum
  else
    Result := 0.33/Freq;
end;
{%ENDREGION}

{%REGION Median Filter}
procedure MedianFilter(var Data:array of float; WinLength:integer);
var
  I,HighWin : integer;
  Ub : integer;
  Buffer:TVector;
begin
  Ub := High(Data);
  if WinLength > Ub then
    SetErrCode(lmDSPFilterWinError);
  if MathErr <> MatOK then
    Exit;
  HighWin := WinLength-1;
  SetLength(Buffer,WinLength);
  for I := 0 to Ub-WinLength do
    Data[I] := Median(Data[I..I+HighWin]);
  for I := Ub-WinLength+1 to Ub do
  begin
    Buffer.FillWithArr(0,Data[I..Ub]);
    Buffer.Fill(Ub-I,HighWin,Data[Ub]);
    Data[I] := Median(Buffer);
  end;
end;
{%ENDREGION}

{%REGION NARROWBAND Filters}

procedure ApplyNarrowBandFilter(var Data:array of float; const A: TInCoeffs; const B: TRecursCoeffs);
var
  I,J: integer;
  Old: array[-2..0] of Float;
begin
  for I := -1 to 0 do
    Old[I] := Data[I+1];

  for I := 2 to high(Data) do
  begin
    for J := -2 to -1 do
      Old[J] := Old[J+1];
    Old[0] := Data[I];
    Data[I] := Data[I]*A[0]+Old[-1]*A[1]+Old[-2]*A[2]+Data[I-1]*B[1]+Data[I-2]*B[2];
  end;
end;

procedure NotchFilter(var Data:array of float; ASamplingRate: Float; AFreqReject: Float; ABW: Float);
var
  K, R, CoF : Float;
  A: TInCoeffs;
  B: TRecursCoeffs;
begin
  if AFreqReject / ASamplingRate > 0.5 then
  begin
    SetErrCode(lmTooHighFreqError);
    Exit;
  end;
  FindNarrowBandParams(AFreqReject,ABW,ASamplingRate,K,R,CoF);
  FindNotchCoeffs(K,R,CoF,A,B);
  ApplyNarrowBandFilter(Data,A,B);
end;

// notch filter passes AFreqReject, aBW is rejected bandwidth, measured at 0.5 power (0.7 amplitude)
procedure BandPassFilter(var Data:array of float; ASamplingRate: Float; AFreqPass: Float; ABW: Float);
var
  K, R, CoF : Float;
  A: TInCoeffs;
  B: TRecursCoeffs;
begin
  if AFreqPass / ASamplingRate > 0.5 then
  begin
    SetErrCode(lmTooHighFreqError);
    Exit;
  end;
  FindNarrowBandParams(AFreqPass,ABW,ASamplingRate,K,R,CoF);
  FindBandPassCoeffs(K,R,CoF,A,B);
  ApplyNarrowBandFilter(Data,A,B);
end;
{%ENDREGION}

procedure HighPassFilter(var Data: array of float; ASamplingRate: Float; ACutFreq: Float);
var
  X, Old0, Old1: float;
  A0, A1 : float;
  I : integer;
begin
  if ACutFreq / ASamplingRate > 0.5 then
  begin
    SetErrCode(lmTooHighFreqError);
    Exit;
  end;
  X := exp(-TwoPi*ACutFreq/ASamplingRate);
  A0 := (1+X)/2;
  A1 := -A0;
  Old0 := Data[0];
  for I := 1 to high(Data) do
  begin
    Old1 := Old0;
    Old0 := Data[I];
    Data[I] := Data[I]*A0 + Old1*A1 + Data[I-1]*X;
  end;
end;

procedure ChebyshevFilter(var Data:array of float; ASamplingRate: Float; ACutFreq: Float;
                              NPoles: integer; PRipple: float; AHighPass:boolean);
var
  A, B, BR: TVector;
  I, J, K: integer;
  Old : TVector;
begin
  if not (NPoles in [2,4,6,8,10]) then
    SetErrCode(lmPolesNumError);
  if ACutFreq > 0.5*ASamplingRate then
    SetErrCode(lmTooHighFreqError);
  if (PRipple < 0) or (PRipple > 29.0) then
    SetErrCode(lmFFTBadRipple);
  if MathErr <> matOK then
    Exit;
  DimVector(Old,NPoles);
  DimVector(BR,NPoles);
  FindChebyshevCoeffs(ASamplingRate, ACutFreq, AHighPass, PRipple, NPoles, A, B);
  J := NPoles;
  for I := 0 to NPoles do
  begin                   // reverse B
    BR[J] := B[I];
    Dec(J);
  end;
  J := NPoles-1;
  for I := 0 to NPoles-1 do
  begin
    Old[J] := Data[I];
    Dec(J);  // now Old contains beginning of Data in reversed order
  end;
  for I := 0 to NPoles - 1 do
    Data[I] := 0;
  for I := NPoles to High(Data) do
  begin
    Old.Insert(Data[I],0); // value is inserted into beginning of Old, others are shifted to the right; last lost
    K := I - NPoles;
    Data[I] := 0;
    for J := 0 to NPoles do
      Data[I] := Data[I] + Old[J]*A[J] + Data[K+J]*BR[J];
  end;
end;

end.

