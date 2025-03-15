unit Transforms;
{$mode objfpc}{$H+}
interface
uses
  uTypes, uComplex, uErrors, uFilters, uFFT, uVecUtils, uCompVecUtils;

procedure GenerateData(DataKind:integer);

procedure FilterData(FilterKind:integer; cascade : boolean);

procedure Fourier;

var
  N  : integer = 16384;
  SR : float = 1000; //sampling rate
  SinFreq1 : float   = 2;
  SinFreq2 : float   = 50;
  SinFreq3 : float   = 110;
  CutOffFreq : float = 50;
  BW : float         = 5; //bandwidth
  NPoles : integer   = 6;
  Ripple : float     = 0.5;
  HighPass : boolean = false;

  TimeVector    : TVector;
  FreqScale     : TVector;
  RawData       : TVector;
  FilteredData  : TVector;
  RawFreqVector : TVector;
  FFreqVector   : TVector;

implementation

function Delta(I:Integer):float;
var
  N2:integer;
begin
  N2 := N div 2;
  if I = N2 then
    Result := 1
  else
    Result := 0;
end;

function Sinusoids(I:Integer):float;
begin
  Result := sin(SinFreq1*TimeVector[I]*TwoPi) + sin(SinFreq2*TimeVector[I]*TwoPi)+sin(SinFreq3*TimeVector[I]*TwoPi);
end;

function Steps(I:Integer):float;
begin
  if (TimeVector[I] > 2) and (TimeVector[I] < 6) then
    Result := 1
  else
    Result := 0;
end;

function StepSin(I:Integer):float;
begin
  Result := Sinusoids(I)+Steps(I);
end;

function Chirps(I:Integer):float;
begin
  if not (IsZero(TimeVector[I])) then
    Result := sin(1/TimeVector[I])
  else
    Result := 0;
end;

procedure GenerateData(DataKind:integer);
begin
  TimeVector := Seq(0,N-1,-4.0,1/SR);
  case DataKind of
    0: RawData := InitWithFunc(0,N-1,@Delta);
    1: RawData := InitWithFunc(0,N-1,@Steps);
    2: RawData := InitWithFunc(0,N-1,@Sinusoids);
    3: RawData := InitWithFunc(0,N-1,@StepSin);
    4: RawData := InitWithFunc(0,N-1,@Chirps);
  end;
end;

procedure FilterData(FilterKind:integer; cascade : boolean);
begin
  if not Assigned(RawData) then
  begin

    Exit;
  end;
  if not Cascade then
    FilteredData := copy(RawData,0,length(RawData))
  else begin
    if not assigned(FilteredData) then
    begin
      SetErrCode(lmDFTError,'No filtered data to be refiltered.');
      Exit;
    end;
  end;
  case FilterKind of
    0: GaussFilter(FilteredData,SR,CutOffFreq);
    1: MovingAverageFilter(FilteredData,MoveAvFindWindow(1000,CutOffFreq));
    2: MedianFilter(FilteredData,5);
    3: NotchFilter(FilteredData,SR,CutOffFreq,BW);
    4: BandpassFilter(FilteredData,SR,CutOffFreq,BW);
    5: HighPassFilter(FilteredData,SR,CutOffFreq);
    6: ChebyshevFilter(FilteredData,SR,CutOffFreq,NPoles,Ripple,Highpass);
  end;
end;

procedure Fourier;
var
  DataComp, InvComp, Pol : TCompVector;
  I,NM: integer;
begin
  DimVector(DataComp,High(RawData));
  DimVector(InvComp,High(RawData));
  NM := length(Datacomp);
  for I := 0 to High(Datacomp) do
    DataComp[I] := RawData[I];
  FFT(NM,DataComp,InvComp);
  Pol := CMakePolar(InvComp[0..N div 2 - 1]);
  RawFreqVector := ExtractReal(Pol);
  if assigned(FilteredData) then
  begin
    for I := 0 to High(FilteredData) do
      DataComp[I] := FilteredData[I];
    FFT(NM,DataComp,InvComp);
    Pol := CMakePolar(InvComp[0..(N div 2 - 1)]);
    FFreqVector := ExtractReal(Pol);
  end;
  FreqScale := Seq(0,High(FFreqVector),0,SR/N);
end;

end.

