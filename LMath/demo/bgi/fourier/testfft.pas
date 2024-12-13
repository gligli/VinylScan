{ ******************************************************************
  Fast Fourier Transform (modified from Pascal program by Don Cross)
  ******************************************************************
  The program generates a time signal consisting of a large 200 Hz 
  sine wave added to a small 2000 Hz cosine wave, which is graphed
  on the screen (Press a key after you are done viewing each graph)

  Next, it performs the FFT and graphs the resulting complex
  frequency samples.

  Results are stored in the output file testfft.txt
  ****************************************************************** }

program testfft;

uses
{$IFDEF USE_DLL}
  dmath;
{$ELSE}
  utypes, uminmax, ufft, uScaling, uplot;
{$ENDIF} 

const
  PathToBGI = 'c:\tp\bgi';  { Change as necessary }

  function F(T : Float) : Float;
  begin
    F := Sin(200 * 2 * PI * T) + 0.2 * Cos(2000 * 2 * PI * T);
  end;

const
  NumSamples   = 512;                        { Buffer size (power of 2) }
  SamplingRate = 22050;                      { Sampling rate (Hz) }
  MaxIndex     = NumSamples - 1;             { Max. array index }
  MidIndex     = NumSamples div 2;
  DT           = 1 / SamplingRate;           { Time unit (s) }
  DF           = SamplingRate / NumSamples;  { Frequency unit (Hz) }

var
  InArray, OutArray : TCompVector;
  T, Freq           : TVector;
  OutFile           : Text;
  I                 : Integer;

procedure ListData(DataArray : TCompVector; Comment : String);
var
  I : Integer;
begin
  WriteLn(OutFile, '*** ', Comment, ' ***');
  WriteLn(OutFile);
  WriteLn(OutFile, 'index':20, 'real':20, 'imag':20);
  for I := 0 to MaxIndex do
    begin
      WriteLn(OutFile, I:20,
              DataArray[I].X:20:5, DataArray[I].Y:20:5);
    end;

  WriteLn(OutFile);
  WriteLn(OutFile, '------------------------------------------------------------------------');
  WriteLn(OutFile);
end;

procedure PlotData(T : TVector; Z : TCompVector; Title : String);
var
  X                 : TVector;  { Real part of Z }
  Xmin, Xmax, Xstep : Float;    { Ox scale }
  Ymin, Ymax, Ystep : Float;    { Oy scale }
  I                 : Integer;  { Loop variable }
begin
  if not InitGraphics(9, 2, PathToBGI) then
    begin
      Writeln('Unable to set graphic mode!');
      Exit;
    end;

  SetWindow(15, 85, 15, 85, True);

  DimVector(X, MaxIndex);

  for I := 0 to MaxIndex do
    X[I] := Z[I].X;

  AutoScale(T, 0, MaxIndex, LinScale, Xmin, Xmax, Xstep);
  AutoScale(X, 0, MaxIndex, LinScale, Ymin, Ymax, Ystep);

  SetOxScale(LinScale, Xmin, Xmax, Xstep);
  SetOyScale(LinScale, Ymin, Ymax, Ystep);

  SetOxTitle('Time (s)');
  SetOyTitle('Amplitude');
  SetGraphTitle(Title);

  { Set point type to 0 so that only lines will be plotted }
  SetPointParam(1, 0, 1, 1);

  PlotOxAxis;
  PlotOyAxis;

  PlotGrid(BothGrid);

  WriteGraphTitle;

  PlotCurve(T, X, 0, MaxIndex, 1);

  ReadLn;

  LeaveGraphics;
end;

procedure PlotFFT(Freq : TVector; Z : TCompVector; Title : String);
var
  Fq : TVector;  { Frequency }
  X  : TVector;  { Real part of FFT }
  Y  : TVector;  { Imag. part of FFT }

  Xmin, Xmax, Xstep       : Float;    { Ox scale }
  Yr_min, Yr_max, Yr_step : Float;    { Oy scale (real part) }
  Yi_min, Yi_max, Yi_step : Float;    { Oy scale (imag. part) }
  Ymin, Ymax, Ystep       : Float;    { Oy scale (global) }
  I                       : Integer;  { Loop variable }
begin
  DimVector(Fq, MidIndex);  { Frequency }
  DimVector(X, MidIndex);   { Real part of FFT }
  DimVector(Y, MidIndex);   { Imag. part of FFT }

  if not InitGraphics(9, 2, PathToBGI) then
    begin
      Writeln('Unable to set graphic mode!');
      Exit;
    end;

  SetWindow(15, 80, 15, 85, True);
  SetMaxCurv(2);

  for I := 0 TO MidIndex do
    begin
      Fq[I] := Freq[I];
      X[I] := Z[I].X;
      Y[I] := Z[I].Y;
    end;

  AutoScale(Fq, 0, MidIndex, LinScale, Xmin, Xmax, Xstep);
  AutoScale(X, 0, MidIndex, LinScale, Yr_min, Yr_max, Yr_step);
  AutoScale(Y, 0, MidIndex, LinScale, Yi_min, Yi_max, Yi_step);

  Ymin  := Min(Yr_min, Yi_min);
  Ymax  := Max(Yr_max, Yi_max);
  Ystep := Min(Yr_step, Yi_step);

  SetOxScale(LinScale, Xmin, Xmax, Xstep);
  SetOyScale(LinScale, Ymin, Ymax, Ystep);

  SetOxTitle('Frequency (Hz)');
  SetOyTitle('FFT');
  SetGraphTitle(Title);

  SetPointParam(1, 0, 1, 1);  { Set point type to 0 so that }
  SetPointParam(2, 0, 1, 1);  { only lines will be plotted. }

  SetCurvLegend(1, 'Real');
  SetCurvLegend(2, 'Imag.');

  PlotOxAxis;
  PlotOyAxis;

  PlotGrid(BothGrid);

  WriteGraphTitle;

  WriteLegend(2, False, True);

  PlotCurve(Fq, X, 0, MidIndex, 1);
  PlotCurve(Fq, Y, 0, MidIndex, 2);

  ReadLn;

  LeaveGraphics;
end;

begin
  DimVector(InArray, MaxIndex);
  DimVector(OutArray, MaxIndex);
  DimVector(T, MaxIndex);
  DimVector(Freq, MaxIndex);

  Assign(OutFile, 'testfft.txt');
  Rewrite(OutFile);

  for I := 0 to MaxIndex do
    begin
      T[I] := I * DT;
      Freq[I] := I * DF;
      InArray[I].X := F(T[I]);
      InArray[I].Y := 0.0;
    end;

  ListData(InArray, 'Time domain data before transform');
  PlotData(T, InArray, 'Original signal');

  FFT(NumSamples, InArray, OutArray);
  PlotFFT(Freq, OutArray, 'Fourier Transform');

  ListData(OutArray, 'Frequency domain data after transform');

  Close(OutFile);
end.
