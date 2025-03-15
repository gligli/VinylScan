{ ******************************************************************
  Fast Fourier Transform (modified from Pascal program by Don Cross)
  ******************************************************************
  This program compares the FFT with the direct computation of the
  frequencies, on a set of random data.

  Results are stored in the output file freq.txt
  ****************************************************************** }

program freq;

uses
{$IFDEF USE_DLL}
  dmath;
{$ELSE}
  utypes, ufft;
{$ENDIF}    

const
  NumSamples = 512;             { Buffer size (power of 2) }
  MaxIndex   = NumSamples - 1;  { Max. array index }

var
  InArray, OutArray : TCompVector;
  OutFile           : Text;
  I                 : Integer;
  Z                 : Complex;

begin
  DimVector(InArray, MaxIndex);
  DimVector(OutArray, MaxIndex);

  { Fill input buffers with random data }
  for I := 0 to MaxIndex do
    begin
      InArray[I].X := Random(10000);
      InArray[I].Y := Random(10000);
    end;

  Assign(OutFile, 'freq.txt');
  Rewrite(OutFile);

  WriteLn(OutFile);
  WriteLn(OutFile, '*** Testing procedure CalcFrequency ***');
  WriteLn(OutFile);

  FFT(NumSamples, InArray, OutArray);

  for I := 0 to MaxIndex do
    begin
      Z := CalcFrequency(NumSamples, I, InArray);
      WriteLn(OutFile, I:4,
              OutArray[I].X:15:6, Z.X:15:6,
              OutArray[I].Y:20:6, Z.Y:15:6);
    end;

  Close(OutFile);
end.

