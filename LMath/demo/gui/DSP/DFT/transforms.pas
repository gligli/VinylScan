unit Transforms;
{$mode objfpc}{$H+}
interface
uses
  uTypes, uIntervals, uComplex, uErrors, uEval, uDFT, uFFT, uVecUtils;

const
  mFFT = 0;
  mDFT = 1;

procedure GenerateData(ExprRe, ExprIm: string; N : integer; Range:TInterval;
                             out TimeLine:TVector; out Data: TCompVector);

procedure Execute(InData: TCompVector; N, Method, MethodInv: integer;
                             out TransData, InvTransdata: TCompVector);

implementation

procedure GenerateData(ExprRe, ExprIm: string; N : integer; Range: TInterval;
                             out TimeLine:TVector; out Data: TCompVector);
var
  Step: Float;
  I: integer;
begin
  DimVector(Data,N);
  Step := Range.Length / (N - 1);
  TimeLine := Seq(0,N-1,Range.Lo,Step);
  for I := 0 to N - 1 do
  begin
    SetVariable('X',TimeLine[I]);
    Data[I].X := Eval(ExprRe);
    Data[I].Y := Eval(ExprIm);
  end;
end;

procedure Execute(InData: TCompVector; N, Method, MethodInv : integer;
                             out TransData, InvTransdata: TCompVector);
begin
  DimVector(TransData,N);
  DimVector(InvTransData,N);
  case Method of
    mFFT: uFFT.FFT(N,InData,TransData);
    mDFT: begin
      TransData := copy(InData,0,N);
      FFTC1D(TransData,0,N-1);
    end;
  end;
  case MethodInv of
    mFFT: IFFT(N,TransData,InvTransData);
    mDFT: begin
      InvTransData := copy(TransData,0,N);
      FFTC1DInv(InvTransData,0,N-1);
    end;
  end;
end;

end.

