unit scan2track;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Types, Math, Graphics, GraphType, FPCanvas, FPImage, FPWritePNG, MTProcs,
  utils, fgl, inputscan, FilterIIRLPBessel, FilterIIRHPBessel;

const
  CSampleDecoderBits = 8;
  CSampleDecoderMulti = 4;

  CSampleDecoderMax = (1 shl (CSampleDecoderBits - 1)) - 1;

type

  { TScan2Track }

  TScan2Track = class
  private
    FOutputWAVFileName: String;

    FScan: TInputScan;
    FBitsPerSample: Integer;
    FSampleRate: Integer;
    FPointsPerRevolution: Integer;
    FRadiansPerRevolutionPoint: Double;

    FTrack: TIntegerDynArray;

  public
    constructor Create(ASampleRate: Integer = 48000; ABitsPerSample: Integer = 16; ADPI: Integer = 2400);
    destructor Destroy; override;

    procedure EvalTrack;

    procedure Run;

    property OutputWAVFileName: String read FOutputWAVFileName write FOutputWAVFileName;

    property Scan: TInputScan read FScan;
    property SampleRate: Integer read FSampleRate;
    property BitsPerSample: Integer read FBitsPerSample;
    property PointsPerRevolution: Integer read FPointsPerRevolution;
    property RadiansPerRevolutionPoint: Double read FRadiansPerRevolutionPoint;

    property Track: TIntegerDynArray read FTrack;
  end;

implementation
uses main;

{ TScan2Track }

constructor TScan2Track.Create(ASampleRate: Integer; ABitsPerSample: Integer; ADPI: Integer);
begin
  FSampleRate := ASampleRate;
  FBitsPerSample := ABitsPerSample;

  FPointsPerRevolution := Round(FSampleRate / C45RpmRevolutionsPerSecond);
  FRadiansPerRevolutionPoint := Pi * 2.0 / FPointsPerRevolution;

  FScan := TInputScan.Create(ADPI);
end;

destructor TScan2Track.Destroy;
begin
  FScan.Free;

  inherited Destroy;
end;

procedure TScan2Track.EvalTrack;
var
  fltSamples: TFilterIIRHPBessel;
  samples: TSmallIntDynArray;

  function DecodeSample(radius, angle: Double; out feedback: Double): Double;
  var
    imulti, ismp, upCnt, upAcc: Integer;
    r, px, py, middleSmp, stdDev, upLimit, cxa, cma, sn, cs: Double;
    smpBuf: array[-CSampleDecoderMax-1 .. CSampleDecoderMax] of Double;
  begin
    cxa := C45RpmRecordingGrooveWidth * Scan.DPI / CSampleDecoderMax;
    cma := FRadiansPerRevolutionPoint / CSampleDecoderMulti;

    Result := 0;
    for imulti := 0 to CSampleDecoderMulti - 1 do
    begin
      SinCos(angle + imulti * cma, sn, cs);

      for ismp := -CSampleDecoderMax-1 to CSampleDecoderMax do
      begin
        r := radius + ismp * cxa;

        px := cs * r + Scan.Center.X;
        py := sn * r + Scan.Center.Y;

        if Scan.InRangePointD(py, px) then
          smpBuf[ismp] := Scan.GetPointD(py, px, isImage)
        else
          smpBuf[ismp] := 0.0;
      end;

      MeanAndStdDev(smpBuf, middleSmp, stdDev);

      upLimit := middleSmp + stdDev;

      //middleSmp := Mean(smpBuf);
      //middleSmp := (MinValue(smpBuf) + MaxValue(smpBuf)) * 0.5;

      upAcc := 0;
      upCnt := 0;
      for ismp := -CSampleDecoderMax-1 to CSampleDecoderMax do
        if InRange(smpBuf[ismp], middleSmp, upLimit) then
        begin
          upAcc += ismp;
          Inc(upCnt);
        end;

      Result += DivDef(upAcc, CSampleDecoderMax * upCnt, 0.0);
    end;

    Result *= (1.0 / CSampleDecoderMulti);

    feedback := Result * C45RpmRecordingGrooveWidth * Scan.DPI;
  end;

  procedure StoreSample(fsmp: Double; pos: Integer);
  var
    smp: SmallInt;
  begin
    smp :=  Make16BitSample(fsmp);

    while pos >= Length(samples) do
      SetLength(samples, Ceil(Length(samples) * cPhi));
    samples[pos] := smp;
  end;

var
  angle, radius, sn, cs, px, py, feedback, fbRatio, fsmp, ffSmp: Double;
  iSample, iLut: Integer;
  pbuf: TPointFList;
  t, pt: QWord;
  sinCosLut: TPointDDynArray;
begin
  WriteLn('EvalTrack');

  SetLength(samples, FSampleRate);
  fltSamples := TFilterIIRHPBessel.Create(nil);
  pbuf := TPointFList.Create;
  try
    fltSamples.FreqCut1 := CLowCutoffFreq;
    fltSamples.SampleRate := FSampleRate;
    fltSamples.Order := 4;

    pt := GetTickCount64;

    fbRatio := CutoffToFeedbackRatio(CLowCutoffFreq, FSampleRate) * 2.0;
    BuildSinCosLUT(FPointsPerRevolution, sinCosLut, Scan.GrooveStartAngle, -2.0 * Pi);

    iSample := 0;
    iLut := 0;
    radius := Scan.FirstGrooveRadius;
    repeat
      angle := Scan.GrooveStartAngle - FRadiansPerRevolutionPoint * iLut;

      fsmp := DecodeSample(radius, angle, feedback);
      ffSmp := fltSamples.FilterFilter(fsmp);
      StoreSample(ffSmp, iSample);

      radius -= C45RpmRecordingGrooveWidth * Scan.DPI / FPointsPerRevolution;
      radius += feedback * fbRatio;

      cs := sinCosLut[iLut].X;
      sn := sinCosLut[iLut].Y;

      px := cs * radius + Scan.Center.X;
      py := sn * radius + Scan.Center.Y;
      pbuf.Add(TPointF.Create(px, py));

      Inc(iSample);

      Inc(iLut);
      if iLut >= FPointsPerRevolution then
        iLut := 0;

////////////////////////
      t := GetTickCount64;
      if t - pt >= 4000 then
      begin
        main.MainForm.DrawPoints(pbuf, clLime);

        SetLength(samples, iSample);
        CreateWAV(1, 16, FSampleRate, FOutputWAVFileName, samples);

        pbuf.Clear;

        pt := GetTickCount64;
      end;
////////////////////////

    until not InRange(radius, Scan.ConcentricGrooveRadius, C45RpmOuterSize * 0.5 * Scan.DPI);

    SetLength(samples, iSample);
    CreateWAV(1, 16, FSampleRate, FOutputWAVFileName, samples);
  finally
    pbuf.Free;
    fltSamples.Free;
  end;

  WriteLn('Done!');
end;

procedure TScan2Track.Run;
begin
  Scan.LoadPNG;
  Scan.FindTrack;
  EvalTrack;
end;

end.

