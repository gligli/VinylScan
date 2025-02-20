unit scan2track;

{$include 'compileroptions.inc'}

interface

uses
  Classes, SysUtils, Types, Math, Graphics, GraphType, FPCanvas, FPImage, FPWritePNG, MTProcs,
  utils, fgl, inputscan, FilterIIRLPBessel, FilterIIRHPBessel;

const
  CSampleDecoderBits = 12;
  CSampleDecoderMax = 1 shl (CSampleDecoderBits - 2);

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

    procedure LoadPNG;
    procedure EvalTrack;

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
  fltSample: TFilterIIRHPBessel;
  fltMiddle: TFilterIIRLPBessel;
  samples: TSmallIntDynArray;

  function DecodeSample(radius, angleSin, angleCos: Double): Double;
  const
    CMaxOffset = round(C45RpmRecordingGrooveWidth * 19200 {dpi});
  type
    THerpCoeffs = array[0 .. 3, 0 .. 3] of Integer;
  var
    j, ismp, ix, iy, icx, icy, cx, cy, upCnt, upAcc, a1, a2, a3, y0, y1, y2, y3, sample, sampleMin, sampleMax, sampleMiddle: Integer;
    r, px, py, cxa, f1, f2, f3: Double;
    phc: ^THerpCoeffs;
    coeffs: array[-CMaxOffset .. CMaxOffset, -CMaxOffset .. CMaxOffset] of THerpCoeffs;
    emptys: array[-CMaxOffset .. CMaxOffset, -CMaxOffset .. CMaxOffset] of Boolean;
    smpBuf: array[-CSampleDecoderMax-1 .. CSampleDecoderMax] of Integer;
  begin
    FillChar(emptys, SizeOf(emptys), 1);

    cxa := C45RpmRecordingGrooveWidth * Scan.DPI / CSampleDecoderMax;

    cx := Trunc(angleCos * radius + Scan.Center.X);
    cy := Trunc(angleSin * radius + Scan.Center.Y);

    sampleMin := High(Integer);
    sampleMax := Low(Integer);
    for ismp := -CSampleDecoderMax-1 to CSampleDecoderMax do
    begin
      r := radius + ismp * cxa;

      px := angleCos * r + Scan.Center.X;
      py := angleSin * r + Scan.Center.Y;

      if Scan.InRangePointD(py, px) then
      begin
        ix := Trunc(px);
        iy := Trunc(py);

        icx := ix - cx;
        icy := iy - cy;

        phc := @coeffs[icy, icx];

        if emptys[icy, icx] then
        begin
          for j := 0 to 3 do
          begin
            phc^[j, 0] := Scan.Image[iy + j - 1, ix - 1];
            phc^[j, 1] := Scan.Image[iy + j - 1, ix + 0];
            phc^[j, 2] := Scan.Image[iy + j - 1, ix + 1];
            phc^[j, 3] := Scan.Image[iy + j - 1, ix + 2];

            herpCoeffs(phc^[j, 0], phc^[j, 1] , phc^[j, 2], phc^[j, 3]);

            phc^[j, 0] := phc^[j, 0] shl 15;
          end;

          emptys[icy, icx] := False;
        end;

        f1 := px - ix;
        f2 := Sqr(f1);
        f3 := f2 * f1;

        a1 := round(f1 * (High(SmallInt) + 1));
        a2 := round(f2 * (High(SmallInt) + 1));
        a3 := round(f3 * (High(SmallInt) + 1));

        y0 := phc^[0, 0] + phc^[0, 1] * a1 + phc^[0, 2] * a2 + phc^[0, 3] * a3;
        y1 := phc^[1, 0] + phc^[1, 1] * a1 + phc^[1, 2] * a2 + phc^[1, 3] * a3;
        y2 := phc^[2, 0] + phc^[2, 1] * a1 + phc^[2, 2] * a2 + phc^[2, 3] * a3;
        y3 := phc^[3, 0] + phc^[3, 1] * a1 + phc^[3, 2] * a2 + phc^[3, 3] * a3;

        sample := herp(y0, y1, y2, y3, py - iy);
      end
      else
      begin
        sample := 0;
      end;

      sampleMin := Min(sampleMin, sample);
      sampleMax := Max(sampleMax, sample);

      smpBuf[ismp] := sample;
    end;

    sampleMiddle := Round(fltMiddle.FilterFilter((sampleMin + sampleMax) * 0.5));

    upAcc := 0;
    upCnt := 0;
    for ismp := -CSampleDecoderMax-1 to CSampleDecoderMax do
      if smpBuf[ismp] >= sampleMiddle then
      begin
        upAcc += ismp;
        Inc(upCnt);
      end;

    Result := DivDef(upAcc, CSampleDecoderMax * upCnt, 0.0);
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
  radius, sn, cs, px, py, fbRatio, fsmp, ffSmp: Double;
  iSample, iLut: Integer;
  pbuf: TPointFList;
  t, pt: QWord;
  sinCosLut: TSinCosDynArray;
begin
  WriteLn('EvalTrack');

  SetLength(samples, FSampleRate);
  fltSample := TFilterIIRHPBessel.Create(nil);
  fltMiddle := TFilterIIRLPBessel.Create(nil);
  pbuf := TPointFList.Create;
  try
    fltMiddle.FreqCut1 := CLowCutoffFreq;
    fltMiddle.SampleRate := FSampleRate;
    fltMiddle.Order := 4;

    fltSample.FreqCut1 := CLowCutoffFreq;
    fltSample.SampleRate := FSampleRate;
    fltSample.Order := 4;

    pt := GetTickCount64;

    fbRatio := 0.01;
    BuildSinCosLUT(FPointsPerRevolution, sinCosLut, Scan.GrooveStartAngle, -2.0 * Pi);

    iSample := 0;
    iLut := 0;
    radius := Scan.FirstGrooveRadius;
    repeat
      cs := sinCosLut[iLut].Cos;
      sn := sinCosLut[iLut].Sin;

      fsmp := DecodeSample(radius, sn, cs);

      ffSmp := fltSample.FilterFilter(fsmp);
      StoreSample(ffSmp, iSample);

      radius -= C45RpmRecordingGrooveWidth * Scan.DPI / FPointsPerRevolution;
      radius += fsmp * fbRatio;

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

    main.MainForm.DrawPoints(pbuf, clLime);

    SetLength(samples, iSample);
    CreateWAV(1, 16, FSampleRate, FOutputWAVFileName, samples);
  finally
    pbuf.Free;
    fltMiddle.Free;
    fltSample.Free;
  end;

  WriteLn('Done!');
end;

procedure TScan2Track.LoadPNG;
begin
  Scan.LoadPNG;
  Scan.FindTrack(FSampleRate);
end;

end.

