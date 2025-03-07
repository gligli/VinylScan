unit scan2track;

{$include 'compileroptions.inc'}

interface

uses
  Classes, SysUtils, Types, Math, Graphics, GraphType, FPCanvas, FPImage, FPWritePNG, MTProcs, fgl,
  utils, inputscan, FilterIIRLPBessel, FilterIIRHPBessel;

const
  CSampleDecoderBits = 10;
  CSampleDecoderMax = 1 shl (CSampleDecoderBits - 1);

type
  TScan2Track = class;

  TSampleEvent = function(Sender: TScan2Track; X, Y, Percent: Double; Finished: Boolean): Boolean of object;

  { TScan2Track }

  TScan2Track = class
  private
    FScan: TInputScan;

    FOutputWAVFileName: String;
    FOnSample: TSampleEvent;

    FBitsPerSample: Integer;
    FSampleRate: Integer;
    FPointsPerRevolution: Integer;
    FRadiansPerRevolutionPoint: Double;

    function DecodeSample(radius, angleSin, angleCos: Double): Double;

  public
    constructor Create(ASampleRate: Integer = 48000; ABitsPerSample: Integer = 16; ADPI: Integer = 2400);
    destructor Destroy; override;

    procedure LoadPNG;
    procedure EvalTrack;

    property OnSample: TSampleEvent read FOnSample write FOnSample;
    property OutputWAVFileName: String read FOutputWAVFileName write FOutputWAVFileName;

    property Scan: TInputScan read FScan;
    property SampleRate: Integer read FSampleRate;
    property BitsPerSample: Integer read FBitsPerSample;
    property PointsPerRevolution: Integer read FPointsPerRevolution;
    property RadiansPerRevolutionPoint: Double read FRadiansPerRevolutionPoint;
  end;

implementation

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

function TScan2Track.DecodeSample(radius, angleSin, angleCos: Double): Double;
var
  ismp, upCnt: Integer;
  r, px, py, cxa, sample, sampleMiddle, upAcc: Double;
  smpBuf: array[-CSampleDecoderMax .. CSampleDecoderMax - 1] of Double;
begin
  cxa := C45RpmRecordingGrooveWidth * Scan.DPI / (CSampleDecoderMax - 0.5);

  r := radius + CSampleDecoderMax * cxa;
  px := angleCos * r + Scan.Center.X;
  py := angleSin * r * Scan.SkewY + Scan.Center.Y;

  if not Scan.InRangePointD(py, px) then
    Exit(0.0);

  sampleMiddle := 0.0;
  for ismp := -CSampleDecoderMax to CSampleDecoderMax - 1 do
  begin
    r := radius + (ismp + 0.5) * cxa;
    px := angleCos * r + Scan.Center.X;
    py := angleSin * r * Scan.SkewY + Scan.Center.Y;

    sample := Scan.GetPointD_Sinc(Scan.Image, py, px);

    sampleMiddle += sample;

    smpBuf[ismp] := sample;
  end;

  sampleMiddle /= CSampleDecoderMax * 2.0;

  upAcc := 0;
  upCnt := 0;
  for ismp := -CSampleDecoderMax to CSampleDecoderMax - 1 do
    if smpBuf[ismp] >= sampleMiddle  then
    begin
      upAcc += ismp + 0.5;
      Inc(upCnt);
    end;

  Result := DivDef(upAcc, (CSampleDecoderMax - 0.5) * upCnt, 0.0);
end;


procedure TScan2Track.EvalTrack;
var
  samples: TSmallIntDynArray;

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
  radius, rOuter, sn, cs, ox, oy, fbRatio, fSmp, ffSmp, pct: Double;
  iSample, iLut: Integer;
  validSample: Boolean;
  fltSample: TFilterIIRHPBessel;
  sinCosLut: TSinCosDynArray;
begin
  WriteLn('EvalTrack');

  SetLength(samples, FSampleRate);
  fltSample := TFilterIIRHPBessel.Create(nil);
  try
    fltSample.FreqCut1 := CLowCutoffFreq;
    fltSample.SampleRate := FSampleRate;
    fltSample.Order := 1;

    BuildSinCosLUT(FPointsPerRevolution, sinCosLut, Scan.GrooveStartAngle, -2.0 * Pi);

    fbRatio := CutoffToFeedbackRatio(CLowCutoffFreq, FSampleRate) * C45RpmRecordingGrooveWidth * Scan.DPI;

    rOuter := C45RpmOuterSize * 0.5 * Scan.DPI;
    iSample := 0;
    iLut := 0;
    radius := Scan.FirstGrooveRadius;
    repeat
      cs := sinCosLut[iLut].Cos;
      sn := sinCosLut[iLut].Sin;
      Inc(iLut);
      if iLut >= FPointsPerRevolution then
        iLut := 0;

      fSmp := DecodeSample(radius, sn, cs);
      radius += fSmp * fbRatio;

      ffSmp := fltSample.FilterFilter(fSmp);
      StoreSample(ffSmp, iSample);
      Inc(iSample);

      validSample := InRange(radius, Scan.ConcentricGrooveRadius, rOuter);

      if Assigned(FOnSample) then
      begin
        ox := cs * radius;
        oy := sn * radius * Scan.SkewY;

        pct := (Sqrt(Sqr(ox) + Sqr(oy)) - Scan.ConcentricGrooveRadius) / (Scan.FirstGrooveRadius - Scan.ConcentricGrooveRadius);
        pct := EnsureRange(1.0 - pct, 0.0, 1.0) * 100.0;

        validSample := FOnSample(Self, ox + Scan.Center.X, oy + Scan.Center.Y, pct, not validSample) and validSample;
      end;

    until not validSample;

    SetLength(samples, iSample);
    CreateWAV(1, 16, FSampleRate, FOutputWAVFileName, samples);
  finally
    fltSample.Free;
  end;

  WriteLn;
  WriteLn('Done!');
end;

procedure TScan2Track.LoadPNG;
begin
  Scan.LoadPNG;
  Scan.FindTrack(FSampleRate);
end;

end.

