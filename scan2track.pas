unit scan2track;

{$include 'compileroptions.inc'}

interface

uses
  Classes, SysUtils, Types, Math, Graphics, GraphType, FPCanvas, FPImage, FPWritePNG, MTProcs, fgl,
  utils, inputscan, powell, FilterIIRLPBessel, FilterIIRHPBessel;

const
  CSampleDecoderBits = 12;
  CSampleDecoderMax = 1 shl (CSampleDecoderBits - 2);

type
  TScan2Track = class;

  TSampleEvent = function(Sender: TScan2Track; X, Y, Percent: Double; Finished: Boolean): Boolean of object;

  { TScan2Track }

  TScan2Track = class
  private
    FOutputWAVFileName: String;
    FOnSample: TSampleEvent;

    FScan: TInputScan;
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
  r, px, py, cxa, sample, sampleMin, sampleMax, sampleMiddle, upAcc: Double;
  smpBuf: array[-CSampleDecoderMax-1 .. CSampleDecoderMax] of Double;
begin
  cxa := C45RpmRecordingGrooveWidth * Scan.DPI / CSampleDecoderMax;

  r := radius + CSampleDecoderMax * cxa;
  px := angleCos * r + Scan.Center.X;
  py := angleSin * r + Scan.Center.Y;

  if not Scan.InRangePointD(py, px) then
    Exit(0.0);

  sampleMin := Infinity;
  sampleMax := -Infinity;
  for ismp := -CSampleDecoderMax-1 to CSampleDecoderMax do
  begin
    r := radius + ismp * cxa;
    px := angleCos * r + Scan.Center.X;
    py := angleSin * r + Scan.Center.Y;

    sample := Scan.GetPointD_Sinc(Scan.Image, py, px);

    sampleMin := Min(sampleMin, sample);
    sampleMax := Max(sampleMax, sample);

    smpBuf[ismp] := sample;
  end;

  sampleMiddle := (sampleMax + sampleMin) * 0.5;
  //sampleMiddle := Mean(smpBuf);

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
  radius, rOuter, sn, cs, px, py, fbRatio, fSmp, ffSmp, pct: Double;
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
    fltSample.Order := 4;

    BuildSinCosLUT(FPointsPerRevolution, sinCosLut, Scan.GrooveStartAngle, -2.0 * Pi);

    fbRatio := CutoffToFeedbackRatio(C45RpmLowCutoffFreq, FSampleRate) * C45RpmRecordingGrooveWidth * Scan.DPI;

    rOuter := C45RpmOuterSize * 0.5 * Scan.DPI;
    iSample := 0;
    iLut := 0;
    radius := Scan.FirstGrooveRadius;
    repeat
      cs := sinCosLut[iLut].Cos;
      sn := sinCosLut[iLut].Sin;

      fSmp := DecodeSample(radius, sn, cs);

      ffSmp := fltSample.FilterFilter(fSmp);

      StoreSample(ffSmp, iSample);

      radius += fSmp * fbRatio;

      px := cs * radius + Scan.Center.X;
      py := sn * radius + Scan.Center.Y;

      Inc(iSample);

      Inc(iLut);
      if iLut >= FPointsPerRevolution then
        iLut := 0;

      validSample := InRange(radius, Scan.ConcentricGrooveRadius, rOuter);

      if Assigned(FOnSample) then
      begin
        pct := (radius - Scan.ConcentricGrooveRadius) / (Scan.FirstGrooveRadius - Scan.ConcentricGrooveRadius);
        pct := EnsureRange(1.0 - pct, 0.0, 1.0) * 100.0;

        validSample := validSample and FOnSample(Self, px, py, pct, not validSample);
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

