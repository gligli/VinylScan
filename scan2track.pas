unit scan2track;

{$include 'compileroptions.inc'}

interface

uses
  Classes, SysUtils, Types, Math, Graphics, GraphType, FPCanvas, FPImage, FPWritePNG, MTProcs,
  utils, inputscan, FilterIIRLPBessel, FilterIIRHPBessel;

type
  TScan2Track = class;

  TSampleEvent = function(Sender: TScan2Track; Sample, X, Y: Double; var Radius: Double; Percent: Double; Finished: Boolean): Boolean of object;

  { TScan2Track }

  TScan2Track = class
  private
    FScan: TInputScan;
    FScanOwned: Boolean;

    FOutputWAVFileName: String;
    FOnSample: TSampleEvent;

    FDecoderPrecision: Integer;
    FDecoderMax: Integer;
    FSampleRate: Integer;
    FPointsPerRevolution: Integer;
    FRadiansPerRevolutionPoint: Double;
    FSilent: Boolean;

    function DecodeSample(radius, angleSin, angleCos: Double): Double;

  public
    constructor Create(ADefaultDPI: Integer = 2400; ASilent: Boolean = False; ASampleRate: Integer = 48000; ADecoderPrecision: Integer = 10);
    constructor CreateFromInputScan(AInputScan: TInputScan; ASilent: Boolean; ADecoderPrecision: Integer);
    destructor Destroy; override;

    procedure LoadScan;
    procedure EvalTrack;

    property OnSample: TSampleEvent read FOnSample write FOnSample;
    property OutputWAVFileName: String read FOutputWAVFileName write FOutputWAVFileName;

    property Scan: TInputScan read FScan;
    property SampleRate: Integer read FSampleRate;
    property DecoderPrecision: Integer read FDecoderPrecision;
    property PointsPerRevolution: Integer read FPointsPerRevolution;
    property RadiansPerRevolutionPoint: Double read FRadiansPerRevolutionPoint;
  end;

implementation

{ TScan2Track }

constructor TScan2Track.Create(ADefaultDPI: Integer; ASilent: Boolean; ASampleRate: Integer;
  ADecoderPrecision: Integer);
begin
  FSampleRate := ASampleRate;
  FDecoderPrecision := EnsureRange(ADecoderPrecision, 1, 16);
  FDecoderMax := 1 shl (FDecoderPrecision - 1);
  FSilent := ASilent;

  FPointsPerRevolution := Round(FSampleRate / C45RpmRevolutionsPerSecond);
  FRadiansPerRevolutionPoint := Pi * 2.0 / FPointsPerRevolution;

  FScan := TInputScan.Create(ADefaultDPI);
  FScanOwned := True;
end;

constructor TScan2Track.CreateFromInputScan(AInputScan: TInputScan; ASilent: Boolean; ADecoderPrecision: Integer);
begin
  FPointsPerRevolution := AInputScan.PointsPerRevolution;
  FRadiansPerRevolutionPoint := AInputScan.RadiansPerRevolutionPoint;

  FSampleRate := Round(FPointsPerRevolution * C45RpmRevolutionsPerSecond);
  FDecoderPrecision := EnsureRange(ADecoderPrecision, 1, 16);
  FDecoderMax := 1 shl (FDecoderPrecision - 1);
  FSilent := ASilent;

  FScan := AInputScan;
end;

destructor TScan2Track.Destroy;
begin
  if FScanOwned then
    FScan.Free;

  inherited Destroy;
end;

function TScan2Track.DecodeSample(radius, angleSin, angleCos: Double): Double;
var
  iSmp, upCnt, posMin, posMax: Integer;
  r, px, py, cxa: Double;
  sample, sampleMin, sampleMax, sampleMiddle, sampleIdx, upAcc: Single;
  smpBuf: array[SmallInt] of Double;
begin
  cxa := C45RpmRecordingGrooveWidth * Scan.DPI / (FDecoderMax - 0.5);

  r := radius + FDecoderMax * cxa;
  px := angleCos * r + Scan.Center.X;
  py := angleSin * r * Scan.SkewY + Scan.Center.Y;

  if not Scan.InRangePointD(py, px) then
    Exit(0.0);

  posMin := -FDecoderMax;
  posMax := FDecoderMax - 1;
  sampleMin := Infinity;
  sampleMax := -Infinity;
  for iSmp := posMin to posMax do
  begin
    r := radius + (iSmp + 0.5) * cxa;
    px := angleCos * r + Scan.Center.X;
    py := angleSin * r * Scan.SkewY + Scan.Center.Y;

    sample := Scan.GetPointD_Sinc(Scan.Image, py, px);

    sampleMin := Min(sampleMin, sample);
    sampleMax := Max(sampleMax, sample);

    smpBuf[iSmp] := sample;
  end;

  sampleMiddle := (sampleMax + sampleMin) * 0.5;

  upAcc := 0;
  upCnt := 0;
  for iSmp := posMin to posMax do
    if smpBuf[iSmp] >= sampleMiddle then
    begin
      upAcc += iSmp + 0.5;
      Inc(upCnt);
    end;

  sampleIdx := DivDef(upAcc, upCnt, 0.0);

  Result := sampleIdx / (FDecoderMax - 0.5);
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
  hasOutFile, validSample: Boolean;
  fltSample: TFilterIIRHPBessel;
  sinCosLut: TSinCosDynArray;
begin
  if not FSilent then
    WriteLn('EvalTrack');

  hasOutFile := Trim(FOutputWAVFileName) <> '';
  if hasOutFile then
    SetLength(samples, FSampleRate);
  fltSample := TFilterIIRHPBessel.Create(nil);
  try
    fltSample.FreqCut1 := CLowCutoffFreq;
    fltSample.SampleRate := FSampleRate;
    fltSample.Order := 4;

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

      if hasOutFile then
      begin
        StoreSample(ffSmp, iSample);
        Inc(iSample);
      end;

      validSample := InRange(radius, Scan.ConcentricGrooveRadius, rOuter);

      if Assigned(FOnSample) then
      begin
        ox := cs * radius;
        oy := sn * radius * Scan.SkewY;

        pct := (Sqrt(Sqr(ox) + Sqr(oy)) - Scan.ConcentricGrooveRadius) / (Scan.FirstGrooveRadius - Scan.ConcentricGrooveRadius);
        pct := EnsureRange(1.0 - pct, 0.0, 1.0) * 100.0;

        validSample := FOnSample(Self, ffSmp, ox + Scan.Center.X, oy + Scan.Center.Y, radius, pct, not validSample) and validSample;
      end;

    until not validSample;

    if hasOutFile then
    begin
      SetLength(samples, iSample);
      CreateWAV(1, 16, FSampleRate, FOutputWAVFileName, samples);
    end;

    if not FSilent then
      WriteLn('Done!');
  finally
    fltSample.Free;
  end;
end;

procedure TScan2Track.LoadScan;
begin
  Scan.LoadImage;
  Scan.FindTrack(FSampleRate);
end;

end.

