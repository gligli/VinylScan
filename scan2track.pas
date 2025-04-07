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
    FSampleRate: Integer;
    FPointsPerRevolution: Integer;
    FRadiansPerRevolutionPoint: Double;
    FSilent: Boolean;

    FPrevSamples: TSingleDynArray;
    FPrevSamplesReady: Boolean;
    FPrevSamplesPos: Integer;

    FUsedSampleValues: array[SmallInt] of Boolean;

    procedure InitPrevSamples;
    procedure UpdatePrevSamples(Sample: Single);
    function GetPrevSamplesExtents: Double;
    function DecodeSample(radius, angleSin, angleCos: Double; precision: Integer; estSample: Double = NaN): Double;

  public
    constructor Create(ADefaultDPI: Integer = 2400; ASilent: Boolean = False; ASampleRate: Integer = 48000; ADecoderPrecision: Integer = 12);
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
  FSilent := ASilent;

  FPointsPerRevolution := Round(FSampleRate / C45RpmRevolutionsPerSecond);
  FRadiansPerRevolutionPoint := Pi * 2.0 / FPointsPerRevolution;

  FScan := TInputScan.Create(ADefaultDPI);
  FScanOwned := True;

  InitPrevSamples;
end;

constructor TScan2Track.CreateFromInputScan(AInputScan: TInputScan; ASilent: Boolean; ADecoderPrecision: Integer);
begin
  FPointsPerRevolution := AInputScan.PointsPerRevolution;
  FRadiansPerRevolutionPoint := AInputScan.RadiansPerRevolutionPoint;

  FSampleRate := Round(FPointsPerRevolution * C45RpmRevolutionsPerSecond);
  FDecoderPrecision := EnsureRange(ADecoderPrecision, 1, 16);
  FSilent := ASilent;

  FScan := AInputScan;

  InitPrevSamples;
end;

destructor TScan2Track.Destroy;
begin
  if FScanOwned then
    FScan.Free;

  inherited Destroy;
end;

procedure TScan2Track.InitPrevSamples;
var i: Integer;
begin
  SetLength(FPrevSamples, Ceil(FSampleRate / 1000.0));
  FPrevSamplesReady := False;
  FPrevSamplesPos := 0;

  for i := 0 to High(FPrevSamples) do
    FPrevSamples[i] := NaN;
end;

procedure TScan2Track.UpdatePrevSamples(Sample: Single);
begin
  FPrevSamples[FPrevSamplesPos] := Sample;

  Inc(FPrevSamplesPos);

  if FPrevSamplesPos >= Length(FPrevSamples) then
  begin
    FPrevSamplesReady := True;
    FPrevSamplesPos := 0;
  end;
end;

function TScan2Track.GetPrevSamplesExtents: Double;
var
  i: Integer;
begin
  if not FPrevSamplesReady then
    Exit(NaN);

  Result := StdDev(FPrevSamples);
end;

function TScan2Track.DecodeSample(radius, angleSin, angleCos: Double; precision: Integer; estSample: Double): Double;
var
  iSmp, posMin, posMax, decoderMax: Integer;
  r, px, py, cxa, psExtents, estSampleIdx: Double;
  sample, sampleMin, sampleMax, sampleMiddle, sampleIdx: Single;
  smpBuf: array[SmallInt] of Single;
  idxCnt: array[Boolean] of Integer;
  idxAcc: array[Boolean] of Integer;
  up: Boolean;
begin
  decoderMax := 1 shl (precision - 1);

  cxa := C45RpmRecordingGrooveWidth * 0.5 * Scan.DPI / decoderMax;

  r := radius + decoderMax * cxa;
  px := angleCos * r + Scan.Center.X;
  py := angleSin * r * Scan.SkewY + Scan.Center.Y;

  if not Scan.InRangePointD(py, px) then
    Exit(0.0);

  posMin := -decoderMax;
  posMax := decoderMax - 1;
  if not IsNan(estSample) and FPrevSamplesReady then
  begin
    estSampleIdx := estSample * decoderMax - 0.5;
    psExtents := GetPrevSamplesExtents;
    posMin := Max(Low(smpBuf), Floor(estSampleIdx - psExtents));
    posMax := Min(High(smpBuf), Ceil(estSampleIdx + psExtents));
  end;

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

  for up := False to True do
  begin
    idxAcc[up] := 0;
    idxCnt[up] := 0;
  end;

  for iSmp := posMin to posMax do
  begin
    sample := smpBuf[iSmp];

    up := sample >= sampleMiddle;

    idxAcc[up] += iSmp;
    Inc(idxCnt[up]);
  end;

  if not IsNan(estSample) then
  begin
    sampleIdx := DivDef(idxAcc[True], idxCnt[True], 0.0) + 0.5;
    UpdatePrevSamples(sampleIdx);
  end
  else
  begin
    sampleIdx := DivDef(idxAcc[True], idxCnt[True], 0.0) - DivDef(idxAcc[False], idxCnt[False], 0.0);
  end;

  Result := sampleIdx / decoderMax;
end;

procedure TScan2Track.EvalTrack;
var
  samples: TDoubleDynArray;

  procedure StoreSample(var samplesArray: TDoubleDynArray;  fsmp: Double; pos: Integer);
  begin
    while pos >= Length(samplesArray) do
      SetLength(samplesArray, Ceil(Length(samplesArray) * cPhi));
    samplesArray[pos] := fsmp;
  end;

var
  rOuter, sn, cs, ox, oy, fbRatio, ffSmp, instantPct, maxPct, radius, fSmp, fSmpPre: Double;
  iSample, iLut, cnt: Integer;
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
    fltSample.Order := 1;

    BuildSinCosLUT(FPointsPerRevolution, sinCosLut, Scan.GrooveStartAngle, -2.0 * Pi);

    fbRatio := CutoffToFeedbackRatio(CLowCutoffFreq, FSampleRate) * C45RpmRecordingGrooveWidth * 0.5 * Scan.DPI;

    radius := Scan.SetDownRadius;
    maxPct := 0.0;
    rOuter := C45RpmOuterSize * 0.5 * Scan.DPI;
    iSample := 0;
    iLut := 0;

    repeat
      cs := sinCosLut[iLut].Cos;
      sn := sinCosLut[iLut].Sin;
      Inc(iLut);
      if iLut >= FPointsPerRevolution then
        iLut := 0;

      fSmpPre := DecodeSample(radius, sn, cs, Max(1, FDecoderPrecision div 2));
      radius += fSmpPre * fbRatio;

      fSmp := DecodeSample(radius, sn, cs, FDecoderPrecision, fSmpPre);

      FUsedSampleValues[Make16BitSample(fSmp)] := True;

      ffSmp := fltSample.FilterFilter(fSmp);

      if hasOutFile then
        StoreSample(samples, ffSmp, iSample);

      validSample := InRange(radius, Scan.ConcentricGrooveRadius, rOuter);

      if Assigned(FOnSample) then
      begin
        ox := cs * radius;
        oy := sn * radius * Scan.SkewY;

        instantPct := (radius - Scan.ConcentricGrooveRadius) / (Scan.SetDownRadius - Scan.ConcentricGrooveRadius);
        instantPct := EnsureRange(1.0 - instantPct, 0.0, 1.0) * 100.0;
        maxPct := Max(maxPct, instantPct);

        validSample := FOnSample(Self, ffSmp, ox + Scan.Center.X, oy + Scan.Center.Y, radius, maxPct, not validSample) and validSample;
      end;

      Inc(iSample);

    until not validSample;

    if hasOutFile then
    begin
      SetLength(samples, iSample);
      CreateWAV(1, 16, FSampleRate, FOutputWAVFileName, samples);
    end;

    if not FSilent then
    begin
      cnt := 0;
      for iSample := Low(FUsedSampleValues) to High(FUsedSampleValues) do
        Inc(cnt, Ord(FUsedSampleValues[iSample]));
      WriteLn('UsedSampleValues:', cnt:8);

      WriteLn('Done!');
    end;
  finally
    fltSample.Free;
  end;
end;

procedure TScan2Track.LoadScan;
begin
  Scan.LoadImage;
  Scan.FindTrack(False, FSampleRate);
end;

end.

