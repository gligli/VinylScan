unit scan2track;

{$include 'compileroptions.inc'}

interface

uses
  Classes, SysUtils, Types, Math, Graphics, GraphType, FPCanvas, FPImage, FPWritePNG, MTProcs,
  utils, inputscan, profiles, Filter, FilterIIRLPBessel, FilterIIRHPBessel;

const
  CLoopbackLowCutoffFreq = 500.0;
  CLowCutoffFreq = 20.0;
  CTrebbleBoostAmp = 4.5;
  CTrack2TrackToTrackWidthRatio = 0.7;

type
  TScan2Track = class;

  TSampleEvent = function(Sender: TScan2Track; Sample, LeftPx, RightPx: TPointD; Percent: Double; Finished: Boolean): Boolean of object;

  { TScan2Track }

  TScan2Track = class
  private
    FProfileRef: TProfile;
    FInputScan: TInputScan;

    FOutputWAVFileName: String;
    FOnSample: TSampleEvent;

    FDecoderPrecision: Integer;
    FSampleRate: Integer;
    FPointsPerRevolution: Integer;
    FRadiansPerRevolutionPoint: Double;
    FSilent: Boolean;

    function DecodeSample(radius, prevRadius, angleSin, angleCos: Double): TPointD;

  public
    constructor Create(AProfileRef: TProfile; AScanFileName: String; ADefaultDPI: Integer = 2400; ASilent: Boolean = False; ASampleRate: Integer = 48000; ADecoderPrecision: Integer = 6);
    destructor Destroy; override;

    procedure LoadScan;
    procedure EvalTrack;

    property OnSample: TSampleEvent read FOnSample write FOnSample;
    property OutputWAVFileName: String read FOutputWAVFileName write FOutputWAVFileName;

    property ProfileRef: TProfile read FProfileRef;
    property InputScan: TInputScan read FInputScan;
    property SampleRate: Integer read FSampleRate;
    property DecoderPrecision: Integer read FDecoderPrecision;
    property PointsPerRevolution: Integer read FPointsPerRevolution;
    property RadiansPerRevolutionPoint: Double read FRadiansPerRevolutionPoint;
  end;

implementation

{ TScan2Track }

constructor TScan2Track.Create(AProfileRef: TProfile; AScanFileName: String; ADefaultDPI: Integer; ASilent: Boolean;
  ASampleRate: Integer; ADecoderPrecision: Integer);
begin
  FProfileRef := AProfileRef;
  FSampleRate := ASampleRate;
  FDecoderPrecision := EnsureRange(ADecoderPrecision, 1, 16);
  FSilent := ASilent;

  FPointsPerRevolution := Round(FSampleRate / FProfileRef.RevolutionsPerSecond);
  FRadiansPerRevolutionPoint := -Pi * 2.0 / FPointsPerRevolution;

  FInputScan := TInputScan.Create(AProfileRef, ADefaultDPI, False);
  FInputScan.ImageFileName := AScanFileName;
end;

destructor TScan2Track.Destroy;
begin
  FInputScan.Free;

  inherited Destroy;
end;

function TScan2Track.DecodeSample(radius, prevRadius, angleSin, angleCos: Double): TPointD;
var
  iSmp, posMin, posMax, decoderMax: Integer;
  r, cx, cy, skx, sky, px, py, cvtSmpRadius, sum: Double;

  function GetSampleIdx(iSmp: Integer): Double;
  var
    r: Double;
  begin
    r := radius + (iSmp + 0.5) * cvtSmpRadius;
    Result := FInputScan.GetPointD_Final(FInputScan.ProcessedImage, (angleSin * r + cy) * sky, (angleCos * r + cx) * skx);
  end;

begin
  Result.X := 0.0;
  Result.Y := 0.0;

  decoderMax := 1 shl FDecoderPrecision;

  cvtSmpRadius := FProfileRef.RecordingGrooveWidth * 0.5 * FInputScan.DPI / decoderMax;
  if not IsNan(prevRadius) then
    cvtSmpRadius := EnsureRange((prevRadius - radius) * 0.5 * CTrack2TrackToTrackWidthRatio / decoderMax, 0.0, cvtSmpRadius);

  cx := FInputScan.Center.X;
  cy := FInputScan.Center.Y;
  skx := FInputScan.Skew.X;
  sky := FInputScan.Skew.Y;

  r := radius + decoderMax * cvtSmpRadius;
  px := (angleCos * r + cx) * skx;
  py := (angleSin * r + cy) * sky;

  if not FInputScan.InRangePointD(py, px) then
    Exit;

  posMin := -decoderMax;
  posMax := decoderMax - 1;

  Result.X := 0.0;
  for iSmp := 0 to posMax do
    Result.X += GetSampleIdx(iSmp);

  Result.Y := 0.0;
  for iSmp := posMin to -1 do
    Result.Y += GetSampleIdx(iSmp);

  sum := GetMono(Result);

  Result.X -= sum;
  Result.Y -= sum;

  Result.X /= decoderMax * -Low(SmallInt);
  Result.Y /= -decoderMax * -Low(SmallInt);
end;

procedure TScan2Track.EvalTrack;

  procedure StoreSample(var samplesArray: TDoubleDynArray; fsmp: TPointD; pos: Integer; mono: Boolean);
  begin
    if mono then
    begin
      while pos >= Length(samplesArray) do
        SetLength(samplesArray, Ceil(Length(samplesArray) * cPhi) + 1);
      samplesArray[pos] := GetMono(fsmp);
    end
    else
    begin
      while pos * 2 >= Length(samplesArray) do
        SetLength(samplesArray, Ceil(Length(samplesArray) * cPhi) + 2);
      samplesArray[pos * 2 + 0] := fsmp.X;
      samplesArray[pos * 2 + 1] := fsmp.Y;
    end;
  end;

  function FilterAndStuff(AFilter: TFilter; ASample: Double; ASampleIdx: Integer): Double;
  var
    iFilterStuffing: Integer;
  begin
    if ASampleIdx = 0 then
      for iFilterStuffing := 1 to FSampleRate do
        AFilter.FilterFilter(ASample);

    Result := AFilter.FilterFilter(ASample);
  end;

var
  iSample, iLut, channels: Integer;
  rOuter, sn, cs, fbRatio, instantPct, maxPct, grooveRadius, maxSample: Double;
  hasOutFile, validSample: Boolean;
  sample, rawSample, filteredSample: TPointD;
  radius: TPointD;
  leftPx, rightPx: TPointD;
  prevRadiuses: TPointDDynArray;
  sinCosLut: TSinCosDynArray;
  samples: TDoubleDynArray;
  fltSampleL, fltSampleR: TFilterIIRHPBessel;
  fltXHL, fltXHR: TFilterIIRHPBessel;
  fltXLL, fltXLR: TFilterIIRLPBessel;
begin
  if not FSilent then
    WriteLn('EvalTrack');

  hasOutFile := Trim(FOutputWAVFileName) <> '';
  if hasOutFile then
    SetLength(samples, FSampleRate * 2);
  fltSampleL := TFilterIIRHPBessel.Create(nil);
  fltSampleR := TFilterIIRHPBessel.Create(nil);
  fltXHL := TFilterIIRHPBessel.Create(nil);
  fltXHR := TFilterIIRHPBessel.Create(nil);
  fltXLL := TFilterIIRLPBessel.Create(nil);
  fltXLR := TFilterIIRLPBessel.Create(nil);
  try
    fltSampleL.FreqCut1 := CLowCutoffFreq;
    fltSampleL.SampleRate := FSampleRate;
    fltSampleL.Order := 4;
    fltSampleR.FreqCut1 := CLowCutoffFreq;
    fltSampleR.SampleRate := FSampleRate;
    fltSampleR.Order := 4;

    fltXHL.FreqCut1 := CLoopbackLowCutoffFreq;
    fltXHL.SampleRate := FSampleRate;
    fltXHL.Order := 4;
    fltXHR.FreqCut1 := CLoopbackLowCutoffFreq;
    fltXHR.SampleRate := FSampleRate;
    fltXHR.Order := 4;
    fltXLL.FreqCut1 := CLoopbackLowCutoffFreq;
    fltXLL.SampleRate := FSampleRate;
    fltXLL.Order := 4;
    fltXLR.FreqCut1 := CLoopbackLowCutoffFreq;
    fltXLR.SampleRate := FSampleRate;
    fltXLR.Order := 4;

    // build Sin/Cos LUT

    BuildSinCosLUT(FPointsPerRevolution, sinCosLut, FInputScan.GrooveStartAngle, -2.0 * Pi);

    // init

    sample.X := NaN;
    sample.Y := NaN;
    SetLength(prevRadiuses, FPointsPerRevolution);
    for iLut := 0 to High(prevRadiuses) do
    begin
      prevRadiuses[iLut].X := NaN;
      prevRadiuses[iLut].Y := NaN;
    end;

    grooveRadius := FProfileRef.RecordingGrooveWidth * 0.5 * FInputScan.DPI;
    fbRatio := CutoffToFeedbackRatio(CLoopbackLowCutoffFreq, FSampleRate) * grooveRadius;

    maxPct := 0.0;
    rOuter := FProfileRef.OuterSize * 0.5 * FInputScan.DPI;
    radius.X := FInputScan.SetDownRadius + FProfileRef.RecordingGrooveThickness * 0.5 * FInputScan.DPI;
    radius.Y := FInputScan.SetDownRadius - FProfileRef.RecordingGrooveThickness * 0.5 * FInputScan.DPI;
    iSample := 0;
    iLut := 0;

    repeat
      // decode sample

      cs := sinCosLut[iLut].Cos;
      sn := sinCosLut[iLut].Sin;

      rawSample := DecodeSample(GetMono(radius), prevRadiuses[iLut].Y, sn, cs);

      radius.X += rawSample.X * fbRatio;
      radius.Y += rawSample.Y * fbRatio;

      sample.X := FilterAndStuff(fltXLL, radius.X / grooveRadius, iSample) + CTrebbleBoostAmp * FilterAndStuff(fltXHL, rawSample.X, iSample);
      sample.Y := FilterAndStuff(fltXLR, radius.Y / grooveRadius, iSample) + CTrebbleBoostAmp * FilterAndStuff(fltXHR, rawSample.Y, iSample);

      // handle and store sample

      filteredSample.X := FilterAndStuff(fltSampleL, sample.X, iSample);
      filteredSample.Y := FilterAndStuff(fltSampleR, sample.Y, iSample);

      if hasOutFile then
        StoreSample(samples, filteredSample, iSample, FProfileRef.Mono);

      // progression

      validSample :=
          InRange(radius.X, FInputScan.ConcentricGrooveRadius, rOuter) and
          InRange(radius.Y, FInputScan.ConcentricGrooveRadius, rOuter) and
          (IsNan(prevRadiuses[iLut].Y) or (prevRadiuses[iLut].Y > radius.X)) and
          (radius.X >= radius.Y);

      if Assigned(FOnSample) then
      begin
        leftPx.X := (cs * radius.X + FInputScan.Center.X) * FInputScan.Skew.X;
        leftPx.Y := (sn * radius.X + FInputScan.Center.Y) * FInputScan.Skew.Y;

        rightPx.X := (cs * radius.Y + FInputScan.Center.X) * FInputScan.Skew.X;
        rightPx.Y := (sn * radius.Y + FInputScan.Center.Y) * FInputScan.Skew.Y;

        instantPct := (GetMono(radius) - FInputScan.ConcentricGrooveRadius) / (FInputScan.SetDownRadius - FInputScan.ConcentricGrooveRadius);
        instantPct := EnsureRange(1.0 - instantPct, 0.0, 1.0) * 100.0;
        maxPct := Max(maxPct, instantPct);

        validSample := FOnSample(Self, filteredSample, leftPx, rightPx, maxPct, not validSample) and validSample;
			end;

      // advance to next iteration

      prevRadiuses[iLut] := radius;

      Inc(iSample);
      Inc(iLut);
      if iLut >= FPointsPerRevolution then
        iLut := 0;

    until not validSample;

    if hasOutFile then
    begin
      channels := IfThen(FProfileRef.Mono, 1, 2);
      SetLength(samples, iSample * channels);

      // normalize samples
      maxSample := -Infinity;
      for iSample := 0 to High(samples) do
        maxSample := Max(maxSample, Abs(samples[iSample]));
      maxSample := DivDef(1.0, maxSample, 1.0);
      for iSample := 0 to High(samples) do
        samples[iSample] *= maxSample;

      CreateWAV(channels, 16, FSampleRate, FOutputWAVFileName, samples);
    end;

    if not FSilent then
      WriteLn('Done!');
  finally
    fltSampleL.Free;
    fltSampleR.Free;
  end;
end;

procedure TScan2Track.LoadScan;
begin
  WriteLn('LoadScan');

  FInputScan.LoadImage;
  FInputScan.FindTrack(False, False, FSampleRate);
  FInputScan.Linearize;
end;

end.

