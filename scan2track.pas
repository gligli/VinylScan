unit scan2track;

{$include 'compileroptions.inc'}

interface

uses
  Classes, SysUtils, Types, Math, Graphics, GraphType, FPCanvas, FPImage, FPWritePNG, MTProcs,
  utils, inputscan, profiles, Filter, FilterIIRLPBessel, FilterIIRHPBessel, filter_rbj;

const
  CLoopbackLowCutoffFreq = 150.0;
  CLowCutoffFreq = 20.0;
  CTrackWidthRatio = 0.7;

type
  TScan2Track = class;

  TSampleEvent = function(Sender: TScan2Track; Sample, Position: TPointD; Percent: Double; Finished: Boolean): Boolean of object;

  { TScan2Track }

  TScan2Track = class
  private
    FProfileRef: TProfile;
    FInputScan: TInputScan;

    FOutputWAVFileName: String;
    FOnSample: TSampleEvent;

    FDecoderPrecision: Integer;
    FSampleRate: Integer;
    FSilent: Boolean;
    FStatisticalDecodingSigma: Double;
    FUseStatisticalDecoding: Boolean;

    FPointsPerRevolution: Integer;
    FRadiansPerRevolutionPoint: Double;
    FDecodeMax: Integer;

    FDecodeBuf: TDoubleDynArray;
    FDecodeSinCosLut: TSinCosDDynArray;

    FRiaaFilters: array[0..1, Boolean{stereo right?}] of TRbjEqFilter;

    procedure InitRiaa;
    procedure FreeRiaa;
    function FilterRiaa(ASample: TPointD): TPointD;

    function DecodeSample(radius, prevRadius, angleSin, angleCos: Double; out AFeedback: Double): TPointD;
  public
    constructor Create(AProfileRef: TProfile; AScanFileName: String; ADefaultDPI: Integer = 2400; ASilent: Boolean = False; ASampleRate: Integer = 48000; ADecoderPrecision: Integer = 4);
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
    property UseStatisticalDecoding: Boolean read FUseStatisticalDecoding write FUseStatisticalDecoding;
    property StatisticalDecodingSigma: Double read FStatisticalDecodingSigma write FStatisticalDecodingSigma;
  end;

implementation

{ TScan2Track }

constructor TScan2Track.Create(AProfileRef: TProfile; AScanFileName: String; ADefaultDPI: Integer; ASilent: Boolean;
  ASampleRate: Integer; ADecoderPrecision: Integer);
begin
  FProfileRef := AProfileRef;
  FSampleRate := ASampleRate;
  FDecoderPrecision := EnsureRange(ADecoderPrecision, 1, 8);
  FSilent := ASilent;
  FUseStatisticalDecoding := False;
  FStatisticalDecodingSigma := 5.0;

  FPointsPerRevolution := Round(FSampleRate / FProfileRef.RevolutionsPerSecond);
  FRadiansPerRevolutionPoint := -Pi * 2.0 / FPointsPerRevolution;

  FDecodeMax := 1 shl (FDecoderPrecision - Ord(FProfileRef.Mono));
  SetLength(FDecodeBuf, Sqr((FDecodeMax shl 1) + 1));
  BuildSinCosLUT(FDecodeMax shl 1, FDecodeSinCosLut, FRadiansPerRevolutionPoint * 0.5, FRadiansPerRevolutionPoint);

  FInputScan := TInputScan.Create(AProfileRef, ADefaultDPI, False);
  FInputScan.ImageFileName := AScanFileName;

  InitRiaa;
end;

destructor TScan2Track.Destroy;
begin
  FreeRiaa;
  FInputScan.Free;

  inherited Destroy;
end;

procedure TScan2Track.InitRiaa;
var
  iFilter: Integer;
  stereoRight: Boolean;
begin
  for stereoRight := False to True do
  begin
    for iFilter := Low(FRiaaFilters) to High(FRiaaFilters) do
      FRiaaFilters[iFilter, stereoRight] := TRbjEqFilter.create(FSampleRate, 0);

    FRiaaFilters[0, stereoRight].CalcFilterCoeffs(kHighShelf, 1031.990, 0.467, 12.570, False);
    FRiaaFilters[1, stereoRight].CalcFilterCoeffs(kPeaking, 37794.908, 0.559, 6.130, False);
  end;
end;

procedure TScan2Track.FreeRiaa;
var
  iFilter: Integer;
  stereoRight: Boolean;
begin
  for iFilter := Low(FRiaaFilters) to High(FRiaaFilters) do
    for stereoRight := False to True do
      FRiaaFilters[iFilter, stereoRight].Free;
end;

function TScan2Track.FilterRiaa(ASample: TPointD): TPointD;
var
  iFilter: Integer;
begin
  Result := ASample;

  for iFilter := Low(FRiaaFilters) to High(FRiaaFilters) do
  begin
    Result.X := FRiaaFilters[iFilter, False].Process(Result.X);
    Result.Y := FRiaaFilters[iFilter, True].Process(Result.Y);
  end;
end;

function TScan2Track.DecodeSample(radius, prevRadius, angleSin, angleCos: Double; out AFeedback: Double): TPointD;
var
  iSmp, iAngle, angleStride, posMin, posMax: Integer;
  r, px, py, cx, cy, cs, sn, snDec, csDec, cvtSmpRadius: Double;
  sample, sampleMiddle, sampleStdDev, sampleMin, sampleMax, feedback, newValue, quantError: Double;
  pSnCs, pDec, pDec2: PDouble;
  rt, up: Boolean;
  stats: array[Boolean, Boolean] of record
    Acc: Double;
    Cnt: Integer;
  end;
begin
  // init

  Result.X := 0.0;
  Result.Y := 0.0;
  AFeedback := 0.0;
  cx := FInputScan.Center.X;
  cy := FInputScan.Center.Y;

  cvtSmpRadius := FProfileRef.RecordingGrooveWidth * 0.5 * FInputScan.DPI / FDecodeMax;
  if not IsNan(prevRadius) then
    cvtSmpRadius := EnsureRange((prevRadius - radius) * 0.5 * CTrackWidthRatio / FDecodeMax, 0.0, cvtSmpRadius);

  // ensure decoding is not OOB of image

  r := radius + FDecodeMax * cvtSmpRadius;
  px := angleCos * r + cx;
  py := angleSin * r + cy;
  if not FInputScan.InRangePointD(py, px) then
    Exit;

  // gather raw image samples

  posMin := -FDecodeMax;
  posMax := FDecodeMax - 1;
  pDec := @FDecodeBuf[0];
  sampleMiddle := 0.0;
  sampleMin := Infinity;
  sampleMax := -Infinity;
  for iSmp := posMin to posMax do
  begin
    r := radius + (iSmp + 0.5) * cvtSmpRadius;
    pSnCs := @FDecodeSinCosLut[0].Sin;

    for iAngle := 1 to Length(FDecodeSinCosLut) do
    begin
      snDec := pSnCs^; Inc(pSnCs);
      csDec := pSnCs^; Inc(pSnCs);

      cs := angleCos * csDec - angleSin * snDec;
      sn := angleSin * csDec + angleCos * snDec;

      sample := FInputScan.GetPointD_Final(FInputScan.ProcessedImage, sn * r + cy, cs * r + cx);

      pDec^ := sample;
      Inc(pDec);

      sampleMiddle += sample;
      sampleMin := Min(sampleMin, sample);
      sampleMax := Max(sampleMax, sample);
    end;
    Inc(pDec);
  end;

  // optionally devise middle/min/max from mean/standard deviation

  if FUseStatisticalDecoding then
  begin
    sampleMiddle /= Sqr(FDecodeMax shl 1);
    sampleStdDev := 0.0;
    pDec := @FDecodeBuf[0];
    for iSmp := posMin to posMax do
    begin
      for iAngle := 1 to Length(FDecodeSinCosLut) do
      begin
        sample := pDec^;
        sampleStdDev += Sqr(sample - sampleMiddle);
        Inc(pDec);
      end;
      Inc(pDec);
    end;
    sampleStdDev := Sqrt(sampleStdDev / Sqr(FDecodeMax shl 1)) * FStatisticalDecodingSigma;
    sampleMin := sampleMiddle - sampleStdDev;
    sampleMax := sampleMiddle + sampleStdDev;
  end
  else
  begin
    sampleMiddle := (sampleMin + sampleMax) * 0.5;
  end;

  // threshold-based decoding with Floyd-Steinberg dithering

  FillChar(stats, SizeOf(stats), 0);
  angleStride := Length(FDecodeSinCosLut) + 1;
  pDec := @FDecodeBuf[0];
  for iSmp := posMin to posMax do
  begin
    rt := iSmp < 0;

    for iAngle := 1 to Length(FDecodeSinCosLut) do
    begin
      sample := pDec^;

      up := sample >= sampleMiddle;

      // Floyd-Steinberg algorithm
      newValue := IfThen(up, sampleMax, sampleMin);
      quantError := sample - newValue;
      pDec^ := newValue;
      pDec[            + 1] += quantError * (7 / 16);
      pDec[angleStride - 1] += quantError * (3 / 16);
      pDec[angleStride    ] += quantError * (5 / 16);
      pDec[angleStride + 1] += quantError * (1 / 16);

      stats[rt, up].Acc += iSmp + 0.5;
      Inc(stats[rt, up].Cnt);

      Inc(pDec);
    end;
    Inc(pDec);
  end;

  // finish decode

  feedback := DivDef(stats[False, True].Acc + stats[True, True].Acc, stats[False, True].Cnt + stats[True, True].Cnt, 0.0);
  feedback := feedback / FDecodeMax;
  if not FProfileRef.Mono then
  begin
    Result.X := DivDef(stats[False, True].Acc, stats[False, True].Cnt, 0.0);
    Result.Y := DivDef(stats[True, True].Acc, stats[True, True].Cnt, 0.0);
    Result.X := Result.X / (FDecodeMax * 0.5);
    Result.Y := Result.Y / (FDecodeMax * 0.5);
  end
  else
  begin
    Result.X := feedback;
    Result.Y := feedback;
  end;
  AFeedback := feedback;
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
  rOuter, sn, cs, fbRatio, instantPct, maxPct, grooveRadius, maxSample, radius, feedback, loSample: Double;
  hasOutFile, validSample: Boolean;
  sample, rawSample: TPointD;
  pxPos: TPointD;
  prevRadiuses: TDoubleDynArray;
  sinCosLut: TSinCosDDynArray;
  samples: TDoubleDynArray;
  fltSampleL, fltSampleR: TFilterIIRHPBessel;
  fltXHL, fltXHR: TFilterIIRHPBessel;
  fltXL: TFilterIIRLPBessel;
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
  fltXL := TFilterIIRLPBessel.Create(nil);
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
    fltXL.FreqCut1 := CLoopbackLowCutoffFreq;
    fltXL.SampleRate := FSampleRate;
    fltXL.Order := 4;

    // build Sin/Cos LUT

    BuildSinCosLUT(FPointsPerRevolution, sinCosLut, FInputScan.GrooveStartAngle, -2.0 * Pi);

    // init

    sample.X := NaN;
    sample.Y := NaN;
    SetLength(prevRadiuses, FPointsPerRevolution);
    for iLut := 0 to High(prevRadiuses) do
      prevRadiuses[iLut] := NaN;

    grooveRadius := FProfileRef.RecordingGrooveWidth * 0.5 * FInputScan.DPI;
    fbRatio := CutoffToFeedbackRatio(CLoopbackLowCutoffFreq, FSampleRate) * grooveRadius;

    maxPct := 0.0;
    rOuter := FProfileRef.OuterSize * 0.5 * FInputScan.DPI;
    radius := FInputScan.SetDownRadius;
    iSample := 0;
    iLut := 0;

    repeat
      // decode sample

      cs := sinCosLut[iLut].Cos;
      sn := sinCosLut[iLut].Sin;

      rawSample := DecodeSample(radius, prevRadiuses[iLut], sn, cs, feedback);

      radius += feedback * fbRatio;

      loSample := FilterAndStuff(fltXL, radius / grooveRadius, iSample);

      sample.X := loSample + FilterAndStuff(fltXHL, rawSample.X, iSample);
      sample.Y := loSample + FilterAndStuff(fltXHR, rawSample.Y, iSample);

      // handle and store sample

      sample.X := FilterAndStuff(fltSampleL, sample.X, iSample);
      sample.Y := FilterAndStuff(fltSampleR, sample.Y, iSample);

      sample := FilterRiaa(sample);

      if hasOutFile then
        StoreSample(samples, sample, iSample, FProfileRef.Mono);

      // progression

      validSample :=
          InRange(radius, FInputScan.ConcentricGrooveRadius, rOuter) and
          (IsNan(prevRadiuses[iLut]) or (prevRadiuses[iLut] > radius));

      if Assigned(FOnSample) then
      begin
        pxPos.X := cs * radius + FInputScan.Center.X;
        pxPos.Y := sn * radius + FInputScan.Center.Y;

        instantPct := (radius - FInputScan.ConcentricGrooveRadius) / (FInputScan.SetDownRadius - FInputScan.ConcentricGrooveRadius);
        instantPct := EnsureRange(1.0 - instantPct, 0.0, 1.0) * 100.0;
        maxPct := Max(maxPct, instantPct);

        validSample := FOnSample(Self, sample, pxPos, maxPct, not validSample) and validSample;
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
    fltXHL.Free;
    fltXHR.Free;
    fltXL.Free;
  end;
end;

procedure TScan2Track.LoadScan;
begin
  WriteLn('LoadScan');

  FInputScan.LoadImage;
  FInputScan.FindTrack(False, False, FSampleRate);
end;

end.

