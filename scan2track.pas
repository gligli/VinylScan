unit scan2track;

{$include 'compileroptions.inc'}

interface

uses
  Classes, SysUtils, Types, Math, Graphics, GraphType, FPCanvas, FPImage, FPWritePNG, MTProcs,
  utils, inputscan, profiles, Filter, FilterIIRLPBessel, FilterIIRHPBessel, filter_rbj;

const
  CWavPrecision = 16;
  CRadiusLoopbackLowCutoffFreq = 100.0;
  CInvRadiusLoopbackLowCutoffFreq = 200.0;
  CLowCutoffFreq = 20.0;
  CTimeFormat = 'nn:ss.zzz';

type
  TDecodeGeometry = record
    LeftCvt, RightCvt: Double;
    PosMin, PosMax: Integer;
    TrackMin, TrackMax: Integer;
  end;

  TDecodeContext = record
    DecodeMax: Integer;
    FSBuf: TDoubleDynArray;
  end;

  TScan2Track = class;

  TSampleEvent = function(Sender: TScan2Track; Position, InvDecodePosition: TPointD; Percent: Double; Time: TTime; Finished: Boolean): Boolean of object;

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

    FTrackSinCosLut: TSinCosDDynArray;
    FTrack: TPointDDynArray;
    FAudio: TDoubleDynArray;

    FRiaaFilters: array[0..1, Boolean{stereo right?}] of TRbjEqFilter;

    procedure InitRiaa;
    procedure FreeRiaa;
    function FilterRiaa(ASample: TPointD): TPointD;

    function GetAudio(ATrackIdx: Integer): TPointD;
    procedure SetAudio(ATrackIdx: Integer; const ASample: TPointD);

    function GetDecodeGeometry(radius, invRadius, prevInvRadius: Double; decodeMax: Integer; forceGrooveWidth: Boolean): TDecodeGeometry;
    procedure InitDecodeContext(var context: TDecodeContext);
    procedure FreeDecodeContext(var context: TDecodeContext);

    function DecodeSample_Work(radius, invRadius, prevInvRadius, angleSin, angleCos: Double): Double;
    function DecodeSample_Final(var context: TDecodeContext; radius, invRadius, prevInvRadius, angleSin, angleCos: Double): TPointD;

    procedure EvalTrack;
    procedure DecodeAudio;
    procedure FilterAudio;
    procedure NormalizeAudio;
  public
    constructor Create(AProfileRef: TProfile; AScanFileName: String; ADefaultDPI: Integer = 2400; ASampleRate: Integer = 48000; ADecoderPrecision: Integer = 7);
    destructor Destroy; override;

    procedure LoadScan;
    procedure Process;
    procedure Save;

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

constructor TScan2Track.Create(AProfileRef: TProfile; AScanFileName: String; ADefaultDPI: Integer; ASampleRate: Integer; ADecoderPrecision: Integer);
begin
  FProfileRef := AProfileRef;
  FSampleRate := Max(8000, ASampleRate);
  FDecoderPrecision := EnsureRange(ADecoderPrecision, 1, CWavPrecision);

  FPointsPerRevolution := Round(FSampleRate / FProfileRef.RevolutionsPerSecond);
  FRadiansPerRevolutionPoint := -Pi * 2.0 / FPointsPerRevolution;

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

procedure TScan2Track.InitDecodeContext(var context: TDecodeContext);
begin
  context.DecodeMax := 1 shl (Min(CWavPrecision, FDecoderPrecision shl 1) - Ord(FProfileRef.Mono));
  SetLength(context.FSBuf, (context.DecodeMax shl 1) + 1);
end;

procedure TScan2Track.FreeDecodeContext(var context: TDecodeContext);
begin
  SetLength(context.FSBuf, 0);
end;

function TScan2Track.DecodeSample_Work(radius, invRadius, prevInvRadius, angleSin, angleCos: Double): Double;
var
  iSmp, decodeMax: Integer;
  r, cx, cy, sampleMin, sampleMax, sample: Double;
  acc: TPointD;
  geometry: TDecodeGeometry;
begin
  // init

  Result := 0.0;
  cx := FInputScan.Center.X;
  cy := FInputScan.Center.Y;
  decodeMax := 1 shl FDecoderPrecision;
  geometry := GetDecodeGeometry(radius, invRadius, prevInvRadius, decodeMax, True);

  // ensure decoding is not OOB of image

  r := radius + decodeMax * geometry.LeftCvt;
  if not FInputScan.InRangePointD(angleSin * r + cy, angleCos * r + cx) then
    Exit;

  // image depth based decoding

  sampleMin := Infinity;
  sampleMax := FInputScan.GetPointD_Work(FInputScan.ProcessedImage, angleSin * radius + cy, angleCos * radius + cx);

  acc.X := 0.0;
  acc.Y := 0.0;
  for iSmp := 1 to decodeMax do
  begin
    r := radius + iSmp * geometry.LeftCvt;
    sample := FInputScan.GetPointD_Work(FInputScan.ProcessedImage, angleSin * r + cy, angleCos * r + cx);
    sampleMin := Min(sampleMin, sample);
    sampleMax := Max(sampleMax, sample);
    acc.X += sample;

    r := radius - iSmp * geometry.RightCvt;
    sample := FInputScan.GetPointD_Work(FInputScan.ProcessedImage, angleSin * r + cy, angleCos * r + cx);
    sampleMin := Min(sampleMin, sample);
    sampleMax := Max(sampleMax, sample);
    acc.Y += sample;
  end;
  acc.X /= decodeMax;
  acc.Y /= decodeMax;

  // finish decode

  acc.X := DivDef(acc.X - sampleMin, sampleMax - sampleMin, 0.5);
  acc.Y := DivDef(acc.Y - sampleMin, sampleMax - sampleMin, 0.5);

  Result := acc.X - acc.Y;
end;

function TScan2Track.DecodeSample_Final(var context: TDecodeContext; radius, invRadius, prevInvRadius, angleSin,
  angleCos: Double): TPointD;
var
  iSmp: Integer;
  r, cx, cy, sample, sampleMiddle, sampleMin, sampleMax, newValue, quantError, cvtLerpFactor: Double;
  geometry: TDecodeGeometry;
  pDec: PDouble;
  rt, up: Boolean;
  stats: array[Boolean] of Integer;
begin
  // init

  Result.X := 0.0;
  Result.Y := 0.0;
  cx := FInputScan.Center.X;
  cy := FInputScan.Center.Y;
  geometry := GetDecodeGeometry(radius, invRadius, prevInvRadius, context.DecodeMax, False);
  cvtLerpFactor := 1.0 / (context.DecodeMax shl 1);

  // ensure decoding is not OOB of image

  r := radius + context.DecodeMax * geometry.LeftCvt;
  if not FInputScan.InRangePointD(angleSin * r + cy, angleCos * r + cx) then
    Exit;

  // gather raw image samples

  sampleMin := Infinity;
  sampleMax := -Infinity;

  pDec := @context.FSBuf[0];
  for iSmp := geometry.PosMin to geometry.PosMax do
  begin
    rt := iSmp < 0;

    r := radius + (iSmp + 0.5) * lerp(geometry.RightCvt, geometry.LeftCvt, (iSmp - geometry.PosMin) * cvtLerpFactor);
    sample := FInputScan.GetPointD_Final(FInputScan.ProcessedImage, angleSin * r + cy, angleCos * r + cx);

    if InRange(iSmp, geometry.TrackMin, geometry.TrackMax) then
    begin
      sampleMin := Min(sampleMin, sample);
      sampleMax := Max(sampleMax, sample);
    end;

    pDec^ := sample;
    Inc(pDec);
  end;
  sampleMiddle := (sampleMin + sampleMax) * 0.5;

  // threshold-based decoding with noise shaping

  FillChar(stats, SizeOf(stats), 0);
  pDec := @context.FSBuf[0];
  for iSmp := geometry.PosMin to geometry.PosMax do
  begin
    rt := iSmp < 0;
    sample := pDec^;
    up := sample >= sampleMiddle;

    // 1D noise shaping algorithm
    newValue := sampleMin + Ord(up) * (sampleMax - sampleMin);
    quantError := sample - newValue;
    pDec^ := newValue;
    pDec[1] += quantError;

    if up then
      Inc(stats[rt]);

    Inc(pDec);
  end;

  // finish decode

  if FProfileRef.Mono then
  begin
    Result.X := (stats[False] - stats[True]) / context.DecodeMax;
    Result.Y := Result.X;
  end
  else
  begin
    Result.X := stats[False] / context.DecodeMax * 2.0 - 1.0;
    Result.Y := -stats[True] / context.DecodeMax * 2.0 - 1.0;
  end;
end;

procedure TScan2Track.EvalTrack;

  procedure StoreTrack(fsmp: TPointD; pos: Integer);
  begin
    while pos >= Length(FTrack) do
      SetLength(FTrack, Ceil(Length(FTrack) * cPhi) + 1);
    FTrack[pos] := fsmp;
  end;

var
  iSample, iLut: Integer;
  rOuter, sn, cs, fbRatio, invFbRatio, instantPct, maxPct, grooveRadius, radius, invRadius, invSample, rawSample: Double;
  validSample: Boolean;
  trackSample: TPointD;
  pxPos, invPxPos: TPointD;
  prevRadiuses: TPointDDynArray;
begin
  WriteLn('EvalTrack');

  // init

  trackSample.X := NaN;
  trackSample.Y := NaN;
  SetLength(FTrack, FSampleRate * 2);

  SetLength(prevRadiuses, FPointsPerRevolution);
  for iLut := 0 to High(prevRadiuses) do
  begin
    prevRadiuses[iLut].X := NaN;
    prevRadiuses[iLut].Y := NaN;
  end;

  grooveRadius := FProfileRef.RecordingGrooveWidth * 0.5 * FInputScan.DPI;
  fbRatio := CutoffToFeedbackRatio(CRadiusLoopbackLowCutoffFreq, FSampleRate) * grooveRadius;
  invFbRatio := CutoffToFeedbackRatio(CInvRadiusLoopbackLowCutoffFreq, FSampleRate) * grooveRadius;

  maxPct := 0.0;
  rOuter := FProfileRef.OuterSize * 0.5 * FInputScan.DPI;
  radius := FInputScan.SetDownRadius;
  invRadius := radius - FProfileRef.RecordingGrooveWidth * 0.5 * FInputScan.DPI;
  iSample := 0;
  iLut := 0;

  repeat
    // decode sample & advance radius using negative feedback

    cs := FTrackSinCosLut[iLut].Cos;
    sn := FTrackSinCosLut[iLut].Sin;

    invSample := DecodeSample_Work(invRadius, NaN, NaN, sn, cs);
    invRadius -= invSample * invFbRatio;
    if not IsNan(prevRadiuses[iLut].X) then
      invRadius := Max(radius - (prevRadiuses[iLut].X - radius), invRadius);

    rawSample := DecodeSample_Work(radius, invRadius, prevRadiuses[iLut].Y, sn, cs);
    radius += rawSample * fbRatio;

    trackSample.X := radius;
    trackSample.Y := invRadius;
    StoreTrack(trackSample, iSample);

    // progression

    validSample :=
        InRange(radius, FInputScan.ConcentricGrooveRadius, rOuter) and
        InRange(invRadius, FInputScan.ConcentricGrooveRadius, rOuter) and
        (IsNan(prevRadiuses[iLut].X) or (prevRadiuses[iLut].X > radius)) and
        (IsNan(prevRadiuses[iLut].X) or (prevRadiuses[iLut].X > invRadius)) and
        (radius >= invRadius);

    if Assigned(FOnSample) then
    begin
      pxPos.X := cs * radius + FInputScan.Center.X;
      pxPos.Y := sn * radius + FInputScan.Center.Y;

      invPxPos.X := cs * invRadius + FInputScan.Center.X;
      invPxPos.Y := sn * invRadius + FInputScan.Center.Y;

      instantPct := (radius - FInputScan.ConcentricGrooveRadius) / (FInputScan.SetDownRadius - FInputScan.ConcentricGrooveRadius);
      instantPct := EnsureRange(1.0 - instantPct, 0.0, 1.0) * 100.0;
      maxPct := Max(maxPct, instantPct);

      validSample := FOnSample(Self, pxPos, invPxPos, maxPct, iSample / (FSampleRate * SecsPerDay), not validSample) and validSample;
		end;

    // advance to next iteration

    prevRadiuses[iLut] := trackSample;

    Inc(iSample);
    Inc(iLut);
    if iLut >= FPointsPerRevolution then
      iLut := 0;

  until not validSample;

  SetLength(FTrack, iSample);
end;

function TScan2Track.GetAudio(ATrackIdx: Integer): TPointD;
begin
  if FProfileRef.Mono then
  begin
    Result.X := FAudio[ATrackIdx];
    Result.Y := Result.X;
  end
  else
  begin
    Result.X := FAudio[ATrackIdx * 2 + 0];
    Result.Y := FAudio[ATrackIdx * 2 + 1];
  end;
end;

procedure TScan2Track.SetAudio(ATrackIdx: Integer; const ASample: TPointD);
begin
  if FProfileRef.Mono then
  begin
    FAudio[ATrackIdx] := GetMono(ASample);
  end
  else
  begin
    FAudio[ATrackIdx * 2 + 0] := ASample.X;
    FAudio[ATrackIdx * 2 + 1] := ASample.Y;
  end;
end;

function TScan2Track.GetDecodeGeometry(radius, invRadius, prevInvRadius: Double; decodeMax: Integer; forceGrooveWidth: Boolean): TDecodeGeometry;
var
  grooveWidth, pxExtents: Double;
begin
  Result.PosMin := -decodeMax;
  Result.PosMax := decodeMax - 1;
  grooveWidth := FProfileRef.RecordingGrooveWidth * 0.5 * FInputScan.DPI;

  Result.RightCvt := grooveWidth / decodeMax;
  Result.TrackMin := Result.PosMin;
  if not IsNan(invRadius) then
  begin
    pxExtents := Max(0.0, radius - invRadius);
    if forceGrooveWidth then
      pxExtents := Min(pxExtents, grooveWidth);

    Result.RightCvt := pxExtents / decodeMax;
    if pxExtents <> 0.0 then
      Result.TrackMin := round(grooveWidth / pxExtents * Result.PosMin);
  end;

  Result.LeftCvt := Result.RightCvt;
  Result.TrackMax := Result.PosMax;
  if not IsNan(prevInvRadius) then
  begin
    pxExtents := Max(0.0, prevInvRadius - radius);
    if forceGrooveWidth then
      pxExtents := Min(pxExtents, grooveWidth);

    Result.LeftCvt := pxExtents / decodeMax;
    if pxExtents <> 0.0 then
      Result.TrackMax := round(grooveWidth / pxExtents * Result.PosMax);
  end;
end;

procedure TScan2Track.DecodeAudio;
var
  lock: TSpinlock;
  blockSize, blockCount, samplesDone: Integer;
  startTime: QWord;

  procedure DoBlock(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    iTrack, trackStart, trackEnd: Integer;
    prevInvRadius: Double;
    sample: TPointD;
    context: TDecodeContext;
    sncs: ^TSinCosD;
  begin
    if not InRange(AIndex, 0, blockCount - 1) then
      Exit;

    InitDecodeContext(context);
    try
      trackStart := AIndex * blockSize;
      trackEnd := Min((AIndex + 1) * blockSize - 1, High(FTrack));

      for iTrack := trackStart to trackEnd do
      begin
        sncs := @FTrackSinCosLut[iTrack mod FPointsPerRevolution];
        prevInvRadius := NaN;
        if iTrack >= FPointsPerRevolution then
          prevInvRadius := FTrack[iTrack - FPointsPerRevolution].Y;
        sample := DecodeSample_Final(context, FTrack[iTrack].X, FTrack[iTrack].Y, prevInvRadius, sncs^.Sin, sncs^.Cos);
        SetAudio(iTrack, sample);
      end;

      SpinEnter(@lock);
      try
        samplesDone += trackEnd - trackStart + 1;
        Write(FormatDateTime(CTimeFormat, samplesDone / (FSampleRate * SecsPerDay)), ' / ',
              FormatDateTime(CTimeFormat, Length(FTrack) / (FSampleRate * SecsPerDay)), ',',
              DivDef(samplesDone / FSampleRate, (GetTickCount64 - startTime) * 0.001, 0.0):6:3, 'x',
              #13);
      finally
        SpinLeave(@lock);
      end;
    finally
      FreeDecodeContext(context);
    end;

  end;

begin
  WriteLn('DecodeAudio');

  SpinLeave(@lock);
  samplesDone := 0;
  blockSize := FSampleRate div 100;
  blockCount := (Length(FTrack) - 1) div blockSize + 1;
  SetLength(FAudio, Length(FTrack) * IfThen(FProfileRef.Mono, 1, 2));

  startTime := GetTickCount64;
  ProcThreadPool.DoParallelLocalProc(@DoBlock, 0, blockCount - 1);
  WriteLn;
end;

procedure TScan2Track.FilterAudio;

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
  iTrack: Integer;
  grooveRadius, loSample: Double;
  sample: TPointD;
  fltSampleL, fltSampleR: TFilterIIRHPBessel;
  fltXHL, fltXHR: TFilterIIRHPBessel;
  fltXL: TFilterIIRLPBessel;
begin
  WriteLn('FilterAudio');

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

    fltXHL.FreqCut1 := CRadiusLoopbackLowCutoffFreq;
    fltXHL.SampleRate := FSampleRate;
    fltXHL.Order := 4;
    fltXHR.FreqCut1 := CRadiusLoopbackLowCutoffFreq;
    fltXHR.SampleRate := FSampleRate;
    fltXHR.Order := 4;
    fltXL.FreqCut1 := CRadiusLoopbackLowCutoffFreq;
    fltXL.SampleRate := FSampleRate;
    fltXL.Order := 4;

    grooveRadius := FProfileRef.RecordingGrooveWidth * 0.5 * FInputScan.DPI;

    for iTrack := 0 to High(FTrack) do
    begin
      sample := GetAudio(iTrack);

      loSample := FilterAndStuff(fltXL, FTrack[iTrack].X / grooveRadius, iTrack);
      sample.X := loSample + FilterAndStuff(fltXHL, sample.X, iTrack);
      sample.Y := loSample + FilterAndStuff(fltXHR, sample.Y, iTrack);

      sample.X := FilterAndStuff(fltSampleL, sample.X, iTrack);
      sample.Y := FilterAndStuff(fltSampleR, sample.Y, iTrack);

      sample := FilterRiaa(sample);

      SetAudio(iTrack, sample);
    end;
  finally
    fltSampleL.Free;
    fltSampleR.Free;
    fltXHL.Free;
    fltXHR.Free;
    fltXL.Free;
  end;
end;

procedure TScan2Track.NormalizeAudio;
var
  iAudio: Integer;
  maxSample: Double;
begin
  WriteLn('NormalizeAudio');

  maxSample := -Infinity;
  for iAudio := 0 to High(FAudio) do
    maxSample := Max(maxSample, Abs(FAudio[iAudio]));
  maxSample := DivDef(1.0, maxSample, 1.0);
  for iAudio := 0 to High(FAudio) do
    FAudio[iAudio] *= maxSample;
end;

procedure TScan2Track.Process;
begin
  EvalTrack;
  DecodeAudio;
  FilterAudio;
  NormalizeAudio;
end;

procedure TScan2Track.Save;
var
  channels: Integer;
begin
  WriteLn('Saving ', FOutputWAVFileName);

  channels := IfThen(FProfileRef.Mono, 1, 2);

  CreateWAV(channels, CWavPrecision, FSampleRate, FOutputWAVFileName, FAudio);

  WriteLn('Done!');
end;

procedure TScan2Track.LoadScan;
begin
  WriteLn('LoadScan');

  FInputScan.LoadImage;
  FInputScan.FindTrack(False, False, FSampleRate);

  BuildSinCosLUT(FPointsPerRevolution, FTrackSinCosLut, FInputScan.GrooveStartAngle, -2.0 * Pi);
end;

end.

