unit scan2track;

{$include 'compileroptions.inc'}

interface

uses
  Classes, SysUtils, Types, Math, Graphics, GraphType, FPCanvas, FPImage, FPWritePNG, MTProcs,
  utils, inputscan, profiles, FilterIIRLPBessel, FilterIIRHPBessel;

const
  CDecoderSigma = 2.0;

type
  TScan2Track = class;

  TSampleEvent = function(Sender: TScan2Track; ScanIdx: Integer; Sample: TPointD; X, Y: Double; var Radius: Double; Percent: Double; Finished: Boolean): Boolean of object;

  { TScan2Track }

  TScan2Track = class
  private
    FProfileRef: TProfile;
    FInputScans: TInputScanDynArray;
    FScansOwned: Boolean;

    FOutputWAVFileName: String;
    FOnSample: TSampleEvent;

    FDecoderPrecision: Integer;
    FSampleRate: Integer;
    FPointsPerRevolution: Integer;
    FRadiansPerRevolutionPoint: Double;
    FSilent: Boolean;

    FUsedSampleValues: array[SmallInt] of Boolean;

    FStartAngle: Double;
    FDPI: Integer;
    FRadiuses: TDoubleDynArray;

    FPrevSamples: TDoubleDynArray;
    FPrevSamplesPos, FPrevSamplesCount: Integer;
    FPrevSamplesMeanAcc, FPrevSamplesStdDevAcc: Double;

    procedure Init(AProfileRef: TProfile; ASilent: Boolean; ASampleRate: Integer; ADecoderPrecision: Integer);
    procedure FindTrackStart;
    function DecodeSample(AScan: TInputScan; radius, prevRadius, angleSin, angleCos: Double): TPointD;

  public
    constructor Create(AProfileRef: TProfile; AScanFileNames: TStrings; ADefaultDPI: Integer = 2400; ASilent: Boolean = False; ASampleRate: Integer = 48000; ADecoderPrecision: Integer = 6);
    constructor CreateFromInputScans(AProfileRef: TProfile; AInputScans: TInputScanDynArray; ASilent: Boolean; ASampleRate: Integer; ADecoderPrecision: Integer);
    destructor Destroy; override;

    procedure LoadScans;
    procedure EvalTrack;

    property OnSample: TSampleEvent read FOnSample write FOnSample;
    property OutputWAVFileName: String read FOutputWAVFileName write FOutputWAVFileName;

    property ProfileRef: TProfile read FProfileRef;
    property InputScans: TInputScanDynArray read FInputScans;
    property SampleRate: Integer read FSampleRate;
    property DecoderPrecision: Integer read FDecoderPrecision;
    property PointsPerRevolution: Integer read FPointsPerRevolution;
    property RadiansPerRevolutionPoint: Double read FRadiansPerRevolutionPoint;
  end;

implementation

{ TScan2Track }

constructor TScan2Track.Create(AProfileRef: TProfile; AScanFileNames: TStrings; ADefaultDPI: Integer; ASilent: Boolean;
  ASampleRate: Integer; ADecoderPrecision: Integer);
var
  iScan: Integer;
begin
  Init(AProfileRef, ASilent, ASampleRate, ADecoderPrecision);

  SetLength(FInputScans, AScanFileNames.Count);
  for iScan := 0 to High(FInputScans) do
  begin
    FInputScans[iScan] := TInputScan.Create(AProfileRef, ADefaultDPI, True);
    FInputScans[iScan].ImageFileName := AScanFileNames[iScan];
  end;

  FScansOwned := True;
end;

constructor TScan2Track.CreateFromInputScans(AProfileRef: TProfile; AInputScans: TInputScanDynArray; ASilent: Boolean;
  ASampleRate: Integer; ADecoderPrecision: Integer);
begin
  Init(AProfileRef, ASilent, ASampleRate, ADecoderPrecision);

  FInputScans := AInputScans;
end;

destructor TScan2Track.Destroy;
var
  iScan: Integer;
begin
  if FScansOwned then
    for iScan := 0 to High(FInputScans) do
      FInputScans[iScan].Free;

  inherited Destroy;
end;

procedure TScan2Track.Init(AProfileRef: TProfile; ASilent: Boolean; ASampleRate: Integer; ADecoderPrecision: Integer);
begin
  FProfileRef := AProfileRef;
  FSampleRate := ASampleRate;
  FDecoderPrecision := EnsureRange(ADecoderPrecision, 1, 16);
  FSilent := ASilent;

  FPointsPerRevolution := Round(FSampleRate / FProfileRef.RevolutionsPerSecond);
  FRadiansPerRevolutionPoint := -Pi * 2.0 / FPointsPerRevolution;

  SetLength(FPrevSamples, Round(FSampleRate / CLowCutoffFreq) shl (FDecoderPrecision + 1));
end;

function CompareInputScansStartAngleQuality(Item1, Item2, UserParameter: Pointer): Integer;
var
  s1: ^TInputScan absolute Item1;
  s2: ^TInputScan absolute Item2;
begin
  Result := CompareValue(s2^.GrooveStartAngleQuality, s1^.GrooveStartAngleQuality);
end;

procedure TScan2Track.FindTrackStart;
var
  iScan: Integer;
  startAngleBest: Double;
  scan: TInputScan;
begin
  SetLength(FRadiuses, Length(FInputScans));

  FStartAngle := NaN;
  startAngleBest := -Infinity;
  FDPI := -1;
  for iScan := 0 to High(FInputScans) do
  begin
    scan := FInputScans[iScan];

    if FDPI < 0 then
      FDPI := scan.DPI
    else
      Assert(scan.DPI = FDPI);

    FRadiuses[iScan] := scan.SetDownRadius;

    if scan.GrooveStartAngleQuality >= startAngleBest then
    begin
      startAngleBest := scan.GrooveStartAngleQuality;
      FStartAngle := scan.GrooveStartAngle;
    end;
  end;

  Assert(not IsNan(FStartAngle));

  if Length(FInputScans) > 1 then
  begin
    QuickSort(FInputScans[0], 0, High(FInputScans), SizeOf(TInputScan), @CompareInputScansStartAngleQuality);
    Writeln('Best start angle: ', FInputScans[0].ImageShortName);
  end;
end;

function TScan2Track.DecodeSample(AScan: TInputScan; radius, prevRadius, angleSin, angleCos: Double): TPointD;
var
  iSmp, posMin, posMax, decoderMax: Integer;
  r, cx, cy, px, py, cxa, sampleMean, sampleStdDev: Double;

  function GetSampleIdx(iSmp: Integer): Double;
  begin
    r := radius + (iSmp + 0.5) * cxa;
    Result := AScan.GetPointD_Final(AScan.Image, angleSin * r + cy, angleCos * r + cx);

    if FPrevSamplesCount >= Length(FPrevSamples) then
    begin
      FPrevSamplesStdDevAcc -= Sqr(FPrevSamplesMeanAcc / FPrevSamplesCount - FPrevSamples[FPrevSamplesPos]);
      FPrevSamplesMeanAcc -= FPrevSamples[FPrevSamplesPos];
    end;

    FPrevSamples[FPrevSamplesPos] := Result;

    Inc(FPrevSamplesPos);
    if FPrevSamplesPos >= Length(FPrevSamples) then
      FPrevSamplesPos := 0;
    if FPrevSamplesCount <= Length(FPrevSamples) then
      Inc(FPrevSamplesCount);

    FPrevSamplesMeanAcc += Result;
    FPrevSamplesStdDevAcc += Sqr(FPrevSamplesMeanAcc / FPrevSamplesCount - Result);
  end;

  function ConvertToSampleValue(ASampleIdxAcc: Double): Double;
  begin
    Result := ASampleIdxAcc / decoderMax;
    Result := DivDef(Result - sampleMean, sampleStdDev * CDecoderSigma, 0.0);
  end;

begin
  Result.X := 0.0;
  Result.Y := 0.0;

  decoderMax := 1 shl FDecoderPrecision;

  cxa := FProfileRef.RecordingGrooveWidth * 0.5 * AScan.DPI / decoderMax;

  cx := AScan.Center.X;
  cy := AScan.Center.Y;

  r := radius + decoderMax * cxa;
  px := angleCos * r + cx;
  py := angleSin * r + cy;

  if not AScan.InRangePointD(py, px) then
    Exit;

  if not IsNan(prevRadius) then
    decoderMax := EnsureRange(Floor((prevRadius - radius) * 0.5 * CTrack2TrackToTrackWidthRatio / cxa), decoderMax shr 2, decoderMax);

  posMin := -decoderMax;
  posMax := decoderMax - 1;
  sampleMean := Infinity;
  sampleStdDev := -Infinity;

  Result.X := 0.0;
  for iSmp := 0 to posMax do
    Result.X += GetSampleIdx(iSmp);

  Result.Y := 0.0;
  for iSmp := posMin to -1 do
    Result.Y += GetSampleIdx(iSmp);

  sampleMean := FPrevSamplesMeanAcc / FPrevSamplesCount;
  sampleStdDev := Sqrt(FPrevSamplesStdDevAcc / FPrevSamplesCount);

  Result.X := ConvertToSampleValue(Result.X);
  Result.Y := ConvertToSampleValue(Result.Y);

  Result.Y := -Result.Y;
end;

function CompareSamples(Item1, Item2, UserParameter: Pointer): Integer;
begin
  Result := CompareValue(PDouble(Item1)^, PDouble(Item2)^);
end;

procedure TScan2Track.EvalTrack;

  function GetMono(const ASample: TPointD): Double;
  begin
    Result := (ASample.X + ASample.Y) * 0.5;
  end;

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

var
  rOuter, sn, cs, ox, oy, fbRatio, instantPct, maxPct, angle: Double;
  iScan, iSample, iLut, cnt, validSmpCnt, channels: Integer;
  hasOutFile, validSample: Boolean;
  smp, validSmpAcc, filteredSmp: TPointD;
  fltSampleL, fltSampleR: TFilterIIRHPBessel;
  sinCosLut: TSinCosDynArray;
  fSmps: TPointDDynArray;
  samples: TDoubleDynArray;
  prevRadiuses: TDoubleDynArray2;
  scan: TInputScan;
begin
  if not FSilent then
    WriteLn('EvalTrack');

  hasOutFile := Trim(FOutputWAVFileName) <> '';
  if hasOutFile then
    SetLength(samples, FSampleRate * 2);
  fltSampleL := TFilterIIRHPBessel.Create(nil);
  fltSampleR := TFilterIIRHPBessel.Create(nil);
  try
    fltSampleL.FreqCut1 := CLowCutoffFreq;
    fltSampleL.SampleRate := FSampleRate;
    fltSampleL.Order := 4;

    fltSampleR.FreqCut1 := CLowCutoffFreq;
    fltSampleR.SampleRate := FSampleRate;
    fltSampleR.Order := 4;

    BuildSinCosLUT(FPointsPerRevolution, sinCosLut, FStartAngle, -2.0 * Pi);

    SetLength(fSmps, Length(FInputScans));

    SetLength(prevRadiuses, FPointsPerRevolution, Length(FInputScans));
    for iLut := 0 to High(prevRadiuses) do
      for iScan := 0 to High(FInputScans) do
        prevRadiuses[iLut, iScan] := NaN;

    fbRatio := CutoffToFeedbackRatio(CLoopbackLowCutoffFreq, FSampleRate) * FProfileRef.RecordingGrooveWidth * 0.5 * FDPI * CDecoderSigma;

    maxPct := 0.0;
    rOuter := FProfileRef.OuterSize * 0.5 * FDPI;
    iSample := 0;
    iLut := 0;

    repeat
      angle := NormalizeAngle(iLut * FRadiansPerRevolutionPoint + FStartAngle);
      cs := sinCosLut[iLut].Cos;
      sn := sinCosLut[iLut].Sin;

      validSmpAcc.X := 0.0;
      validSmpAcc.Y := 0.0;
      validSmpCnt := 0;
      for iScan := 0 to High(FInputScans) do
      begin
        scan := FInputScans[iScan];

        if (Length(FInputScans) > 1) and
            (InNormalizedAngle(angle, scan.CropData.StartAngle, scan.CropData.EndAngle) or
            InNormalizedAngle(angle, scan.CropData.StartAngleMirror, scan.CropData.EndAngleMirror)) then
        begin
          fSmps[iScan].X := NaN;
          fSmps[iScan].Y := NaN;
        end
        else
        begin
          fSmps[iScan] := DecodeSample(scan, FRadiuses[iScan], prevRadiuses[iLut, iScan], sn, cs);
          validSmpAcc.X += fSmps[iScan].X;
          validSmpAcc.Y += fSmps[iScan].Y;
          Inc(validSmpCnt);
        end;
      end;

      smp.X := DivDef(validSmpAcc.X, validSmpCnt, 0.0);
      smp.Y := DivDef(validSmpAcc.Y, validSmpCnt, 0.0);

      for iScan := 0 to High(FInputScans) do
      begin
        if not IsNan(fSmps[iScan].X) then
          FRadiuses[iScan] += GetMono(fSmps[iScan]) * fbRatio
        else
          FRadiuses[iScan] += GetMono(smp) * fbRatio;
      end;

      FUsedSampleValues[Make16BitSample(smp.X)] := True;
      FUsedSampleValues[Make16BitSample(smp.Y)] := True;

      filteredSmp.X := fltSampleL.FilterFilter(smp.X);
      filteredSmp.Y := fltSampleR.FilterFilter(smp.Y);

      if hasOutFile then
        StoreSample(samples, filteredSmp, iSample, FProfileRef.Mono);

      validSample := True;
      for iScan := 0 to High(FInputScans) do
        validSample := validSample and
          InRange(FRadiuses[iScan], InputScans[iScan].ConcentricGrooveRadius, rOuter) and
          (IsNan(prevRadiuses[iLut, iScan]) or (FRadiuses[iScan] < prevRadiuses[iLut, iScan]));

      if Assigned(FOnSample) then
      begin
        for iScan := 0 to High(FInputScans) do
        begin
          ox := cs * FRadiuses[iScan];
          oy := sn * FRadiuses[iScan];

          instantPct := (FRadiuses[iScan] - InputScans[iScan].ConcentricGrooveRadius) / (InputScans[iScan].SetDownRadius - InputScans[iScan].ConcentricGrooveRadius);
          instantPct := EnsureRange(1.0 - instantPct, 0.0, 1.0) * 100.0;
          maxPct := Max(maxPct, instantPct);

          validSample := FOnSample(Self, iScan, filteredSmp, ox + InputScans[iScan].Center.X, oy + InputScans[iScan].Center.Y, FRadiuses[iScan], maxPct, not validSample) and validSample;
			  end;
			end;

      prevRadiuses[iLut] := Copy(FRadiuses);

      Inc(iSample);
      Inc(iLut);
      if iLut >= FPointsPerRevolution then
        iLut := 0;

    until not validSample;

    if hasOutFile then
    begin
      channels := IfThen(FProfileRef.Mono, 1, 2);
      SetLength(samples, iSample * channels);
      CreateWAV(channels, 16, FSampleRate, FOutputWAVFileName, samples);
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
    fltSampleL.Free;
    fltSampleR.Free;
  end;
end;

procedure TScan2Track.LoadScans;

  procedure DoOne(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    Scan: TInputScan;
  begin
    Scan := FInputScans[AIndex];

    Scan.FindTrack(False, FScansOwned, FSampleRate);
  end;

var
  iScan: Integer;
begin
  WriteLn('LoadScans');

  if FScansOwned then
    for iScan := 0 to High(FInputScans) do
      FInputScans[iScan].LoadImage;

  ProcThreadPool.DoParallelLocalProc(@DoOne, 0, High(FInputScans));

  FindTrackStart;
end;

end.

