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

    procedure Init(AProfileRef: TProfile; ASilent: Boolean; ASampleRate: Integer; ADecoderPrecision: Integer);
    procedure FindTrackStart;

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
    FInputScans[iScan] := TInputScan.Create(AProfileRef, ADefaultDPI, Length(FInputScans) > 1);
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

var
  rOuter, sn, cs, ox, oy, fbRatio, instantPct, maxPct, angle: Double;
  iScan, iSample, iLut, cnt, channels: Integer;
  hasOutFile, validSample, sampleCropped: Boolean;
  smp, sample, filteredSample, sampleMeanSD: TPointD;
  fltSampleL, fltSampleR: TFilterIIRHPBessel;
  sinCosLut: TSinCosDynArray;
  angleMeanSDLut: TPointDDynArray2;
  rawSamples: TPointDDynArray;
  radiusSamples: TDoubleDynArray;
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

    // build Sin/Cos LUT

    BuildSinCosLUT(FPointsPerRevolution, sinCosLut, FStartAngle, -2.0 * Pi);

    // build per angle Mean/StdDev LUT

    SetLength(angleMeanSDLut, FPointsPerRevolution, Length(FInputScans));
    for iLut := 0 to High(angleMeanSDLut) do
    begin
      angle := ArcTan2(sinCosLut[iLut].Sin, sinCosLut[iLut].Cos);

      for iScan := 0 to High(FInputScans) do
      begin
        scan := FInputScans[iScan];
        angleMeanSDLut[iLut, iScan] := scan.GetMeanSD(scan.Image,
            FProfileRef.LastMusicGroove * 0.5 * scan.DPI,
            FProfileRef.FirstMusicGroove * 0.5 * scan.DPI,
            NormalizeAngle(angle + FRadiansPerRevolutionPoint * 0.5),
            NormalizeAngle(angle - FRadiansPerRevolutionPoint * 0.5),
            CDecoderSigma);
      end;
    end;

    // init

    SetLength(rawSamples, Length(FInputScans));
    SetLength(radiusSamples, Length(FInputScans));

    SetLength(prevRadiuses, FPointsPerRevolution, Length(FInputScans));
    for iLut := 0 to High(prevRadiuses) do
      for iScan := 0 to High(FInputScans) do
        prevRadiuses[iLut, iScan] := NaN;

    fbRatio := CutoffToFeedbackRatio(CLoopbackLowCutoffFreq, FSampleRate) * FProfileRef.RecordingGrooveWidth * 0.5 * FDPI;

    maxPct := 0.0;
    rOuter := FProfileRef.OuterSize * 0.5 * FDPI;
    iSample := 0;
    iLut := 0;

    repeat
      // decode samples

      angle := NormalizeAngle(iLut * FRadiansPerRevolutionPoint + FStartAngle);
      cs := sinCosLut[iLut].Cos;
      sn := sinCosLut[iLut].Sin;

      for iScan := 0 to High(FInputScans) do
      begin
        scan := FInputScans[iScan];

        sampleCropped := (Length(FInputScans) > 1) and
            (InNormalizedAngle(angle, scan.CropData.StartAngle, scan.CropData.EndAngle) or
            InNormalizedAngle(angle, scan.CropData.StartAngleMirror, scan.CropData.EndAngleMirror));

        if not sampleCropped then
        begin
          rawSamples[iScan] := scan.DecodeSample(FDecoderPrecision, FRadiuses[iScan], prevRadiuses[iLut, iScan], sn, cs, sampleMeanSD);
          radiusSamples[iScan] := GetMono(AdjustSample(rawSamples[iScan], sampleMeanSD));
        end
        else
        begin
          rawSamples[iScan].X := NaN;
          rawSamples[iScan].Y := NaN;
          radiusSamples[iScan] := NaN;
        end;
      end;

      // advance radius using negative feedback

      for iScan := 0 to High(FInputScans) do
      begin
        scan := FInputScans[iScan];

        sampleCropped := IsNan(radiusSamples[iScan]);

        if not sampleCropped then
          FRadiuses[iScan] += radiusSamples[iScan] * fbRatio
        else
          FRadiuses[iScan] += MeanNan(radiusSamples) * fbRatio;
      end;

      // handle and store sample

      cnt := 0;
      sample.X := 0.0;
      sample.Y := 0.0;

      for iScan := 0 to High(FInputScans) do
      begin
        scan := FInputScans[iScan];

        sampleCropped := IsNan(rawSamples[iScan].X) or IsNan(rawSamples[iScan].Y);

        if not sampleCropped then
        begin
          smp := AdjustSample(rawSamples[iScan], angleMeanSDLut[iLut, iScan]);

          sample.X += smp.X;
          sample.Y += smp.Y;
          Inc(cnt);
        end;
      end;

      if cnt > 1 then
      begin
        sample.X /= cnt;
        sample.Y /= cnt;
      end;

      FUsedSampleValues[Make16BitSample(sample.X)] := True;
      FUsedSampleValues[Make16BitSample(sample.Y)] := True;

      filteredSample.X := fltSampleL.FilterFilter(sample.X);
      filteredSample.Y := fltSampleR.FilterFilter(sample.Y);

      if hasOutFile then
        StoreSample(samples, filteredSample, iSample, FProfileRef.Mono);

      // progression

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

          validSample := FOnSample(Self, iScan, filteredSample, ox + InputScans[iScan].Center.X, oy + InputScans[iScan].Center.Y, FRadiuses[iScan], maxPct, not validSample) and validSample;
			  end;
			end;

      // advance to next iteration

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

