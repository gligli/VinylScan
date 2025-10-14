unit scan2track;

{$include 'compileroptions.inc'}

interface

uses
  Classes, SysUtils, Types, Math, Graphics, GraphType, FPCanvas, FPImage, FPWritePNG, MTProcs,
  utils, inputscan, profiles, Filter, FilterIIRLPBessel, FilterIIRHPBessel;

const
  CLowCutoffFreq = 20.0;
  CTrebbleBoostFreq = 500.0;
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
  cx, cy, cvtSmpRadius: Double;

  function GetSample(ASmpPos: Double): Double;
  var
    r: Double;
  begin
    r := radius + ASmpPos * cvtSmpRadius;
    Result := FInputScan.GetPointD_Final(FInputScan.ProcessedImage, angleSin * r + cy, angleCos * r + cx);
  end;

var
  iSmp, decoderMax: Integer;
  r, px, py, md, mx, sample: Double;
begin
  Result.X := 0.0;
  Result.Y := 0.0;

  decoderMax := 1 shl FDecoderPrecision;

  cvtSmpRadius := FProfileRef.RecordingGrooveWidth * 0.5 * FInputScan.DPI / decoderMax;
  if not IsNan(prevRadius) then
    cvtSmpRadius := EnsureRange((prevRadius - radius) * 0.5 * CTrack2TrackToTrackWidthRatio / decoderMax, 0.0, cvtSmpRadius);

  cx := FInputScan.Center.X;
  cy := FInputScan.Center.Y;

  r := radius + decoderMax * cvtSmpRadius;
  px := angleCos * r + cx;
  py := angleSin * r + cy;

  if not FInputScan.InRangePointD(py, px) then
    Exit;

  mx := GetSample(0);

  for iSmp := 1 to decoderMax - 1 do
  begin
    sample := GetSample(iSmp);
    Result.X += sample;

    sample := GetSample(-iSmp);
    Result.Y += sample;
  end;

  Result.X /= decoderMax - 1;
  Result.Y /= decoderMax - 1;

  md := GetMono(Result);

  Result.X := (Result.X - md) / (mx * 0.5);
  Result.Y := (Result.Y - md) / (mx * 0.5);

  Result.X := EnsureRange(Result.X, -1.0, 1.0);
  Result.Y := EnsureRange(Result.Y, -1.0, 1.0);
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
  rOuter, sn, cs, instantPct, maxPct, grooveRadius: Double;
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

    fltXHL.FreqCut1 := CTrebbleBoostFreq;
    fltXHL.SampleRate := FSampleRate;
    fltXHL.Order := 4;
    fltXHR.FreqCut1 := CTrebbleBoostFreq;
    fltXHR.SampleRate := FSampleRate;
    fltXHR.Order := 4;
    fltXLL.FreqCut1 := CTrebbleBoostFreq;
    fltXLL.SampleRate := FSampleRate;
    fltXLL.Order := 4;
    fltXLR.FreqCut1 := CTrebbleBoostFreq;
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

      rawSample := DecodeSample(GetMono(radius), GetMono(prevRadiuses[iLut]), sn, cs);

      radius.X += rawSample.X * grooveRadius;
      radius.Y -= rawSample.Y * grooveRadius;

      sample.X := radius.X / grooveRadius;
      sample.Y := radius.Y / grooveRadius;

      sample.X := (FilterAndStuff(fltXLL, sample.X, iSample) + CTrebbleBoostAmp * FilterAndStuff(fltXHL, sample.X, iSample)) * 0.5;
      sample.Y := (FilterAndStuff(fltXLR, sample.Y, iSample) + CTrebbleBoostAmp * FilterAndStuff(fltXHR, sample.Y, iSample)) * 0.5;

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
          (IsNan(prevRadiuses[iLut].Y) or (prevRadiuses[iLut].Y > radius.Y));

      if Assigned(FOnSample) then
      begin
        leftPx.X := cs * radius.X + FInputScan.Center.X;
        leftPx.Y := sn * radius.X + FInputScan.Center.Y;

        rightPx.X := cs * radius.Y + FInputScan.Center.X;
        rightPx.Y := sn * radius.Y + FInputScan.Center.Y;

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
end;

end.

