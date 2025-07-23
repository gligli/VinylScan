unit scan2track;

{$include 'compileroptions.inc'}

interface

uses
  Classes, SysUtils, Types, Math, Graphics, GraphType, FPCanvas, FPImage, FPWritePNG, MTProcs,
  utils, inputscan, FilterIIRLPBessel, FilterIIRHPBessel;

type
  TScan2Track = class;

  TSampleEvent = function(Sender: TScan2Track; ScanIdx: Integer; Sample, X, Y: Double; var Radius: Double; Percent: Double; Finished: Boolean): Boolean of object;

  { TScan2Track }

  TScan2Track = class
  private
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

    function DecodeSample(AScan: TInputScan; radius, angleSin, angleCos: Double; precision: Integer): Double;

  public
    constructor Create(AImageFileName: String; ADefaultDPI: Integer = 2400; ASilent: Boolean = False; ASampleRate: Integer = 48000; ADecoderPrecision: Integer = 8);
    constructor CreateFromInputScans(AInputScans: TInputScanDynArray; ASilent: Boolean; ASampleRate: Integer; ADecoderPrecision: Integer);
    destructor Destroy; override;

    procedure LoadScans;
    procedure EvalTrack;

    property OnSample: TSampleEvent read FOnSample write FOnSample;
    property OutputWAVFileName: String read FOutputWAVFileName write FOutputWAVFileName;

    property InputScans: TInputScanDynArray read FInputScans;
    property SampleRate: Integer read FSampleRate;
    property DecoderPrecision: Integer read FDecoderPrecision;
    property PointsPerRevolution: Integer read FPointsPerRevolution;
    property RadiansPerRevolutionPoint: Double read FRadiansPerRevolutionPoint;
  end;

implementation

{ TScan2Track }

constructor TScan2Track.Create(AImageFileName: String; ADefaultDPI: Integer; ASilent: Boolean; ASampleRate: Integer;
 ADecoderPrecision: Integer);
begin
  FSampleRate := ASampleRate;
  FDecoderPrecision := EnsureRange(ADecoderPrecision, 1, 16);
  FSilent := ASilent;

  FPointsPerRevolution := Round(FSampleRate / C45RpmRevolutionsPerSecond);
  FRadiansPerRevolutionPoint := -Pi * 2.0 / FPointsPerRevolution;

  SetLength(FInputScans, 1);
  FInputScans[0] := TInputScan.Create(ADefaultDPI);
  FInputScans[0].ImageFileName := AImageFileName;

  FScansOwned := True;
end;

constructor TScan2Track.CreateFromInputScans(AInputScans: TInputScanDynArray; ASilent: Boolean; ASampleRate: Integer;
 ADecoderPrecision: Integer);
begin
  FSampleRate := ASampleRate;
  FDecoderPrecision := EnsureRange(ADecoderPrecision, 1, 16);
  FSilent := ASilent;

  FPointsPerRevolution := Round(FSampleRate / C45RpmRevolutionsPerSecond);
  FRadiansPerRevolutionPoint := -Pi * 2.0 / FPointsPerRevolution;

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

function TScan2Track.DecodeSample(AScan: TInputScan; radius, angleSin, angleCos: Double; precision: Integer): Double;
var
  iSmp, posMin, posMax, decoderMax: Integer;
  r, cx, cy, px, py, cxa: Double;
  sample, sampleMiddle, sampleIdx: Single;
  smpBuf: array[SmallInt] of Single;
  idxCnt: array[Boolean] of Integer;
  idxAcc: array[Boolean] of Integer;
  up: Boolean;
begin
  decoderMax := 1 shl (precision - 1);

  cxa := C45RpmRecordingGrooveWidth * 0.5 * AScan.DPI / decoderMax;

  cx := AScan.Center.X;
  cy := AScan.Center.Y;

  r := radius + decoderMax * cxa;
  px := angleCos * r + cx;
  py := angleSin * r + cy;

  if not AScan.InRangePointD(py, px) then
    Exit(0.0);

  posMin := -decoderMax;
  posMax := decoderMax - 1;
  sampleMiddle := 0.0;
  for iSmp := posMin to posMax do
  begin
    r := radius + (iSmp + 0.5) * cxa;
    px := angleCos * r + cx;
    py := angleSin * r + cy;

    sample := AScan.GetPointD_Sinc(AScan.Image, py, px);

    sampleMiddle += sample;

    smpBuf[iSmp] := sample;
  end;

  sampleMiddle /= posMax - posMin + 1;

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

  sampleIdx := DivDef(idxAcc[True], idxCnt[True], 0.0) - DivDef(idxAcc[False], idxCnt[False], 0.0);

  Result := sampleIdx / decoderMax;
end;

function CompareSamples(Item1, Item2, UserParameter: Pointer): Integer;
begin
  Result := CompareValue(PDouble(Item1)^, PDouble(Item2)^);
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
  rOuter, sn, cs, ox, oy, fbRatio, ffSmp, instantPct, maxPct, fSmp, angle: Double;
  iScan, iSample, iLut, cnt, dpi, validScanCnt: Integer;
  hasOutFile, validSample: Boolean;
  fltSample: TFilterIIRHPBessel;
  sinCosLut: TSinCosDynArray;
  fSmps, validSmps: TDoubleDynArray;
  radiuses: TDoubleDynArray;
  scan: TInputScan;
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

    BuildSinCosLUT(FPointsPerRevolution, sinCosLut, FInputScans[0].GrooveStartAngle, -2.0 * Pi);

    SetLength(fSmps, Length(FInputScans));
    SetLength(validSmps, Length(FInputScans));
    SetLength(radiuses, Length(FInputScans));

    dpi := -1;
    for iScan := 0 to High(FInputScans) do
    begin
      scan := FInputScans[iScan];

      if dpi < 0 then
        dpi := scan.DPI
      else
        Assert(scan.DPI = dpi);

      radiuses[iScan] := scan.SetDownRadius;
    end;

    fbRatio := CutoffToFeedbackRatio(C45RpmLoopbackLowCutoffFreq, FSampleRate) * C45RpmRecordingGrooveWidth * 0.5 * dpi;

    maxPct := 0.0;
    rOuter := C45RpmOuterSize * 0.5 * dpi;
    iSample := 0;
    iLut := 0;

    repeat
      angle := NormalizeAngle(iLut * FRadiansPerRevolutionPoint + FInputScans[0].GrooveStartAngle);
      cs := sinCosLut[iLut].Cos;
      sn := sinCosLut[iLut].Sin;

      Inc(iLut);
      if iLut >= FPointsPerRevolution then
        iLut := 0;

      validScanCnt := 0;
      fSmp := 0;
      for iScan := 0 to High(FInputScans) do
      begin
        scan := FInputScans[iScan];

        if not FScansOwned and
           (InNormalizedAngle(angle, scan.CropData.StartAngle, scan.CropData.EndAngle) or
            InNormalizedAngle(angle, scan.CropData.StartAngleMirror, scan.CropData.EndAngleMirror)) then
        begin
          fSmps[iScan] := NaN;
        end
        else
        begin
          fSmps[iScan] := DecodeSample(scan, radiuses[iScan], sn, cs, FDecoderPrecision);
          validSmps[validScanCnt] := fSmps[iScan];
          Inc(validScanCnt);
        end;
      end;

      fSmp := Mean(@validSmps[0], validScanCnt);

      for iScan := 0 to High(FInputScans) do
      begin
        if not IsNan(fSmps[iScan]) then
          radiuses[iScan] += fSmps[iScan] * fbRatio
        else
          radiuses[iScan] += fSmp * fbRatio;
      end;

      FUsedSampleValues[Make16BitSample(fSmp)] := True;

      ffSmp := fltSample.FilterFilter(fSmp);

      if hasOutFile then
        StoreSample(samples, ffSmp, iSample);

      validSample := True;
      for iScan := 0 to High(FInputScans) do
        validSample := validSample and InRange(radiuses[iScan], InputScans[iScan].ConcentricGrooveRadius, rOuter);

      if Assigned(FOnSample) then
      begin
        for iScan := 0 to High(FInputScans) do
        begin
          ox := cs * radiuses[iScan];
          oy := sn * radiuses[iScan];

          instantPct := (radiuses[iScan] - InputScans[iScan].ConcentricGrooveRadius) / (InputScans[iScan].SetDownRadius - InputScans[iScan].ConcentricGrooveRadius);
          instantPct := EnsureRange(1.0 - instantPct, 0.0, 1.0) * 100.0;
          maxPct := Max(maxPct, instantPct);

          validSample := FOnSample(Self, iScan, ffSmp, ox + InputScans[iScan].Center.X, oy + InputScans[iScan].Center.Y, radiuses[iScan], maxPct, not validSample) and validSample;
			  end;
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

procedure TScan2Track.LoadScans;
var
  iScan: Integer;
begin
  for iScan := 0 to High(FInputScans) do
  begin
    if FScansOwned then
      InputScans[iScan].LoadImage;
    InputScans[iScan].FindTrack(False, FSampleRate);
  end;
end;

end.

