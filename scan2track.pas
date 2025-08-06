unit scan2track;

{$include 'compileroptions.inc'}

interface

uses
  Classes, SysUtils, Types, Math, Graphics, GraphType, FPCanvas, FPImage, FPWritePNG, MTProcs,
  utils, inputscan, FilterIIRLPBessel, FilterIIRHPBessel;

type
  TScan2Track = class;

  TSampleEvent = function(Sender: TScan2Track; ScanIdx: Integer; Sample: TPointD; X, Y: Double; var Radius: Double; Percent: Double; Finished: Boolean): Boolean of object;

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

    procedure Init(ASilent: Boolean; ASampleRate: Integer; ADecoderPrecision: Integer);
    function DecodeSample(AScan: TInputScan; radius, angleSin, angleCos: Double; iLut, precision: Integer): TPointD;

  public
    constructor Create(AScanFileNames: TStrings; ADefaultDPI: Integer = 2400; ASilent: Boolean = False; ASampleRate: Integer = 48000; ADecoderPrecision: Integer = 8);
    constructor CreateFromInputScans(AInputScans: TInputScanDynArray; ASilent: Boolean; ASampleRate: Integer; ADecoderPrecision: Integer);
    destructor Destroy; override;

    procedure LoadScans(AForceBrickwall: Boolean);
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

constructor TScan2Track.Create(AScanFileNames: TStrings; ADefaultDPI: Integer; ASilent: Boolean; ASampleRate: Integer;
  ADecoderPrecision: Integer);
var
  iScan: Integer;
begin
  Init(ASilent, ASampleRate, ADecoderPrecision);

  SetLength(FInputScans, AScanFileNames.Count);
  for iScan := 0 to High(FInputScans) do
  begin
    FInputScans[iScan] := TInputScan.Create(ADefaultDPI, True);
    FInputScans[iScan].ImageFileName := AScanFileNames[iScan];
  end;

  FScansOwned := True;
end;

constructor TScan2Track.CreateFromInputScans(AInputScans: TInputScanDynArray; ASilent: Boolean; ASampleRate: Integer;
 ADecoderPrecision: Integer);
begin
  Init(ASilent, ASampleRate, ADecoderPrecision);

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

procedure TScan2Track.Init(ASilent: Boolean; ASampleRate: Integer; ADecoderPrecision: Integer);
begin
  FSampleRate := ASampleRate;
  FDecoderPrecision := EnsureRange(ADecoderPrecision, 1, 16);
  FSilent := ASilent;

  FPointsPerRevolution := Round(FSampleRate / C45RpmRevolutionsPerSecond);
  FRadiansPerRevolutionPoint := -Pi * 2.0 / FPointsPerRevolution;
end;

function TScan2Track.DecodeSample(AScan: TInputScan; radius, angleSin, angleCos: Double; iLut, precision: Integer): TPointD;

var
  iSmp, posMin, posMax, decoderMax, radiusOffset: Integer;
  r, cx, cy, px, py, cxa, sampleMin, sampleMax: Double;

  function GetSampleIdx(iSmp: Integer): Double;
  begin
    r := radius + (iSmp + 0.5) * cxa;
    Result := AScan.GetPolarPointD(AScan.PolarImage, r - radiusOffset, iLut);
    sampleMin := Min(sampleMin, Result);
    sampleMax := Max(sampleMax, Result);
  end;

  function ConvertToSampleValue(ASampleIdxAcc: Double): Double;
  begin
    Result := ASampleIdxAcc / decoderMax;
    Result := DivDef((Result - sampleMin) * 2.0, sampleMax - sampleMin, 1.0) - 1.0;
  end;

begin
  Result.X := 0.0;
  Result.Y := 0.0;

  decoderMax := 1 shl (precision - 1);
  radiusOffset := Floor(C45RpmLabelOuterSize * 0.5 * AScan.DPI);

  cxa := C45RpmRecordingGrooveWidth * 0.5 * AScan.DPI / decoderMax;

  cx := AScan.Center.X;
  cy := AScan.Center.Y;

  r := radius + decoderMax * cxa;
  px := angleCos * r + cx;
  py := angleSin * r + cy;

  if not AScan.InRangePointD(py, px) then
    Exit;

  posMin := -decoderMax;
  posMax := decoderMax - 1;
  sampleMin := Infinity;
  sampleMax := -Infinity;

  Result.X := 0.0;
  for iSmp := 0 to posMax do
    Result.X += GetSampleIdx(iSmp);

  Result.Y := 0.0;
  for iSmp := posMin to -1 do
    Result.Y += GetSampleIdx(iSmp);

  Result.X := ConvertToSampleValue(Result.X);
  Result.Y := ConvertToSampleValue(Result.Y);

  Result.Y := -Result.Y;
end;

function CompareSamples(Item1, Item2, UserParameter: Pointer): Integer;
begin
  Result := CompareValue(PDouble(Item1)^, PDouble(Item2)^);
end;

procedure TScan2Track.EvalTrack;
var
  samples: TDoubleDynArray;

  procedure StoreSample(var samplesArray: TDoubleDynArray;  fsmp: TPointD; pos: Integer);
  begin
    while pos * 2 >= Length(samplesArray) do
      SetLength(samplesArray, Ceil(Length(samplesArray) * cPhi) + 2);
    samplesArray[pos * 2 + 0] := fsmp.X;
    samplesArray[pos * 2 + 1] := fsmp.Y;
  end;

  function GetMono(const ASample: TPointD): Double;
  begin
    Result := (ASample.X + ASample.Y) * 0.5;
  end;

var
  rOuter, sn, cs, ox, oy, fbRatio, instantPct, maxPct, angle: Double;
  iScan, iSample, iLut, cnt, dpi, validSmpCnt: Integer;
  hasOutFile, validSample: Boolean;
  smp, validSmpAcc, filteredSmp: TPointD;
  fltSampleL, fltSampleR: TFilterIIRHPBessel;
  sinCosLut: TSinCosDynArray;
  fSmps: TPointDDynArray;
  radiuses: TDoubleDynArray;
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

    BuildSinCosLUT(FPointsPerRevolution, sinCosLut, FInputScans[0].GrooveStartAngle, -2.0 * Pi);

    SetLength(fSmps, Length(FInputScans));
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
          fSmps[iScan] := DecodeSample(scan, radiuses[iScan], sn, cs, iLut, FDecoderPrecision);
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
          radiuses[iScan] += GetMono(fSmps[iScan]) * fbRatio
        else
          radiuses[iScan] += GetMono(smp) * fbRatio;
      end;

      FUsedSampleValues[Make16BitSample(smp.X)] := True;
      FUsedSampleValues[Make16BitSample(smp.Y)] := True;

      filteredSmp.X := fltSampleL.FilterFilter(smp.X);
      filteredSmp.Y := fltSampleR.FilterFilter(smp.Y);

      if hasOutFile then
        StoreSample(samples, filteredSmp, iSample);

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

          validSample := FOnSample(Self, iScan, filteredSmp, ox + InputScans[iScan].Center.X, oy + InputScans[iScan].Center.Y, radiuses[iScan], maxPct, not validSample) and validSample;
			  end;
			end;

      Inc(iSample);

    until not validSample;

    if hasOutFile then
    begin
      SetLength(samples, iSample * 2);
      CreateWAV(2, 16, FSampleRate, FOutputWAVFileName, samples);
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

procedure TScan2Track.LoadScans(AForceBrickwall: Boolean);

  procedure DoOne(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    Scan: TInputScan;
  begin
    Scan := FInputScans[AIndex];

    Scan.FindTrack(False, FSampleRate);

    if FScansOwned and (Length(InputScans) > 1) then
      scan.FindCroppedArea;

    scan.RenderPolarImage;
  end;

var
  iScan: Integer;
begin
  WriteLn('LoadScans');

  if FScansOwned then
    for iScan := 0 to High(FInputScans) do
      FInputScans[iScan].LoadImage;

  ProcThreadPool.DoParallelLocalProc(@DoOne, 0, High(FInputScans));
end;

end.

