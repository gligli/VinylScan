unit scan2track;

{$include 'compileroptions.inc'}

interface

uses
  Classes, SysUtils, Types, Math, Graphics, GraphType, FPCanvas, FPImage, FPWritePNG, MTProcs, fgl,
  utils, inputscan, powell, FilterIIRLPBessel, FilterIIRHPBessel;

type
  TSerpCoeffs9ByWord = array[0 .. high(Word)] of TSerpCoeffs9;

  TSampleParams = record
    serpCoeffsLUT: TSerpCoeffs9ByWord;
    radius, angleSin, angleCos: Double;
  end;

  { TScan2Track }

  TScan2Track = class
  private
    FOutputWAVFileName: String;

    FScan: TInputScan;
    FBitsPerSample: Integer;
    FSampleRate: Integer;
    FPointsPerRevolution: Integer;
    FRadiansPerRevolutionPoint: Double;

    FSampleParams: TSampleParams;

    function PowellSample(const arg: TVector; obj: Pointer): TScalar;
    procedure BuilsSerpCoeffsLUT(var coeffs: TSerpCoeffs9ByWord);
    procedure DecodeSample(var sample: Double; radius, angleSin, angleCos: Double);

  public
    constructor Create(ASampleRate: Integer = 48000; ABitsPerSample: Integer = 16; ADPI: Integer = 2400);
    destructor Destroy; override;

    procedure LoadPNG;
    procedure EvalTrack;

    property OutputWAVFileName: String read FOutputWAVFileName write FOutputWAVFileName;

    property Scan: TInputScan read FScan;
    property SampleRate: Integer read FSampleRate;
    property BitsPerSample: Integer read FBitsPerSample;
    property PointsPerRevolution: Integer read FPointsPerRevolution;
    property RadiansPerRevolutionPoint: Double read FRadiansPerRevolutionPoint;
  end;

implementation
uses main;

{ TScan2Track }

constructor TScan2Track.Create(ASampleRate: Integer; ABitsPerSample: Integer; ADPI: Integer);
begin
  FSampleRate := ASampleRate;
  FBitsPerSample := ABitsPerSample;

  FPointsPerRevolution := Round(FSampleRate / C45RpmRevolutionsPerSecond);
  FRadiansPerRevolutionPoint := Pi * 2.0 / FPointsPerRevolution;

  FScan := TInputScan.Create(ADPI);
end;

destructor TScan2Track.Destroy;
begin
  FScan.Free;

  inherited Destroy;
end;

procedure TScan2Track.BuilsSerpCoeffsLUT(var coeffs: TSerpCoeffs9ByWord);
var
  w: Integer;
begin
  for w := 0 to High(Word) do
    serpCoeffs(w * (1 / High(Word)), coeffs[w]);
end;

function TScan2Track.PowellSample(const arg: TVector; obj: Pointer): TScalar;
var
  ix, iy: Integer;
  r, px, py: Double;
  wx, wy: Word;
  intData: TSerpCoeffs9;
begin
  Result := 1000.0;

  if not InRange(arg[0], -1.0, 1.0) then
    Exit;

  r := FSampleParams.radius + arg[0] * C45RpmRecordingGrooveWidth * Scan.DPI;

  px := FSampleParams.angleCos * r + Scan.Center.X;
  py := FSampleParams.angleSin * r + Scan.Center.Y;

  if Scan.InRangePointD(py, px) then
  begin
    ix := trunc(px);
    iy := trunc(py);

    wx := Round((px - ix) * High(Word));
    wy := Round((py - iy) * High(Word));

    serpFromCoeffsXY(FSampleParams.serpCoeffsLUT[wx], Scan.Image, ix, iy, intData);

    Result := -serpFromCoeffs(FSampleParams.serpCoeffsLUT[wy], intData);
  end;
end;


procedure TScan2Track.DecodeSample(var sample: Double; radius, angleSin, angleCos: Double);
var
  x: TVector;
begin
  FSampleParams.angleCos := angleCos;
  FSampleParams.angleSin := angleSin;
  FSampleParams.radius := radius;

  SetLength(x, 1);
  x[0] := sample;
  PowellMinimize(@PowellSample, x, 1.0, 1e-6, 0.5, MaxInt);
  sample := x[0];
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
  radius, sn, cs, px, py, fbRatio, fsmp, ffSmp: Double;
  iSample, iLut: Integer;
  pbuf: TPointFList;
  t, pt: QWord;
  fltSample: TFilterIIRHPBessel;
  sinCosLut: TSinCosDynArray;
begin
  WriteLn('EvalTrack');

  SetLength(samples, FSampleRate);
  fltSample := TFilterIIRHPBessel.Create(nil);
  pbuf := TPointFList.Create;
  try
    fltSample.FreqCut1 := CLowCutoffFreq;
    fltSample.SampleRate := FSampleRate;
    fltSample.Order := 4;

    BuilsSerpCoeffsLUT(FSampleParams.serpCoeffsLUT);
    BuildSinCosLUT(FPointsPerRevolution, sinCosLut, Scan.GrooveStartAngle, -2.0 * Pi);

    pt := GetTickCount64;
    fbRatio := CutoffToFeedbackRatio(C45RpmLowCutoffFreq, FSampleRate) * C45RpmRecordingGrooveWidth * Scan.DPI;

    fsmp := 0.0;
    iSample := 0;
    iLut := 0;
    radius := Scan.FirstGrooveRadius;
    repeat
      cs := sinCosLut[iLut].Cos;
      sn := sinCosLut[iLut].Sin;

      DecodeSample(fsmp, radius, sn, cs);

      ffSmp := fltSample.FilterFilter(fsmp);
      StoreSample(ffSmp, iSample);

      radius += fsmp * fbRatio;

      px := cs * radius + Scan.Center.X;
      py := sn * radius + Scan.Center.Y;
      pbuf.Add(TPointF.Create(px, py));

      Inc(iSample);

      Inc(iLut);
      if iLut >= FPointsPerRevolution then
        iLut := 0;

////////////////////////
      t := GetTickCount64;
      if t - pt >= 1000 then
      begin
        main.MainForm.DrawPoints(pbuf, clLime);

        SetLength(samples, iSample);
        CreateWAV(1, 16, FSampleRate, FOutputWAVFileName, samples);

        pbuf.Clear;

        pt := GetTickCount64;
      end;
////////////////////////

    until not InRange(radius, Scan.ConcentricGrooveRadius, C45RpmOuterSize * 0.5 * Scan.DPI);

    main.MainForm.DrawPoints(pbuf, clLime);

    SetLength(samples, iSample);
    CreateWAV(1, 16, FSampleRate, FOutputWAVFileName, samples);
  finally
    pbuf.Free;
    fltSample.Free;
  end;

  WriteLn('Done!');
end;

procedure TScan2Track.LoadPNG;
begin
  Scan.LoadPNG;
  Scan.FindTrack(FSampleRate);
end;

end.

