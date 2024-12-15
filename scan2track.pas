unit scan2track;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Types, Math, Graphics, GraphType, FPCanvas, FPImage, FPWritePNG, MTProcs,
  utils, fgl, inputscan, FilterIIRLPBessel, FilterIIRHPBessel;

const
  CSampleDecoderBits = 12;
  CSampleDecoderMax = (1 shl (CSampleDecoderBits - 1)) - 1;

type

  { TScan2Track }

  TScan2Track = class
  private
    FOutputWAVFileName: String;

    FScan: TInputScan;
    FBitsPerSample: Integer;
    FSampleRate: Integer;
    FPointsPerRevolution: Integer;
    FRadiansPerRevolutionPoint: Double;

    FTrack: TIntegerDynArray;

    function EvalTrackGR(x: Double; Data: Pointer): Double;
  public
    constructor Create(ASampleRate: Integer = 48000; ABitsPerSample: Integer = 16; ADPI: Integer = 2400);
    destructor Destroy; override;

    procedure EvalTrack;

    procedure Run;

    property OutputWAVFileName: String read FOutputWAVFileName write FOutputWAVFileName;

    property Scan: TInputScan read FScan;
    property SampleRate: Integer read FSampleRate;
    property BitsPerSample: Integer read FBitsPerSample;
    property PointsPerRevolution: Integer read FPointsPerRevolution;
    property RadiansPerRevolutionPoint: Double read FRadiansPerRevolutionPoint;

    property Track: TIntegerDynArray read FTrack;
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

  FScan := TInputScan.Create(FPointsPerRevolution, ADPI);
end;

destructor TScan2Track.Destroy;
begin
  FScan.Free;

  inherited Destroy;
end;

function TScan2Track.EvalTrackGR(x: Double; Data: Pointer): Double;
var
  ismp, aboveCnt: Integer;
begin
  aboveCnt := 0;
  for ismp := -CSampleDecoderMax-1 to CSampleDecoderMax do
   if PDouble(data)[ismp] >= x then
     Inc(aboveCnt);

  Result := aboveCnt;
end;

procedure TScan2Track.EvalTrack;
var
  fltSamples: TFilterIIRHPBessel;
  samples: TSmallIntDynArray;

  function DecodeSample(radius, angle: Double; out feedback: Double): Double;
  var
    ismp, aboveCnt, aboveAcc: Integer;
    r, px, py, middleSmp, cxa, sn, cs: Double;
    smpBuf: array[-CSampleDecoderMax-1 .. CSampleDecoderMax] of Double;
  begin
    SinCos(angle, sn, cs);
    cxa := C45RpmMaxGrooveWidth * Scan.DPI / CSampleDecoderMax;

    for ismp := -CSampleDecoderMax-1 to CSampleDecoderMax do
    begin
      r := radius + ismp * cxa;

      px := cs * r + Scan.Center.X;
      py := sn * r + Scan.Center.Y;

      if Scan.InRangePointD(py, px) then
        smpBuf[ismp] := Scan.GetPointD(py, px, isImage)
      else
        smpBuf[ismp] := 0.0;
    end;

    middleSmp := (MinValue(smpBuf) + MaxValue(smpBuf)) * 0.5;

    aboveAcc := 0;
    aboveCnt := 0;
    for ismp := -CSampleDecoderMax-1 to CSampleDecoderMax do
      if smpBuf[ismp] >= middleSmp then
      begin
        aboveAcc += ismp;
        Inc(aboveCnt);
      end;

    Result := DivDef(aboveAcc, CSampleDecoderMax * aboveCnt, 0.0);

    feedback := Result * C45RpmMaxGrooveWidth * Scan.DPI;
  end;

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
  angle, radius, sn, cs, px, py, feedback, fbRatio, fsmp, ffSmp: Double;
  pos: Integer;
  pbuf: TPointFList;
  t, pt: QWord;
begin
  WriteLn('EvalTrack');

  SetLength(samples, FSampleRate);
  fltSamples := TFilterIIRHPBessel.Create(nil);
  pbuf := TPointFList.Create;
  try
    fltSamples.FreqCut1 := CLowCutoffFreq;
    fltSamples.SampleRate := FSampleRate;
    fltSamples.Order := 4;

    pos := 0;
    pt := GetTickCount64;

    angle := Scan.GrooveStartAngle;
    radius := Scan.FirstGrooveRadius;
    fbRatio := (CLowCutoffFreq / FSampleRate) / sqrt(0.1024 + sqr(CLowCutoffFreq / FSampleRate));

    repeat
      fsmp := DecodeSample(radius, angle, feedback);
      ffSmp := fltSamples.FilterFilter(fsmp);
      StoreSample(ffSmp, pos);

      radius += feedback * fbRatio;
      angle := Scan.GrooveStartAngle - FRadiansPerRevolutionPoint * pos;

      //WriteLn(feedback:9:6);
      SinCos(angle, sn, cs);
      px := cs * radius + Scan.Center.X;
      py := sn * radius + Scan.Center.Y;
      pbuf.Add(TPointF.Create(px, py));

      Inc(pos);

////////////////////////
      t := GetTickCount64;
      if t - pt >= 4000 then
      begin
        main.MainForm.DrawPoints(pbuf, clLime);

        SetLength(samples, pos);
        CreateWAV(1, FBitsPerSample, FSampleRate, FOutputWAVFileName, samples);

        pbuf.Clear;

        pt := GetTickCount64;
      end;
////////////////////////

    until not InRange(radius, Scan.ConcentricGrooveRadius, C45RpmOuterSize * 0.5 * Scan.DPI);

    SetLength(samples, pos);
    CreateWAV(1, FBitsPerSample, FSampleRate, FOutputWAVFileName, samples);
  finally
    pbuf.Free;
    fltSamples.Free;
  end;

  WriteLn('Done!');
end;

procedure TScan2Track.Run;
begin
  Scan.Run;
  EvalTrack;
end;

end.

