unit scan2track;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Types, Math, Graphics, GraphType, FPCanvas, FPImage, FPWritePNG, minlbfgs, MTProcs,
  utils, fgl, powell, inputscan;

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

    function PowellEvalTracking(const arg: TVector; obj: Pointer): TScalar;
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

uses main, forms;

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

const
  CPredictionPointCount = 200;
  CPredictionRevolutionDiv = 720;
var
  pradius, dumx, dumy: Double;
  SinCosLut: array[0 .. CPredictionPointCount - 1] of record
    Sin, Cos: Double;
  end;

function TScan2Track.PowellEvalTracking(const arg: TVector; obj: Pointer): TScalar;
var
  i: Integer;
  r, x, predy, predx, fx: Double;
begin
  r := pradius;
  x := arg[0] * PointsPerRevolution / (CPredictionPointCount * CPredictionRevolutionDiv);

  Result := 0;
  for i := 0 to CPredictionPointCount - 1 do
  begin
    if InRange(r, Scan.ConcentricGrooveRadius, Scan.FirstGrooveRadius) and
       InRange(r, Scan.ConcentricGrooveRadius, Scan.FirstGrooveRadius) then
    begin
      predx := SinCosLut[i].Cos * r + Scan.Center.X;
      predy := SinCosLut[i].Sin * r + Scan.Center.Y;

      fx := Scan.GetPointD(predy, predx, isImage, imLinear);
      Result -= fx;

      //main.Form1.Image.Picture.Bitmap.Canvas.Pixels[round(predx * CReducFactor), round(predy * CReducFactor)] := clBlue;
    end;

    r += x;
  end;

  dumx := predx;
  dumy := predy;

  //for i := 0 to High(arg) do
  //  Write(arg[i]:12:6);
  //WriteLn(-Result:12:6);
end;

procedure BFGSEvalTracking(const arg: TVector; var func: Double; grad: TVector; obj: Pointer);
var
  Self: TScan2Track absolute obj;
  i: Integer;
  r, x, predy, predx, id, fx: Double;
begin
  x := arg[0];

  func := 0;
  grad[0] := 0;
  for i := 0 to CPredictionPointCount - 1 do
  begin
    id := i * Self.PointsPerRevolution / (CPredictionPointCount * CPredictionRevolutionDiv);

    r := pradius + id * x;

    if InRange(r, Self.Scan.ConcentricGrooveRadius, Self.Scan.FirstGrooveRadius) then
    begin
      predx := SinCosLut[i].Cos * r + Self.Scan.Center.X;
      predy := SinCosLut[i].Sin * r + Self.Scan.Center.Y;

      fx := Self.Scan.GetPointD(predy, predx, isImage, imLinear);
      func -= fx;
      grad[0] -= Self.Scan.GetPointD(predy, predx, isSobelX, imLinear) * (SinCosLut[i].Cos * id) +
                 Self.Scan.GetPointD(predy, predx, isSobelY, imLinear) * (SinCosLut[i].Sin * id);

      //main.Form1.Image.Picture.Bitmap.Canvas.Pixels[round(predx * CReducFactor), round(predy * CReducFactor)] := clBlue;
    end;
  end;

  dumx := predx;
  dumy := predy;

  //for i := 0 to High(arg) do
  //  Write(arg[i]:12:6);
  //WriteLn(-func:12:6);
end;

procedure BFGSEvalTracking_(const arg: TVector; var func: Double; grad: TVector; obj: Pointer);
const
  CH = 1e-8;
  CFCoeff: array[0 .. 7] of Double = (-1/280, 4/105, -1/5, 4/5, -4/5, 1/5, -4/105, 1/280);
  CXCoeff: array[0 .. 7] of Double = (4, 3, 2, 1, -1, -2, -3, -4);
var
  Self: TScan2Track absolute obj;
  ig, ic: Integer;
  x: TVector;
begin
  ig := 0;
  grad[ig] := 0;
  for ic := 0 to High(CFCoeff) do
  begin
    x := Copy(arg);
    x[ig] += CXCoeff[ic] * CH;
    grad[ig] += CFCoeff[ic] * Self.PowellEvalTracking(x, obj);
  end;
  grad[ig] /= CH;

  func := Self.PowellEvalTracking(arg, obj);

  //for ig := 0 to High(arg) do
  //  Write(arg[ig]:14:6);
  //WriteLn(func:14:6);
end;

procedure TScan2Track.EvalTrack;

  procedure Correct(angle, radius: Double; var radiusInc: Double);
  var
    ai, a, f: Double;
    i: Integer;
    x, g: TVector;
    state: MinLBFGSState;
    rep: MinLBFGSReport;
  begin
    SetLength(x, 1);
    SetLength(g, 1);

    x[0] := radiusInc;

    pradius := radius;

    ai := Pi * 2.0 / (CPredictionPointCount * CPredictionRevolutionDiv);
    a := angle;
    for i := 0 to CPredictionPointCount - 1 do
    begin
      SinCos(a, SinCosLut[i].Sin, SinCosLut[i].Cos);
      a -= ai;
    end;

{$if 1}
    PowellMinimize(@PowellEvalTracking, x, 1e-8, 1e-6, 0, MaxInt, Self);

    radiusInc := x[0];

    PowellEvalTracking(x, self);
{$else}
    MinLBFGSCreate(1, 1, x, state);
    MinLBFGSSetCond(state, 0, 0, 0, 0);

    while MinLBFGSIteration(state) do
      if State.NeedFG then
      begin
        //BFGSEvalTracking(State.X, state.F, state.G, Self);
        BFGSEvalTracking_(State.X, state.F, state.G, Self);
      end;

    MinLBFGSResults(state, x, rep);

    radiusInc := x[0];
    //BFGSEvalTracking(x, f, g, self);
    BFGSEvalTracking_(x, f, g, self);
{$ifend}

    main.MainForm.Image.Picture.Bitmap.Canvas.Pixels[round(dumx * CReducFactor), round(dumy * CReducFactor)] := clTeal;
  end;


var
  angle, radius, sn, cs, px, py, r, radiusInc, radiusIncSmoo, c8a, middleSmp: Double;
  i, pos, aboveCnt, aboveAcc: Integer;
  pbuf: specialize TFPGList<TPoint>;
  t, pt: QWord;
  ismp, smp:SmallInt;
  sf: TSimpleFilter;
  smpBuf: array[Low(ShortInt) .. High(ShortInt)] of Double;
  samples: TSmallIntDynArray;
begin
  WriteLn('EvalTrack');

  SetLength(samples, FSampleRate);
  sf := TSimpleFilter.Create(CLowCutoffFreq * 2.0 / FSampleRate, 4, False);
  pbuf := specialize TFPGList<TPoint>.Create;
  try
    pos := 0;
    pt := GetTickCount64;

    c8a := C45RpmMaxGrooveWidth * Scan.DPI / High(ShortInt);
    angle := Scan.GrooveStartAngle;
    radius := Scan.FirstGrooveRadius;
    radiusInc := -(Scan.DPI / C45RpmLeadInGroovesPerInch) / FPointsPerRevolution;

    repeat
      angle := Scan.GrooveStartAngle - FRadiansPerRevolutionPoint * pos;

      SinCos(angle, sn, cs);

      smp := 0;
      for ismp := Low(ShortInt) to high(ShortInt) do
      begin
        r := radius + ismp * c8a;

        px := cs * r + Self.Scan.Center.X;
        py := sn * r + Self.Scan.Center.Y;

        if Scan.InRangePointD(py, px) then
          smpBuf[ismp] := Scan.GetPointD(py, px, isImage, imHermite);
      end;

      middleSmp := (MinValue(smpBuf) + MaxValue(smpBuf)) * 0.5;

      aboveAcc := 0;
      aboveCnt := 0;
      for ismp := Low(ShortInt) to high(ShortInt) do
        if smpBuf[ismp] >= middleSmp then
        begin
          aboveAcc += ismp;
          Inc(aboveCnt);
        end;
      smp := EnsureRange(Round(aboveAcc / (High(ShortInt) * aboveCnt) * High(SmallInt)), Low(SmallInt), High(SmallInt));

      while pos >= Length(samples) do
        SetLength(samples, Ceil(Length(samples) * cPhi));
      samples[pos] := smp;

      px := cs * radius + Self.Scan.Center.X;
      py := sn * radius + Self.Scan.Center.Y;

      Correct(angle, radius, radiusInc);

      radiusIncSmoo := sf.ProcessSample(radiusInc);

      Write(pos:8, aboveCnt:4, #13);

      radius += radiusIncSmoo;
      Inc(pos);

////////////////////////
      t := GetTickCount64;
      pbuf.Add(Point(round(px * CReducFactor), round(py * CReducFactor)));
      if t - pt >= 4000 then
      begin

        for i := 0 to pbuf.Count - 1 do
          main.MainForm.Image.Picture.Bitmap.Canvas.Pixels[pbuf[i].X, pbuf[i].Y] := clLime;

        main.MainForm.HorzScrollBar.Position := pbuf.Last.X - main.MainForm.Width div 2;
        main.MainForm.VertScrollBar.Position := pbuf.Last.Y - main.MainForm.Height div 2;

        pbuf.Clear;

        Application.ProcessMessages;

        SetLength(samples, pos);
        CreateWAV(1, FBitsPerSample, FSampleRate, FOutputWAVFileName, samples);

        pt := GetTickCount64;
      end;
////////////////////////

    until not InRange(radius, Scan.ConcentricGrooveRadius, C45RpmOuterSize * 0.5 * Scan.DPI);

    SetLength(samples, pos);
    CreateWAV(1, FBitsPerSample, FSampleRate, FOutputWAVFileName, samples);
  finally
    pbuf.Free;
    sf.Free;
  end;
end;

procedure TScan2Track.Run;
begin
  Scan.Run;
  EvalTrack;
end;

end.

