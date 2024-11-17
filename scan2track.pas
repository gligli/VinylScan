unit scan2track;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Types, Math, Graphics, GraphType, FPCanvas, FPImage, FPWritePNG,
  utils, bufstream, fgl, powell, minasa, minlbfgs, inputscan;

type

  { TScan2Track }

  TScan2Track = class
  private
    FScan: TInputScan;
    FBitsPerSample: Integer;
    FSampleRate: Integer;
    FPointsPerRevolution: Integer;
    FRadiansPerRevolutionPoint: Double;

    FTrack: TIntegerDynArray;
  public
    constructor Create(ASampleRate: Integer = 48000; ABitsPerSample: Integer = 16; ADPI: Integer = 2400);
    destructor Destroy; override;

    procedure EvalTrack;

    procedure Run;

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
  CPredictionPointCount = 360;
var
  pradius, dumx, dumy: Double;
  SinCosLut: array[0 .. CPredictionPointCount - 1] of TPointD;

function PowellEvalTracking(const x: TVector; Data: Pointer): TScalar;
var
  Self: TScan2Track absolute Data;
  i: Integer;
  r, ri, predy, predx: Double;
begin
  r := pradius;
  ri := x[0] * Self.PointsPerRevolution / CPredictionPointCount;

  Result := 0;
  if InRange(r, Self.Scan.ConcentricGrooveRadius, Self.Scan.FirstGrooveRadius) and
      InRange(r - ri * CPredictionPointCount, Self.Scan.ConcentricGrooveRadius, Self.Scan.FirstGrooveRadius) then
    for i := 0 to CPredictionPointCount - 1 do
    begin
      predx := SinCosLut[i].X * r + Self.Scan.Center.X;
      predy := SinCosLut[i].Y * r + Self.Scan.Center.Y;

      Result -= Self.Scan.GetPointD(Self.Scan.Image, predy, predx);

      r -= ri;
    end;

  dumx := predx;
  dumy := predy;

  //WriteLn(x[0]:12:6,x[1]:12:6,Result:20:3);
end;

procedure BFGSEvalTracking(const arg: TVector; var func: Double; grad: TVector; obj: Pointer);
var
  Self: TScan2Track absolute obj;
  i: Integer;
  sn, cs, r, ri, predy, predx, id: Double;
begin
  ri := arg[0];

  func := 0;
  grad[0] := 0;
  for i := 0 to CPredictionPointCount - 1 do
  begin
    id := i * Self.PointsPerRevolution / CPredictionPointCount;

    r := pradius - id * ri;

    if InRange(r, Self.Scan.ConcentricGrooveRadius, Self.Scan.FirstGrooveRadius) then
    begin
      predx := SinCosLut[i].X * r + Self.Scan.Center.X;
      predy := SinCosLut[i].Y * r + Self.Scan.Center.Y;

      func -= Self.Scan.GetPointD(Self.Scan.Image, predy, predx);
      grad[0] -= Self.Scan.GetPointD(Self.Scan.XGradient, predy, predx) * cs * -id + Self.Scan.GetPointD(Self.Scan.YGradient, predy, predx) * sn * -id;
    end;
  end;

  dumx := predx;
  dumy := predy;

  //WriteLn(arg[0]:12:3,func:20:3,grad[0]:12:3);
end;

procedure BFGSEvalTracking_(const arg: TVector; var func: Double; grad: TVector; obj: Pointer);
var
  x: TVector;
  h: Double;
begin
  h := 1e-8;

  // 1/280 	−4/105 	1/5 	−4/5 	0 	4/5 	−1/5 	4/105 	−1/280

  grad[0] := 0;
  x := Copy(arg); x[0] += 4.0 * h; grad[0] += -1.0 / 280.0 * PowellEvalTracking(x, obj);
  x := Copy(arg); x[0] += 3.0 * h; grad[0] += 4.0 / 105.0 * PowellEvalTracking(x, obj);
  x := Copy(arg); x[0] += 2.0 * h; grad[0] += -1.0 / 5.0 * PowellEvalTracking(x, obj);
  x := Copy(arg); x[0] += 1.0 * h; grad[0] += 4.0 / 5.0 * PowellEvalTracking(x, obj);
  x := Copy(arg); x[0] -= 1.0 * h; grad[0] += -4.0 / 5.0 * PowellEvalTracking(x, obj);
  x := Copy(arg); x[0] -= 2.0 * h; grad[0] += 1.0 / 5.0 * PowellEvalTracking(x, obj);
  x := Copy(arg); x[0] -= 3.0 * h; grad[0] += -4.0 / 105.0 * PowellEvalTracking(x, obj);
  x := Copy(arg); x[0] -= 4.0 * h; grad[0] += 1.0 / 280.0 * PowellEvalTracking(x, obj);
  grad[0] /= h;

  func := PowellEvalTracking(arg, obj);

  //WriteLn(arg[0]:12:3,func:20:3,grad[0]:12:3,' estimated');
end;

procedure TScan2Track.EvalTrack;
const
  CBFPrecMul = 4;
var
  angle, radius, sn, cs, corr, px, py, p, bestf, f, bestr, a, ai: Double;
  i, pos: Integer;
  x, bl, bu: TVector;
  fs: TBufferedFileStream;
  pbuf: specialize TFPGList<TPoint>;
  t, pt: QWord;

  procedure Correct;
  var
    i, k: Integer;
    state: MinASAState;
    rep: MinASAReport;
  begin
    x[0] := corr;
    pradius := radius;

    ai := Pi * 2.0 / CPredictionPointCount;
    a := angle;
    for i := 0 to CPredictionPointCount - 1 do
    begin
      SinCos(a, SinCosLut[i].Y, SinCosLut[i].X);
      a -= ai;
    end;

  {$if 1}
    //PowellMinimize(@PowellEvalTracking, x, 1.0, 0, 0, MaxInt, Self);

    bestf := Infinity;
    bestr := 0;
    for k := -Round(C45RpmMaxGrooveWidth * Scan.DPI * CBFPrecMul) to Round(2.0 * Scan.DPI / C45RpmLeadInGroovesPerInch * CBFPrecMul) do
    begin
      x[0] := k / (FPointsPerRevolution * CBFPrecMul);
      f := PowellEvalTracking(x, Self);

      if f < bestf then
      begin
        bestf := f;
        bestr := x[0];
      end;
    end;

    x[0] := bestr;
    PowellEvalTracking(x, Self);

  {$else}
    MinASACreate(1, x, bl, bu, state);
    MinASASetCond(state, 0, 0, 0, 0);

    while MinASAIteration(state) do
      if State.NeedFG then
      begin
        //BFGSEvalTracking(State.X, state.F, state.G, Self);
        BFGSEvalTracking_(State.X, state.F, state.G, Self);
      end;

    MinASAResults(state, x, rep);
  {$ifend}

    main.Form1.Image.Picture.Bitmap.Canvas.Pixels[round(dumx * CReducFactor), round(dumy * CReducFactor)] := clBlue;

    corr := x[0];
  end;

begin
  WriteLn('EvalTrack');

  SetLength(x, 1);
  SetLength(bl, 1);
  SetLength(bu, 1);
  fs := TBufferedFileStream.Create('debug.raw', fmCreate or fmShareDenyNone);
  pbuf := specialize TFPGList<TPoint>.Create;
  try
    pos := 0;
    pt := GetTickCount64;

    angle := Scan.GrooveStartAngle;
    radius := Scan.FirstGrooveRadius;
    corr := (Scan.DPI / C45RpmLeadInGroovesPerInch) / FPointsPerRevolution *0 + 223 / FPointsPerRevolution;
    bl[0] := -0.1 * corr;
    bu[0] := 2.0 * corr;

    repeat
      Correct;

      angle := Scan.GrooveStartAngle - FRadiansPerRevolutionPoint * pos;
      radius -= corr;

      SinCos(angle, sn, cs);
      px := cs * radius + Self.Scan.Center.X;
      py := sn * radius + Self.Scan.Center.Y;

      Write(pos:8, corr:20:6, #13);

      if InRange(px, 0, High(Scan.Image[0]) - 1) and InRange(py, 0, High(Scan.Image) - 1) then
      begin
        t := GetTickCount64;

        pbuf.Add(Point(round(px * CReducFactor), round(py * CReducFactor)));

        if t - pt >= 1000 then
        begin

          for i := 0 to pbuf.Count - 1 do
            main.Form1.Image.Picture.Bitmap.Canvas.Pixels[pbuf[i].X, pbuf[i].Y] := clLime;

          main.Form1.HorzScrollBar.Position := pbuf.Last.X - main.Form1.Width div 2;
          main.Form1.VertScrollBar.Position := pbuf.Last.Y - main.Form1.Height div 2;

          pbuf.Clear;

          Application.ProcessMessages;
          pt := GetTickCount64;
        end;

        p := Scan.GetPointD(Scan.Image, py, px);

        fs.Write(p, sizeof(p));
      end;

      Inc(pos);

    until radius <= Scan.ConcentricGrooveRadius;
  finally
    fs.Free;
    pbuf.Free;
  end;
end;

procedure TScan2Track.Run;
begin
  Scan.Run;
  EvalTrack;
end;

end.

