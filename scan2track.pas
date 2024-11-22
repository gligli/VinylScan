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
  CPredictionRevolutionDiv = 100;
var
  pradius, dumx, dumy: Double;
  SinCosLut: array[0 .. CPredictionPointCount - 1] of record
    Sin, Cos: Double;
  end;

function PowellEvalTracking(const arg: TVector; obj: Pointer): TScalar;
var
  Self: TScan2Track absolute obj;
  i: Integer;
  r, x, predy, predx, fx: Double;
begin
  r := pradius;
  x := arg[0] * Self.PointsPerRevolution / (CPredictionPointCount * CPredictionRevolutionDiv);

  Result := 0;
  for i := 0 to CPredictionPointCount - 1 do
  begin
    if InRange(r, Self.Scan.ConcentricGrooveRadius, Self.Scan.FirstGrooveRadius) and
       InRange(r, Self.Scan.ConcentricGrooveRadius, Self.Scan.FirstGrooveRadius) then
    begin
      predx := SinCosLut[i].Cos * r + Self.Scan.Center.X;
      predy := SinCosLut[i].Sin * r + Self.Scan.Center.Y;

      fx := Self.Scan.GetPointD(Self.Scan.Image, predy, predx, imLinear);
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

      fx := Self.Scan.GetPointD(Self.Scan.Image, predy, predx, imLinear);
      func -= fx;
      grad[0] -= Self.Scan.GetPointD(Self.Scan.XGradient, predy, predx, imLinear) * (SinCosLut[i].Cos * id) +
                 Self.Scan.GetPointD(Self.Scan.YGradient, predy, predx, imLinear) * (SinCosLut[i].Sin * id);

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
  ig, ic: Integer;
  x: TVector;
begin
  ig := 0;
  grad[ig] := 0;
  for ic := 0 to High(CFCoeff) do
  begin
    x := Copy(arg);
    x[ig] += CXCoeff[ic] * CH;
    grad[ig] += CFCoeff[ic] * PowellEvalTracking(x, obj);
  end;
  grad[ig] /= CH;

  func := PowellEvalTracking(arg, obj);

  //for ig := 0 to High(arg) do
  //  Write(arg[ig]:14:6);
  //WriteLn(func:14:6);
end;

procedure TScan2Track.EvalTrack;
const
  CDecodePrecMul = 1000;

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

{$if 0}
    PowellMinimize(@PowellEvalTracking, x, 1e-6, 1e-6, 1e-6, MaxInt, Self);

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
  angle, radius, sn, cs, px, py, p, r, bestp, radiusInc, radiusIncSmoo, c16a: Double;
  i, pos: Integer;
  fs: TFileStream;
  pbuf: specialize TFPGList<TPoint>;
  t, pt: QWord;
  ismp, smp:SmallInt;
begin
  WriteLn('EvalTrack');

  fs := TFileStream.Create('debug.raw', fmCreate or fmShareDenyNone);
  pbuf := specialize TFPGList<TPoint>.Create;
  try
    pos := 0;
    pt := GetTickCount64;

    c16a := C45RpmMaxGrooveWidth * Scan.DPI / High(SmallInt);
    angle := Scan.GrooveStartAngle;
    radius := Scan.FirstGrooveRadius;
    radiusInc := -(Scan.DPI / C45RpmLeadInGroovesPerInch) / FPointsPerRevolution;
    radiusIncSmoo := radiusInc;

    repeat
      angle := Scan.GrooveStartAngle - FRadiansPerRevolutionPoint * pos;

      SinCos(angle, sn, cs);

      bestp := -Infinity;
      smp := 0;
      for ismp := Low(SmallInt) to high(SmallInt) do
      begin
        r := radius + ismp * c16a;

        px := cs * r + Self.Scan.Center.X;
        py := sn * r + Self.Scan.Center.Y;

        if Scan.InRangePointD(py, px) then
        begin
          p := Scan.GetPointD(Scan.Image, py, px, imHermite);
          if p > bestp then
          begin
            bestp := p;
            smp := ismp;
          end;
        end;
      end;
      fs.WriteWord(Word(smp));

      px := cs * radius + Self.Scan.Center.X;
      py := sn * radius + Self.Scan.Center.Y;

////////////////////////
      t := GetTickCount64;
      pbuf.Add(Point(round(px * CReducFactor), round(py * CReducFactor)));
      if t - pt >= 5000 then
      begin

        for i := 0 to pbuf.Count - 1 do
          main.MainForm.Image.Picture.Bitmap.Canvas.Pixels[pbuf[i].X, pbuf[i].Y] := clLime;

        main.MainForm.HorzScrollBar.Position := pbuf.Last.X - main.MainForm.Width div 2;
        main.MainForm.VertScrollBar.Position := pbuf.Last.Y - main.MainForm.Height div 2;

        pbuf.Clear;

        Application.ProcessMessages;
        pt := GetTickCount64;
      end;
////////////////////////

      Correct(angle, radius, radiusInc);
      radiusIncSmoo := lerp(radiusIncSmoo, radiusInc, CLowCutoffFreq * 2.0 / FSampleRate);

      Write(pos:8, radiusInc:20:6, radiusIncSmoo:20:6, #13);

      radiusInc := radiusIncSmoo;
      radius += radiusInc;
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

