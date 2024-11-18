unit scan2track;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Types, Math, Graphics, GraphType, FPCanvas, FPImage, FPWritePNG,
  utils, bufstream, fgl, powell, inputscan;

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
  CPredictionPointCount = 3600;
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

procedure TScan2Track.EvalTrack;
const
  CTrackingPrecMul = 10;

  function Correct(angle, radius: Double): Double;
  const
    CBFPrecMul = 1;
  var
    bestf, ai, a, f: Double;
    i, k: Integer;
    x: TVector;
  begin
    SetLength(x, 1);

    pradius := radius;

    ai := Pi * 2.0 / CPredictionPointCount;
    a := angle;
    for i := 0 to CPredictionPointCount - 1 do
    begin
      SinCos(a, SinCosLut[i].Y, SinCosLut[i].X);
      a -= ai;
    end;

    //PowellMinimize(@PowellEvalTracking, x, 1.0, 0, 0, MaxInt, Self);

    bestf := Infinity;
    Result := 0;
    for k := -Round(C45RpmMaxGrooveWidth * Scan.DPI * CBFPrecMul) to Round(2.0 * Scan.DPI / C45RpmLeadInGroovesPerInch * CBFPrecMul) do
    begin
      x[0] := k / (FPointsPerRevolution * CBFPrecMul);
      f := PowellEvalTracking(x, Self);

      if f < bestf then
      begin
        bestf := f;
        Result := x[0];
      end;
    end;

    x[0] := Result;
    PowellEvalTracking(x, Self);

    main.Form1.Image.Picture.Bitmap.Canvas.Pixels[round(dumx * CReducFactor), round(dumy * CReducFactor)] := clBlue;
  end;


var
  angle, radius, sn, cs, px, py, p, bestSkew, bestr, accInner, accOuter, accSkew, r: Double;
  i, j, pos: Integer;
  fs: TBufferedFileStream;
  pbuf: specialize TFPGList<TPoint>;
  t, pt: QWord;

begin
  WriteLn('EvalTrack');

  fs := TBufferedFileStream.Create('debug.raw', fmCreate or fmShareDenyNone);
  pbuf := specialize TFPGList<TPoint>.Create;
  try
    pos := 0;
    pt := GetTickCount64;

    angle := Scan.GrooveStartAngle;
    radius := Scan.FirstGrooveRadius;

    repeat
      angle := Scan.GrooveStartAngle - FRadiansPerRevolutionPoint * pos;

      SinCos(angle, sn, cs);

      bestSkew := Infinity;
      bestr := radius;
      for i := -Round(C45RpmMaxGrooveWidth * Scan.DPI * CTrackingPrecMul) to Round(C45RpmMaxGrooveWidth * Scan.DPI * CTrackingPrecMul) do
      begin
        accOuter := 0;
        accInner := 0;

        for j := 1 to Round(C45RpmMaxGrooveWidth * Scan.DPI * CTrackingPrecMul) do
        begin
          r := radius + (i + j) / CTrackingPrecMul;

          px := cs * r + Self.Scan.Center.X;
          py := sn * r + Self.Scan.Center.Y;

          if Scan.InRangePointD(py, px) then
            accOuter += Scan.GetPointD(Scan.Image, py, px);
        end;

        for j := -1 downto -Round(C45RpmMaxGrooveWidth * Scan.DPI * CTrackingPrecMul) do
        begin
          r := radius + (i + j) / CTrackingPrecMul;

          px := cs * r + Self.Scan.Center.X;
          py := sn * r + Self.Scan.Center.Y;

          if Scan.InRangePointD(py, px) then
            accInner += Scan.GetPointD(Scan.Image, py, px);
        end;

        accSkew := Abs(accInner - accOuter);

        if (accSkew < bestSkew) and (accInner + accOuter > 0.1) then
        begin
          bestSkew := accSkew;
          bestr := radius + i / CTrackingPrecMul;
        end;
      end;

      if IsInfinite(bestSkew) then
      begin
        radius -= Correct(angle, radius);
      end
      else
      begin
        radius := radius * 0.99 + bestr * 0.01;
      end;


      px := cs * radius + Self.Scan.Center.X;
      py := sn * radius + Self.Scan.Center.Y;

      Write(pos:8, bestr:20:6, bestSkew:20:6, #13);

      if Scan.InRangePointD(py, px) then
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

