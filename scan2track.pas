unit scan2track;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Types, Math, Graphics, GraphType, FPCanvas, FPImage, FPWritePNG, MTProcs,
  utils, fgl, powell, inputscan, minlbfgs, minasa, FilterIIRLPBessel, FilterIIRHPBessel;

type

  { TScan2Track }

  TScan2Track = class
  private
    FOutputWAVFileName: String;
    FMethod: TMinimizeMethod;
    FSinCosLUT: TPointDDynArray;

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
    property Method: TMinimizeMethod read FMethod write FMethod;

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
  FMethod := mmLBFGS;

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
  CPredictionPointCount = 600;
  CPredictionRevolutionDiv = 360.0 / 1.0;
var
  pradius, dumx, dumy: Double;

function TScan2Track.PowellEvalTracking(const arg: TVector; obj: Pointer): TScalar;
var
  i: Integer;
  r, x, predy, predx, fx: Double;
begin
  r := pradius;
  x := arg[0] * PointsPerRevolution / (CPredictionPointCount * CPredictionRevolutionDiv);

  predx := 0;
  predy := 0;

  Result := 0;
  for i := 0 to CPredictionPointCount - 1 do
  begin
    if InRange(r, Scan.ConcentricGrooveRadius, Scan.FirstGrooveRadius) and
       InRange(r, Scan.ConcentricGrooveRadius, Scan.FirstGrooveRadius) then
    begin
      predx := FSinCosLut[i].X * r + Scan.Center.X;
      predy := FSinCosLut[i].Y * r + Scan.Center.Y;

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

  predx := 0;
  predy := 0;

  func := 0;
  grad[0] := 0;
  for i := 0 to CPredictionPointCount - 1 do
  begin
    id := i * Self.PointsPerRevolution / (CPredictionPointCount * CPredictionRevolutionDiv);

    r := pradius + id * x;

    if InRange(r, Self.Scan.ConcentricGrooveRadius, Self.Scan.FirstGrooveRadius) then
    begin
      predx := Self.FSinCosLut[i].X * r + Self.Scan.Center.X;
      predy := Self.FSinCosLut[i].Y * r + Self.Scan.Center.Y;

      fx := Self.Scan.GetPointD(predy, predx, isImage, imLinear);
      func -= fx;
      grad[0] -= Self.Scan.GetPointD(predy, predx, isXGradient, imLinear) * (Self.FSinCosLut[i].X * id) +
                 Self.Scan.GetPointD(predy, predx, isYGradient, imLinear) * (Self.FSinCosLut[i].Y * id);

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
    x, g, bl, bu: TVector;
    ASAState: MinASAState;
    ASARep: MinASAReport;
    LBFGSState: MinLBFGSState;
    LBFGSRep: MinLBFGSReport;
  begin
    SetLength(x, 1);
    SetLength(bl, 1);
    SetLength(bu, 1);
    SetLength(g, 1);

    x[0] := radiusInc;
    bl[0] := -(Scan.DPI / C45RpmLeadInGroovesPerInch) / FPointsPerRevolution * 10.0;
    bu[0] := (Scan.DPI / C45RpmLeadInGroovesPerInch) / FPointsPerRevolution * 10.0;

    pradius := radius;

    SetLength(FSinCosLUT, CPredictionPointCount);
    ai := Pi * 2.0 / (CPredictionPointCount * CPredictionRevolutionDiv);
    a := angle;
    for i := 0 to CPredictionPointCount - 1 do
    begin
      SinCos(a, FSinCosLut[i].Y, FSinCosLut[i].X);
      a -= ai;
    end;

    case Method of
      mmPowell:
      begin
        PowellMinimize(@PowellEvalTracking, x, 1e-8, 1e-6, 0, MaxInt, Self);
        radiusInc := x[0];
        PowellEvalTracking(x, self);
      end;
      mmASA:
      begin
        MinASACreate(1, x, bl, bu, ASAState);
        MinASASetCond(ASAState, 0, 0, 0, 0);

        while MinASAIteration(ASAState) do
          if ASAState.NeedFG then
          begin
            BFGSEvalTracking(ASAState.X, ASAState.F, ASAState.G, Self);
            //BFGSEvalTracking_(ASAState.X, ASAState.F, ASAState.G, Self);
          end;

        MinASAResults(ASAState, x, ASARep);

        radiusInc := x[0];
        BFGSEvalTracking(x, f, g, self);
        //BFGSEvalTracking_(x, f, g, self);
      end;
      mmLBFGS:
      begin
        MinLBFGSCreate(1, 1, x, LBFGSState);
        MinLBFGSSetCond(LBFGSState, 0, 0, 0, 0);

        while MinLBFGSIteration(LBFGSState) do
          if LBFGSState.NeedFG then
          begin
            BFGSEvalTracking(LBFGSState.X, LBFGSState.F, LBFGSState.G, Self);
            //BFGSEvalTracking_(LBFGSState.X, LBFGSState.F, LBFGSState.G, Self);
          end;

        MinLBFGSResults(LBFGSState, x, LBFGSRep);

        radiusInc := x[0];
        BFGSEvalTracking(x, f, g, self);
        //BFGSEvalTracking_(x, f, g, self);
      end;
    end;

    main.MainForm.Image.Picture.Bitmap.Canvas.Pixels[round(dumx * CReducFactor), round(dumy * CReducFactor)] := clTeal;
  end;

type
  TSampleDecoderType = SmallInt;

var
  angle, radius, sn, cs, px, py, r, radiusInc, radiusIncSmoo, cxa, middleSmp, fsmp: Double;
  i, pos, aboveCnt, aboveAcc: Integer;
  pbuf: specialize TFPGList<TPoint>;
  t, pt: QWord;
  ismp, smp:SmallInt;
  fltRadiusInc: TFilterIIRLPBessel;
  fltSamples: TFilterIIRHPBessel;
  smpBuf: array[Low(TSampleDecoderType) .. High(TSampleDecoderType)] of Double;
  samples: TSmallIntDynArray;
begin
  WriteLn('EvalTrack');

  SetLength(samples, FSampleRate);
  fltRadiusInc := TFilterIIRLPBessel.Create(nil);
  fltSamples := TFilterIIRHPBessel.Create(nil);
  pbuf := specialize TFPGList<TPoint>.Create;
  try
    fltRadiusInc.FreqCut1 := CLowCutoffFreq;
    fltRadiusInc.SampleRate := FSampleRate;
    fltRadiusInc.Order := 8;

    fltSamples.FreqCut1 := CLowCutoffFreq;
    fltSamples.SampleRate := FSampleRate;
    fltSamples.Order := 4;

    pos := 0;
    pt := GetTickCount64;

    cxa := C45RpmMaxGrooveWidth * Scan.DPI / High(TSampleDecoderType);
    angle := Scan.GrooveStartAngle;
    radius := Scan.FirstGrooveRadius;
    radiusInc := -(Scan.DPI / C45RpmLeadInGroovesPerInch) / FPointsPerRevolution;

    repeat
      angle := Scan.GrooveStartAngle - FRadiansPerRevolutionPoint * pos;

      SinCos(angle, sn, cs);

      smp := 0;
      for ismp := Low(TSampleDecoderType) to high(TSampleDecoderType) do
      begin
        r := radius + ismp * cxa;

        px := cs * r + Self.Scan.Center.X;
        py := sn * r + Self.Scan.Center.Y;

        if Scan.InRangePointD(py, px) then
          smpBuf[ismp] := Scan.GetPointD(py, px, isImage, imHermite);
      end;

      middleSmp := (MinValue(smpBuf) + MaxValue(smpBuf)) * 0.5;

      aboveAcc := 0;
      aboveCnt := 0;
      for ismp := Low(TSampleDecoderType) to high(TSampleDecoderType) do
        if smpBuf[ismp] >= middleSmp then
        begin
          aboveAcc += ismp;
          Inc(aboveCnt);
        end;

      fsmp := aboveAcc / (High(TSampleDecoderType) * aboveCnt);
      fsmp := fltSamples.FilterFilter(fsmp);
      smp :=  Make16BitSample(fsmp);

      while pos >= Length(samples) do
        SetLength(samples, Ceil(Length(samples) * cPhi));
      samples[pos] := smp;

      px := cs * radius + Self.Scan.Center.X;
      py := sn * radius + Self.Scan.Center.Y;

      Correct(angle, radius, radiusInc);

      //if Scan.InRangePointD(py, px) then
      //begin
      //  radiusInc := (-sn * Self.Scan.GetPointD(py, px, isXGradient, imHermite) + cs * Self.Scan.GetPointD(py, px, isYGradient, imHermite)) * -1.0;
      //end;


      radiusIncSmoo := fltRadiusInc.FilterFilter(radiusInc);

      Write(pos:8, aboveCnt:8, radiusInc:20:9, #13);

      radius += radiusIncSmoo;
      Inc(pos);

////////////////////////
      t := GetTickCount64;
      pbuf.Add(Point(round(px * CReducFactor), round(py * CReducFactor)));
      if t - pt >= 2000 then
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
    fltRadiusInc.Free;
    fltSamples.Free;
  end;
end;

procedure TScan2Track.Run;
begin
  Scan.Run;
  EvalTrack;
end;

end.

