unit inputscan;

{$include 'compileroptions.inc'}

interface

uses
  Classes, SysUtils, Types, Math, Graphics, FPReadTiff, FPImage, PNGComn, MTProcs,
  utils, powell, hackedreadpng;

type
  TInputScan = class;

  TCorrectRef = record
    AngleIdx, ScanIdx: Integer;
  end;

  TCropData = record
    StartAngle, EndAngle: Double;
    StartAngleMirror, EndAngleMirror: Double;
    RadiusAngleLut: TRadiusAngleDynArray;
    SinCosLut: TSinCosDynArray;
  end;

  TInputScanDynArray = array of TInputScan;

  { TInputScan }

  TInputScan = class
  private
    FImageFileName: String;
    FDPI: Integer;
    FPointsPerRevolution: Integer;
    FRadiansPerRevolutionPoint: Double;
    FSilent: Boolean;
    FSinCosLUT: TSinCosDynArray;

    FCenterExtents: TRect;
    FCenter: TPointD;
    FConcentricGrooveRadius: Double;
    FSkewY: Double;
    FSetDownRadius: Double;
    FGrooveStartAngle: Double;
    FGrooveStartPoint: TPointD;

    FRelativeAngle: Double;
    FCropData: TCropData;
    FCenterQuality: Double;
    FObjective: Double;

    FWidth, FHeight: Integer;
    FImage: TWordDynArray;
    FLeveledImage: TWordDynArray;

    FCorrectRefs: array of TCorrectRef;
    FLock: TSpinlock;

    procedure SetRevolutionFromDPI(ADPI: Integer);
    procedure SetRevolutionFromSampleRate(ASampleRate: Integer);
    function GetImageShortName: String;

    procedure GradientConcentricGroove(const arg: TDoubleDynArray; var func: Double; grad: TDoubleDynArray; obj: Pointer);
    function GridReduceConcentricGroove(const x: TVector; obj: Pointer): TScalar;
    function NelderMeadCrop(const x: TVector; obj: Pointer): TScalar;

    procedure FindCenterExtents;
    procedure FindConcentricGroove_GridSearch;
    procedure FindConcentricGroove_Gradient;
    procedure FindGrooveStart;

    procedure LoadPNG;
    procedure LoadTIFF;
  public
    constructor Create(ADefaultDPI: Integer = 2400; ASilent: Boolean = False);
    destructor Destroy; override;

    procedure LoadImage;
    procedure BrickwallLimit;
    procedure FindTrack(AUseGradient: Boolean; AForcedSampleRate: Integer = -1);
    procedure CorrectByModel(ACenterX, ACenterY, ARelativeAngle, ASkewY: Double);
    procedure Crop(const RadiusAngleLut: TRadiusAngleDynArray; const SinCosLut: TSinCosDynArray);
    procedure FixCISScanners;

    function InRangePointD(Y, X: Double): Boolean;
    function GetPointD_Linear(const Image: TWordDynArray; Y, X: Double): Double;
    procedure GetGradientsD(const Image: TWordDynArray; Y, X: Double; out GY: Double; out GX: Double);
    function GetPointD_Sinc(const Image: TWordDynArray; Y, X: Double): Single;

    procedure AddCorrectRef(AngleIdx, ScanIdx: Integer);
    function HasCorrectRef(const AList: TInputScanDynArray; AngleIdx, ScanIdx: Integer): Boolean;

    property ImageFileName: String read FImageFileName write FImageFileName;
    property ImageShortName: String read GetImageShortName;

    property DPI: Integer read FDPI;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;

    property ConcentricGrooveRadius: Double read FConcentricGrooveRadius;
    property SetDownRadius: Double read FSetDownRadius;
    property GrooveStartAngle: Double read FGrooveStartAngle;
    property GrooveStartPoint: TPointD read FGrooveStartPoint;
    property PointsPerRevolution: Integer read FPointsPerRevolution;
    property RadiansPerRevolutionPoint: Double read FRadiansPerRevolutionPoint;

    property Center: TPointD read FCenter;
    property RelativeAngle: Double read FRelativeAngle;
    property SkewY: Double read FSkewY;
    property CropData: TCropData read FCropData;

    property CenterQuality: Double read FCenterQuality;
    property Objective: Double read FObjective write FObjective;

    property Image: TWordDynArray read FImage;
    property LeveledImage: TWordDynArray read FLeveledImage;
  end;

  { TScanImage }

  TScanImage = class(TFPCustomImage)
  private
    FImage: TWordDynArray;
  protected
    procedure SetInternalPixel(x,y:integer; Value:integer); override;
    function GetInternalPixel(x,y:integer) : integer; override;
    procedure SetInternalColor (x,y:integer; const Value:TFPColor); override;
  public
    property Image: TWordDynArray read FImage write FImage;
  end;

  { TDPIAwareReaderPNG }

  TDPIAwareReaderPNG = class(THackedReaderPNG)
  private
    FDPI: TPoint;
  protected
    procedure HandleChunk; override;
  public
    constructor Create; override;

    property DPI: TPoint read FDPI;
  end;

implementation

{ TInputScan }

procedure TInputScan.SetRevolutionFromDPI(ADPI: Integer);
begin
  FPointsPerRevolution := Ceil(Pi * C45RpmOuterSize * ADPI);
  FRadiansPerRevolutionPoint := Pi * 2.0 / FPointsPerRevolution;
end;

procedure TInputScan.SetRevolutionFromSampleRate(ASampleRate: Integer);
begin
  FPointsPerRevolution := Ceil(ASampleRate / C45RpmRevolutionsPerSecond);
  FRadiansPerRevolutionPoint := Pi * 2.0 / FPointsPerRevolution;
end;

function TInputScan.GetImageShortName: String;
begin
  Result := ChangeFileExt(ExtractFileName(FImageFileName), '');
end;

procedure TInputScan.FindConcentricGroove_GridSearch;
const
  CPointsPerRevolution = 512;
  CSkewDivisor = 1000.0;
  CMinSkew = round(0.98 * CSkewDivisor);
  CMaxSkew = round(1.02 * CSkewDivisor);
var
  sinCosLUT: TSinCosDynArray;
  stencilX: array[TValueSign] of Double;
  stencilY: array[TValueSign] of Integer;
  results: array[CMinSkew .. CMaxSkew] of record
    Objective: Int64;
    X: TVector;
  end;

  procedure DoSkew(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    cx, cy, k, iLut, px, py, ff, resIdx: Integer;
    sky, r: Double;
    f: Int64;
    vs: TValueSign;
    pxArr, pyArr: array[TValueSign, 0 .. CPointsPerRevolution div 4 - 1] of Integer;
  begin
    resIdx := AIndex;
    if not InRange(resIdx, CMinSkew, CMaxSkew) then
      Exit;

    results[resIdx].Objective := Low(Int64);

    sky := resIdx / CSkewDivisor;
    for k := round(C45RpmMinConcentricGroove * FDPI * 0.5) to round(C45RpmMaxConcentricGroove * FDPI * 0.5) do
    begin
      r := k;

      for vs := Low(TValueSign) to High(TValueSign) do
        for iLut := 0 to High(sinCosLUT) do
        begin
          pxArr[vs, ilut] := round(sinCosLUT[iLut].Cos * (r + stencilX[vs]));
          pyArr[vs, ilut] := round(sinCosLUT[iLut].Sin * (r + stencilX[vs]) * sky);
        end;

      for cy := FCenterExtents.Top to FCenterExtents.Bottom do
        for cx := FCenterExtents.Left to FCenterExtents.Right do
        begin
          f := 0;
          for vs := Low(TValueSign) to High(TValueSign) do
          begin
            ff := 0;

            for iLut := 0 to High(sinCosLUT) do
            begin
              px := pxArr[vs, iLut];
              py := pyArr[vs, iLut];

              ff += FLeveledImage[(cy - py) * Width + cx + px];
              ff += FLeveledImage[(cy - py) * Width + cx - px];
              ff += FLeveledImage[(cy + py) * Width + cx + px];
              ff += FLeveledImage[(cy + py) * Width + cx - px];
            end;

            f += ff * stencilY[vs];
          end;

          if f > results[resIdx].Objective then
          begin
            results[resIdx].Objective := f;
            results[resIdx].X := [cx, cy, r, sky];
          end;
        end;
    end;
  end;

var
  iRes: Integer;
  bestf: Int64;
  x: TDoubleDynArray;
begin
  // grid search algorithm to find the concentric groove

  BuildSinCosLUT(CPointsPerRevolution div 4, sinCosLUT, 0.0, Pi / 2.0);

  stencilY[NegativeValue] := -1;
  stencilY[ZeroValue] := 2;
  stencilY[PositiveValue] := -1;

  stencilX[NegativeValue] := -C45RpmLeadOutGrooveThickness * FDPI;
  stencilX[ZeroValue] := 0;
  stencilX[PositiveValue] := C45RpmLeadOutGrooveThickness * FDPI;

  x := [FCenter.X, FCenter.Y, C45RpmConcentricGroove * FDPI * 0.5, 1.0];

  ProcThreadPool.DoParallelLocalProc(@DoSkew, CMinSkew, CMaxSkew);

  bestf := Low(Int64);
  for iRes := CMinSkew to CMaxSkew do
    if results[iRes].Objective > bestf then
    begin
      bestf := results[iRes].Objective;
      x := results[iRes].X;
    end;

  FCenter.X := x[0];
  FCenter.Y := x[1];
  FConcentricGrooveRadius := x[2];
  FSkewY := x[3];
  FCenterQuality := bestf / High(Word);

  //WriteLn(ImageShortName, FCenter.X:12:3, FCenter.Y:12:3, FConcentricGrooveRadius:12:3, FSkewY:12:6, FCenterQuality:12:0);
end;

procedure TInputScan.GradientConcentricGroove(const arg: TDoubleDynArray; var func: Double; grad: TDoubleDynArray;
  obj: Pointer);
var
  iLut: Integer;
  px, py, cx, cy, sky, r, cs, sn, gix, giy, gcx, gcy, gr, gsky, rsx, sy: Double;
  stencilX: array[TValueSign] of Double;
  stencilY: array[TValueSign] of Integer;
  vs: TValueSign;
begin
  cx := arg[0];
  cy := arg[1];
  r := arg[2];
  sky := arg[3];

  stencilY[NegativeValue] := -1;
  stencilY[ZeroValue] := 2;
  stencilY[PositiveValue] := -1;

  stencilX[NegativeValue] := -C45RpmLeadOutGrooveThickness * FDPI;
  stencilX[ZeroValue] := 0;
  stencilX[PositiveValue] := C45RpmLeadOutGrooveThickness * FDPI;

  func := 0.0;
  if Assigned(grad) then
    FillQWord(grad[0], Length(grad), 0);

  gcx := 0.0;
  gcy := 0.0;
  gr := 0.0;
  gsky := 0.0;

  for iLut := 0 to High(FSinCosLUT) do
  begin
    cs := FSinCosLUT[iLut].Cos;
    sn := FSinCosLUT[iLut].Sin;

    for vs := Low(TValueSign) to High(TValueSign) do
    begin
      rsx := r + stencilX[vs];
      sy := stencilY[vs];

      px := cs * rsx + cx;
      py := sn * rsx * sky + cy;

      if InRangePointD(py, px) then
      begin
        func -= GetPointD_Linear(FLeveledImage, py, px) * sy;

        if Assigned(grad) then
        begin
          GetGradientsD(FLeveledImage, py, px, giy, gix);

          gcx -= gix * sy;
          gcy -= giy * sy;
          gr -= (gix * cs + giy * sn * sky) * sy;
          gsky -= giy * sn * sy;
        end;
      end;
    end;
  end;

  func /= High(Word);

  if Assigned(grad) then
  begin
    grad[0] := gcx / High(Word);
    grad[1] := gcy / High(Word);
    grad[2] := gr / High(Word);
    grad[3] := gsky / High(Word);
  end;
end;

function TInputScan.GridReduceConcentricGroove(const x: TVector; obj: Pointer): TScalar;
begin
  GradientConcentricGroove(x, Result, nil, obj);
end;

procedure TInputScan.FindConcentricGroove_Gradient;
var
  ff: Double;
  X: TDoubleDynArray;
begin
  BuildSinCosLUT(Ceil(Pi * C45RpmConcentricGroove * FDPI), FSinCosLUT);

  X := [FCenter.X, FCenter.Y, FConcentricGrooveRadius, FSkewY];

  ff := GradientDescentMinimize(@GradientConcentricGroove, X, [0.0005, 0.0005, 0.0002, 0.0000001], 1e-6, 10000, True);

  FCenter.X := X[0];
  FCenter.Y := X[1];
  FConcentricGrooveRadius := X[2];
  FSkewY := X[3];
  FCenterQuality := -ff;
end;

procedure TInputScan.FindGrooveStart;
var
  i: Integer;
  v, best, sn, cs, bestr, x, y, bestx, besty: Double;
begin
  best := -Infinity;
  bestx := 0;
  besty := 0;
  bestr := 0;
  v := 0;

  for i := 0 to FPointsPerRevolution - 1  do
  begin
    SinCos(i * FRadiansPerRevolutionPoint, sn, cs);

    x := cs * FSetDownRadius + FCenter.X;
    y := sn * FSetDownRadius * FSkewY + FCenter.Y;

    if InRangePointD(y, x) then
    begin
      v := v * 0.99 + GetPointD_Linear(FLeveledImage, y, x) * 0.01;

      if v > best then
      begin
        best := v;
        bestx := x;
        besty := y;
        bestr := i * FRadiansPerRevolutionPoint;
      end;
    end;

    //writeln(i:6,x:8,y:8,v:9:3,best:9:3,bestx:8,besty:8);
  end;

  FGrooveStartAngle := bestr;
  FGrooveStartPoint.X := bestx;
  FGrooveStartPoint.Y := besty;
end;

constructor TInputScan.Create(ADefaultDPI: Integer; ASilent: Boolean);
begin
  FDPI := ADefaultDPI;
  FSilent := ASilent;
  FCenterQuality := -Infinity;
  FObjective := Infinity;
  FSkewY := 1.0;

  SetRevolutionFromDPI(FDPI);
end;

destructor TInputScan.Destroy;
begin
  inherited Destroy;
end;

procedure TInputScan.LoadImage;
begin
  if SameText(ExtractFileExt(FImageFileName), '.tif') or
      SameText(ExtractFileExt(FImageFileName), '.tiff') then
    LoadTIFF
  else
    LoadPNG;

  FindCenterExtents;
end;

procedure TInputScan.LoadPNG;
var
  fs: TFileStream;
  png: TDPIAwareReaderPNG;
  img: TScanImage;
  sz: TPoint;
begin
  if not FSilent then WriteLn('LoadPNG ', FImageFileName);

  fs := TFileStream.Create(FImageFileName, fmOpenRead or fmShareDenyNone);
  png := TDPIAwareReaderPNG.Create;
  try
    sz := png.ImageSize(fs);
    FWidth := sz.X;
    FHeight := sz.Y;

    img := TScanImage.Create(FWidth, FHeight);
    try
      SetLength(FImage, FHeight * FWidth);
      img.Image := FImage;

      if not FSilent then WriteLn('Size:', Width:6, 'x', Height:6);

      png.ImageRead(fs, img);

      if (png.DPI.X > 0) and (png.DPI.X = png.DPI.Y) then
      begin
        FDPI := png.DPI.X;
        if not FSilent then WriteLn('DPI:', FDPI:6);
      end;
    finally
      img.Free;
    end;

    FLeveledImage := FImage;
  finally
    png.Free;
    fs.Free;
  end;
end;

procedure TInputScan.LoadTIFF;
var
  fs: TFileStream;
  tiff: TFPReaderTiff;
  img: TScanImage;
  szX, szY: DWORD;
  dpiX, dpiY: Double;
begin
  if not FSilent then WriteLn('LoadTIFF ', FImageFileName);

  fs := TFileStream.Create(FImageFileName, fmOpenRead or fmShareDenyNone);
  tiff := TFPReaderTiff.Create;
  try
    if not GetTIFFSize(fs, szX, szY, dpiX, dpiY) then
    begin
      szX := 0;
      szY := 0;
      dpiX := 0;
      dpiY := 0;
    end;
    FWidth := szX;
    FHeight := szY;

    fs.Seek(soFromBeginning, 0);

    img := TScanImage.Create(FWidth, FHeight);
    try
      SetLength(FImage, FHeight * FWidth);
      img.Image := FImage;

      if not FSilent then WriteLn('Size:', Width:6, 'x', Height:6);

      tiff.ImageRead(fs, img);

      if (dpiX > 0) and (dpiX = dpiY) then
      begin
        FDPI := Round(dpiX);
        if not FSilent then WriteLn('DPI:', FDPI:6);
      end;
    finally
      img.Free;
    end;

    FLeveledImage := FImage;
  finally
    tiff.Free;
    fs.Free;
  end;
end;

procedure TInputScan.BrickwallLimit;
const
  CRadius = 32;
  CSigma = 2;
var
  offsets: TIntegerDynArray;

  procedure GetL2Extents(ayx: Integer; out amean, astddev: Integer);
  var
    i: Integer;
    px, mn: Integer;
    sd: Single;
    sdAcc: UInt64;
  begin
    mn := 0;
    for i := 0 to High(offsets) do
    begin
      px := FImage[ayx + offsets[i]];
      mn += px;
    end;
    mn := mn div Length(offsets);

    sdAcc := 0;
    for i := 0 to High(offsets) do
    begin
      px := FImage[ayx + offsets[i]];
      px -= mn;
      sdAcc += px * px;
    end;
    sd := Sqrt(sdAcc div Length(offsets));

    amean := mn;
    astddev := round(CSigma * sd);
  end;

  procedure DoY(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    px, x, y, yx: Integer;
    mn, sd: Integer;
  begin
    if not InRange(AIndex, CRadius, FHeight - 1 - CRadius) then
      Exit;

    y := AIndex;

    for x := CRadius to FWidth - 1 - CRadius do
    begin
      yx := y * Width + x;

      px := FImage[yx];

      GetL2Extents(yx, mn, sd);
      px := (px - mn) * (High(Word) + 1) div (sd + 1) + mn;
      px := EnsureRange(px, 0, High(word));

      FLeveledImage[yx] := px;
    end;
  end;

var
  x, y, r, pos: Integer;
begin
  if not FSilent then WriteLn('BrickwallLimit');

  SetLength(offsets, Sqr(CRadius * 2 + 1));
  pos := 0;
  for y := -CRadius to CRadius do
    for x := -CRadius to CRadius do
    begin
      r := round(Sqrt(Sqr(y) + Sqr(x)));

      if r <= CRadius then
      begin
        offsets[pos] := y * Width + x;
        Inc(pos);
      end;
    end;
  SetLength(offsets, pos);

  FLeveledImage := nil;
  SetLength(FLeveledImage, Height * Width);

  ProcThreadPool.DoParallelLocalProc(@DoY, CRadius, FHeight - 1 - CRadius);
end;

procedure TInputScan.FindTrack(AUseGradient: Boolean; AForcedSampleRate: Integer);
begin
  if not FSilent then WriteLn('FindTrack');

  if AForcedSampleRate >= 0 then
    SetRevolutionFromSampleRate(AForcedSampleRate)
  else
    SetRevolutionFromDPI(FDPI);

  FSetDownRadius := (C45RpmStylusSetDown + 2.0 * C45RpmFirstMusicGroove) / 3.0 * FDPI * 0.5;
  FConcentricGrooveRadius := C45RpmConcentricGroove * FDPI * 0.5;

  FindConcentricGroove_GridSearch;
  if AUseGradient then
    FindConcentricGroove_Gradient;
  FindGrooveStart;

  if not FSilent then
  begin
    WriteLn('Center:', FCenter.X:12:3, ',', FCenter.Y:12:3);
    WriteLn('ConcentricGrooveRadius:', FConcentricGrooveRadius:12:3);
    WriteLn('SkewY:', FSkewY:12:6);
    WriteLn('CenterQuality:', FCenterQuality:12:6);
    WriteLn('SetDownRadius:', FSetDownRadius:12:3);
    WriteLn('GrooveStartPoint:', FGrooveStartPoint.X:12:3, ',', FGrooveStartPoint.Y:12:3);
    Writeln('Inner raw sample rate: ', Round(Pi * C45RpmLastMusicGroove * FDPI * C45RpmRevolutionsPerSecond), ' Hz');
  end
  else
  begin
    WriteLn(ImageFileName, ', CenterX:', FCenter.X:9:3, ', CenterY:', FCenter.Y:9:3, ', ConcentricGroove:', FConcentricGrooveRadius:10:3, ', SkewY:', FSkewY:9:6, ', Quality:', FCenterQuality:12:3);
  end;
end;

procedure TInputScan.CorrectByModel(ACenterX, ACenterY, ARelativeAngle, ASkewY: Double);
begin
  if not IsNan(ACenterX) then FCenter.X := ACenterX;
  if not IsNan(ACenterY) then FCenter.Y := ACenterY;
  if not IsNan(ARelativeAngle) then FRelativeAngle := ARelativeAngle;
  if not IsNan(ASkewY) then FSkewY := ASkewY;
end;

function TInputScan.NelderMeadCrop(const x: TVector; obj: Pointer): TScalar;
var
  iLut, iMean: Integer;
  a0a, a1a, a0b, a1b, cx, cy, r, px, py, bt, rOuter: Double;
  cropped: Boolean;
  cnt: array[Boolean] of TIntegerDynArray;
  acc: array[Boolean] of TDoubleDynArray;
  ra: ^TRadiusAngle;
  sc: ^TSinCos;
begin
  Result := 1e6;

  if not InRange(NormalizeAngle(x[1] - x[0]), DegToRad(30.0), DegToRad(120.0)) then
    Exit;

  a0a := NormalizeAngle(x[0]);
  a0b := NormalizeAngle(x[1]);
  a1a := NormalizeAngle(x[0] + Pi);
  a1b := NormalizeAngle(x[1] + Pi);

  rOuter := C45RpmOuterSize * 0.5 * FDPI;

  cx := FCenter.X;
  cy := FCenter.Y;

  for cropped := False to True do
  begin
    SetLength(acc[cropped], Ceil(rOuter));
    SetLength(cnt[cropped], Ceil(rOuter));
  end;

  for iLut := 0 to High(FCropData.RadiusAngleLut) do
  begin
    ra := @FCropData.RadiusAngleLut[iLut];
    sc := @FCropData.SinCosLut[iLut];

    bt := ra^.Angle;
    r := ra^.Radius;

    px := sc^.Cos * r + cx;
    py := sc^.Sin * r + cy;

    if InRangePointD(py, px) then
    begin
      cropped := InNormalizedAngle(bt, a0a, a0b) or InNormalizedAngle(bt, a1a, a1b);
      acc[cropped, Round(r)] += GetPointD_Linear(FLeveledImage, py, px);
      Inc(cnt[cropped, Round(r)]);
    end;
  end;

  for cropped := False to True do
    for iMean := 0 to High(cnt[cropped]) do
      acc[cropped, iMean] := DivDef(acc[cropped, iMean], cnt[cropped, iMean], 0.0);

  Result := -MAE(acc[False], acc[True]);

  //WriteLn(ImageShortName, ', begin:', RadToDeg(a0a):9:3, ', end:', RadToDeg(a0b):9:3, result:18:6);
end;

procedure TInputScan.FindCenterExtents;
const
  CCorneringThres = 7.0;

var
  startBuf: TWordDynArray;

  function DoXYBuf(var buf: TWordDynArray; xy: Integer; alongY, corr: Boolean): Double;
  var
    i: Integer;
  begin
    if alongY then
    begin
      for i := 0 to Height - 1 do
       buf[i] := FImage[i * Width + xy];
    end
    else
    begin
      for i := 0 to Width - 1 do
       buf[i] := FImage[xy * Width + i];
    end;

    Result := NaN;
    if corr then
      Result := Sqrt(MSE(startBuf, buf));
  end;

var
  radiusLimit, maxCorner, xx, yy: Integer;
  baseMSE: Double;
  cornerbuf: TWordDynArray;
begin
  // init

  radiusLimit := Round(C45RpmOuterSize * 0.5 * FDPI);

  FCenterExtents.Left := radiusLimit;
  FCenterExtents.Top := radiusLimit;
  FCenterExtents.Right := Width - radiusLimit;
  FCenterExtents.Bottom := Height - radiusLimit;

  //WriteLn(ImageShortName, FCenterExtents.Left:6,FCenterExtents.Top:6,FCenterExtents.Right:6,FCenterExtents.Bottom:6);

  // corner L/T/R/B until the record edges are reached

  if FCenterExtents.Left <> FCenterExtents.Right then
  begin
    SetLength(startBuf, Height);
    SetLength(cornerbuf, Height);
    maxCorner := Width - radiusLimit * 2 - 1;

    DoXYBuf(startBuf, 0, True, False);
    baseMSE := DoXYBuf(cornerbuf, 2, True, True);
    for xx := 0 to maxCorner do
      if (DoXYBuf(cornerbuf, xx, True, True) > CCorneringThres * baseMSE) or (xx = maxCorner) then
      begin
        FCenterExtents.Left := xx + radiusLimit;
        Break;
      end;

    DoXYBuf(startBuf, Width - 1, True, False);
    baseMSE := DoXYBuf(cornerbuf, Width - 3, True, True);
    for xx := Width - 1 downto Width - 1 - maxCorner do
      if (DoXYBuf(cornerbuf, xx, True, True) > CCorneringThres * baseMSE) or (xx = Width - 1 - maxCorner) then
      begin
        FCenterExtents.Right := xx - radiusLimit;
        Break;
      end;
  end;

  if FCenterExtents.Top <> FCenterExtents.Bottom then
  begin
    SetLength(startBuf, Width);
    SetLength(cornerbuf, Width);
    maxCorner := Height - radiusLimit * 2 - 1;

    DoXYBuf(startBuf, 0, False, False);
    baseMSE := DoXYBuf(cornerbuf, 2, False, True);
    for yy := 0 to maxCorner do
      if (DoXYBuf(cornerbuf, yy, False, True) > CCorneringThres * baseMSE) or (yy = maxCorner) then
      begin
        FCenterExtents.Top := yy + radiusLimit;
        Break;
      end;

    DoXYBuf(startBuf, Height - 1, False, False);
    baseMSE := DoXYBuf(cornerbuf, Height - 3, False, True);
    for yy := Height - 1 downto Height - 1 - maxCorner do
      if (DoXYBuf(cornerbuf, yy, False, True) > CCorneringThres * baseMSE) or (yy = Height - 1 - maxCorner) then
      begin
        FCenterExtents.Bottom := yy - radiusLimit;
        Break;
      end;
  end;

  if FCenterExtents.Left > FCenterExtents.Right then Exchange(FCenterExtents.Left, FCenterExtents.Right);
  if FCenterExtents.Top > FCenterExtents.Bottom then Exchange(FCenterExtents.Top, FCenterExtents.Bottom);

  //WriteLn(ImageShortName, FCenterExtents.Left:6,FCenterExtents.Top:6,FCenterExtents.Right:6,FCenterExtents.Bottom:6);

  FCenter.X := lerp(FCenterExtents.Left, FCenterExtents.Right, 0.5);
  FCenter.Y := lerp(FCenterExtents.Top, FCenterExtents.Bottom, 0.5);
end;

procedure TInputScan.Crop(const RadiusAngleLut: TRadiusAngleDynArray; const SinCosLut: TSinCosDynArray);
var
  X: TVector;
begin
  X := [NormalizeAngle(DegToRad(-30.0)), NormalizeAngle(DegToRad(30.0))];

  FCropData.RadiusAngleLut := RadiusAngleLut;
  FCropData.SinCosLut := SinCosLut;
  try
    NelderMeadMinimize(@NelderMeadCrop, X, [DegToRad(15.0), DegToRad(15.0)], 1e-3);
  finally
    FCropData.RadiusAngleLut := nil;
    FCropData.SinCosLut := nil;
  end;

  FCropData.StartAngle := NormalizeAngle(X[0]);
  FCropData.EndAngle := NormalizeAngle(X[1]);
  FCropData.StartAngleMirror := NormalizeAngle(X[0] + Pi);
  FCropData.EndAngleMirror := NormalizeAngle(X[1] + Pi);
end;

procedure TInputScan.FixCISScanners;

  function FindPhaseAlongX(Recurence, ReccurenceCount: Integer; out Loss: Double): Integer;
  var
    iPhase, iY, iRec, x, worstPhase: Integer;
    worstLoss, l: Double;
  begin
    worstPhase := -1;
    worstLoss := -Infinity;
    for iPhase := 0 to FWidth - 1 do
    begin
      l := 0.0;

      for iRec := 0 to ReccurenceCount do
      begin
        x := Round((iPhase mod Recurence) + Recurence * iRec);

        for iY := 0 to FHeight - 2 do
          l += Abs(FImage[iY * FWidth + x] - FImage[iY * FWidth + x + 1]);
      end;
      l := l / (FHeight * (ReccurenceCount + 1));

      if l > worstLoss then
      begin
        worstLoss := l;
        worstPhase := iPhase;
      end;
    end;

    Result := worstPhase;
    loss := worstLoss;
  end;

  function GetRadialPredictedPx(y, x, xOffset: Integer): Double;
  var
    radius, angle, centerOff, rOff, aOff, aOffPlus, aOffMinus, int, sn, cs, px, py: Double;
  begin
    Result := 0.0;
    centerOff := (x + xOffset) - FCenter.X;

    // angle / radius of the last valid point

    angle := ArcTan2(y - FCenter.Y, x - FCenter.X);
    radius := Sqrt(Sqr(y - FCenter.Y) + Sqr(x - FCenter.X));

    // angle / radius of the point to predict

    int := sqrt(Max(0, sqr(radius) - Sqr(centerOff)));
    aOffPlus := ArcTan2(int, centerOff);
    aOffMinus := ArcTan2(-int, centerOff);
    if Abs(angle - aOffMinus) < Abs(angle - aOffPlus) then
      aOff := aOffMinus
    else
      aOff := aOffPlus;

    rOff := Sqrt(Sqr(y - FCenter.Y) + Sqr(centerOff));

    // predict mirrored

    radius := rOff;
    angle := angle + NormalizedAngleDiff(aOff, angle);

    // back to cartesian coords

    SinCos(angle, sn, cs);
    px := cs * radius + FCenter.X;
    py := sn * radius + FCenter.Y;

    if InRangePointD(py, px) then
      Result := GetPointD_Sinc(FImage, py, px)
  end;

  procedure Resample(X1, X2, FixLen: Integer);
  var
    iSerp, y, x, iPred, yoff, xii, leftFix, rightFix: Integer;
    rt, alpha, xi: Double;
    pred: TDoubleDynArray2;
    serpData: TSerpCoeffs9;
  begin
    leftFix := FixLen div 2;
    rightFix := FixLen - leftFix;

    SetLength(pred, FHeight, X2 - X1 + FixLen);

    for y := 0 to FHeight - 1 do
    begin
      yoff := y * FWidth;

      for x := X1 to X2 - 1 do
        pred[y, x - X1 + leftFix] := FImage[yoff + x];

      for iPred := 0 to leftFix - 1 do
        pred[y, iPred] := GetRadialPredictedPx(y, X1, iPred - leftFix);

      for iPred := 0 to rightFix - 1 do
        pred[y, Length(pred[y]) - rightFix + iPred] := GetRadialPredictedPx(y, X2 - 1, iPred + 1);
    end;

    rt := (X2 - X1 + FixLen) / (X2 - X1);
    for y := 0 to FHeight - 1 do
    begin
      yoff := y * FWidth;

      for x := X1 to X2 - 1 do
      begin
        xi := (x - X1) * rt;
        xii := Trunc(xi);
        alpha := xi - xii;

        for iSerp := Low(TSerpCoeffs9) to -Low(TSerpCoeffs9) do
          if not InRange(xii + iSerp, 0, High(pred[y])) then
            serpData[iSerp] := FImage[yoff + EnsureRange(X1 + xii + iSerp, 0, FWidth - 1)]
          else
            serpData[iSerp] := pred[y, xii + iSerp];

        FImage[yoff + x] := EnsureRange(Round(serpFromCoeffs(serpCoeffs(alpha), @serpData[0])), 0, High(Word));
      end;
    end;

  end;

const
  C2400DPIRecurence = 1728;
  C2400DPIOffset = 5;
var
  iRec, phase, recurence, recCnt, offset, x1, x2: Integer;
  loss: Double;
begin
  recurence := (C2400DPIRecurence * FDPI) div 2400;
  offset := (C2400DPIOffset * FDPI) div 2400;
  recCnt := (FWidth - 1) div Recurence;

  phase := FindPhaseAlongX(recurence, recCnt, loss);
  WriteLn(ImageShortName, phase:8, loss:12:3);

  for iRec := 0 to recCnt - 1 do
  begin
    x1 := phase + recurence * (iRec + 0) + 1;
    x2 := phase + recurence * (iRec + 1) + 1;

    Resample(x1, x2, offset);
  end;
end;

function TInputScan.InRangePointD(Y, X: Double): Boolean;
begin
  Result := InRange(Y, 8, Height - 10) and InRange(X, 8, Width - 10);
end;

function TInputScan.GetPointD_Linear(const Image: TWordDynArray; Y, X: Double): Double;
var
  ix, iy, yx: Integer;
  y1, y2: Double;
begin
  ix := trunc(X);
  iy := trunc(Y);

  yx := iy * FWidth + ix;

  y1 := lerp(Image[yx], Image[yx + 1], X - ix);
  y2 := lerp(Image[yx + FWidth], Image[yx + FWidth + 1], X - ix);

  Result := lerp(y1, y2, Y - iy);
end;

procedure TInputScan.GetGradientsD(const Image: TWordDynArray; Y, X: Double; out GY: Double; out GX: Double);
var
  i, iy, ix, xy, iw: Integer;
  lG00, lG01, lG10, lG11, lgx, lgy, fdy: Double;
begin
  ix := trunc(X);
  iy := trunc(Y);

  xy := iy * FWidth + ix;

  lgx := 0.0;
  lgy := 0.0;

  lG00 := 0.0; lG01 := 0.0; lG10 := 0.0; lG11 := 0.0;
  for i := Low(CFiniteDifferencesYFactor) to High(CFiniteDifferencesYFactor) do
  begin
    if i = 0 then
      Continue;

    fdy := CFiniteDifferencesYFactor[i];

    lG00 += Image[xy + i] * fdy;
    lG01 += Image[xy + 1 + i] * fdy;
    lG10 += Image[xy + FWidth + i] * fdy;
    lG11 += Image[xy + FWidth + 1 + i] * fdy;
  end;
  lgx += lerp(lerp(lG00, lG01, X - ix), lerp(lG10, lG11, X - ix), Y - iy);

  lG00 := 0.0; lG01 := 0.0; lG10 := 0.0; lG11 := 0.0;
  for i := Low(CFiniteDifferencesYFactor) to High(CFiniteDifferencesYFactor) do
  begin
    if i = 0 then
      Continue;

    iw := i * FWidth;
    fdy := CFiniteDifferencesYFactor[i];

    lG00 += Image[xy + iw] * fdy;
    lG01 += Image[xy + 1 + iw] * fdy;
    lG10 += Image[xy + FWidth + iw] * fdy;
    lG11 += Image[xy + FWidth + 1 + iw] * fdy;
  end;
  lgy += lerp(lerp(lG00, lG01, X - ix), lerp(lG10, lG11, X - ix), Y - iy);

  GX := lgx;
  GY := lgy;
end;

function TInputScan.GetPointD_Sinc(const Image: TWordDynArray; Y, X: Double): Single;
var
  ix, iy: Integer;
  intData: TSerpCoeffs9;
  coeffsX, coeffsY: PSingle;
begin
  ix := trunc(X);
  iy := trunc(Y);

  coeffsX := serpCoeffs(X - ix);
  coeffsY := serpCoeffs(Y - iy);

  serpFromCoeffsXY(coeffsX, @Image[iy * FWidth + ix], FWidth, @intData[0]);

  Result := serpFromCoeffs(coeffsY, @intData[0]);
end;

procedure TInputScan.AddCorrectRef(AngleIdx, ScanIdx: Integer);
var
  cr: TCorrectRef;
begin
  cr.AngleIdx := AngleIdx;
  cr.ScanIdx := ScanIdx;

  SpinEnter(@FLock);
  try
    SetLength(FCorrectRefs, Length(FCorrectRefs) + 1);
    FCorrectRefs[High(FCorrectRefs)] := cr;
  finally
    SpinLeave(@FLock);
  end;
end;

function TInputScan.HasCorrectRef(const AList: TInputScanDynArray; AngleIdx, ScanIdx: Integer): Boolean;

  procedure Recurse(AScan: TInputScan);
  var
    i: Integer;
    cr: TCorrectRef;
    toRecurse: array of TInputScan;
  begin
    if Result then Exit;

    SpinEnter(@AScan.FLock);
    try
      for i := 0 to High(AScan.FCorrectRefs) do
      begin
        cr := AScan.FCorrectRefs[i];

        if (cr.AngleIdx = AngleIdx) and (cr.ScanIdx = ScanIdx) then
        begin
          Result := True;
          Break;
        end;

        if cr.AngleIdx = AngleIdx then
        begin
          SetLength(toRecurse, Length(toRecurse) + 1);
          toRecurse[High(toRecurse)] := AList[cr.ScanIdx];
        end;
      end;
    finally
      SpinLeave(@AScan.FLock);
    end;

    for i := 0 to High(toRecurse) do
      Recurse(toRecurse[i]);
  end;

begin
  Result := False;
  Recurse(Self);
end;

{ TScanImage }

procedure TScanImage.SetInternalPixel(x, y: integer; Value: integer);
begin
  // not needed
end;

function TScanImage.GetInternalPixel(x, y: integer): integer;
begin
  Result := FImage[y * Width + x];
end;

procedure TScanImage.SetInternalColor(x, y: integer; const Value: TFPColor);
begin
  FImage[y * Width + x] := ToLuma(Value.Red, Value.Green, Value.Blue) div cLumaDiv;
end;

{ TDPIAwareReaderPNG }

procedure TDPIAwareReaderPNG.HandleChunk;
begin
  inherited HandleChunk;

  if (Chunk.aType = ctpHYs) and (Chunk.data^[8] = $01) then
  begin
    FDPI.X := Round(BEtoN(PCardinal(@Chunk.data^[0])^) * 0.0254);
    FDPI.Y := Round(BEtoN(PCardinal(@Chunk.data^[4])^) * 0.0254);
  end;
end;

constructor TDPIAwareReaderPNG.Create;
begin
  inherited create;

  FDPI := Point(-1, -1);
end;

end.

