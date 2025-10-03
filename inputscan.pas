unit inputscan;

{$include 'compileroptions.inc'}

interface

uses
  Classes, SysUtils, Types, Math, Graphics, FPReadTiff, FPImage, PNGComn, MTProcs,
  utils, profiles, powell, hackedreadpng;

type
  TInputScan = class;

  TStencil = record
    X: array[TValueSign] of Double;
    Y: array[TValueSign] of Integer;
  end;

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
    FProfileRef: TProfile;
    FImageFileName: String;
    FDPI: Integer;
    FPointsPerRevolution: Integer;
    FRadiansPerRevolutionPoint: Double;
    FSilent: Boolean;
    FSinCosLUT: TSinCosDynArray;

    FCenterExtents: TRect;
    FCenter: TPointD;
    FSkew: TPointD;
    FConcentricGrooveRadius: Double;
    FSetDownRadius: Double;
    FGrooveStartAngle: Double;
    FGrooveStartAngleQuality: Double;
    FGrooveStartPoint: TPointD;

    FRelativeAngle: Double;
    FCropData: TCropData;
    FCenterQuality: Double;
    FObjective: Double;

    FWidth, FHeight: Integer;
    FImage: TWordDynArray;
    FProcessedImage: TWordDynArray;

    FCorrectRefs: array of TCorrectRef;
    FLock: TSpinlock;

    procedure SetRevolutionFromDPI(ADPI: Integer);
    procedure SetRevolutionFromSampleRate(ASampleRate: Integer);
    function GetImageShortName: String;
    function MakeStencil: TStencil;

    procedure GradientConcentricGroove(const arg: TDoubleDynArray; var func: Double; grad: TDoubleDynArray; obj: Pointer);
    function NelderMeadCrop(const x: TVector; obj: Pointer): TScalar;
    function MakeRadiusOffsets(ARadius: Integer): TIntegerDynArray;

    procedure FindCenterExtents;
    procedure FindConcentricGroove_GridSearch;
    procedure FindConcentricGroove_Gradient;
    procedure FindCroppedArea;
    procedure FindGrooveStart;

    procedure LoadPNG;
    procedure LoadTIFF;
  public
    constructor Create(AProfileRef: TProfile; ADefaultDPI: Integer = 2400; ASilent: Boolean = False);
    destructor Destroy; override;

    procedure InitImage(AWidth, AHeight, ADPI: Integer);
    procedure LoadImage;
    procedure FixCISScanners;
    procedure Blur;
    procedure FindTrack(AUseGradient, AFindCroppedArea: Boolean; AForcedSampleRate: Integer = -1);
    procedure CorrectByModel(ARelativeAngle, ACenterX, ACenterY, ASkewX, ASkewY: Double);
    procedure Crop(const RadiusAngleLut: TRadiusAngleDynArray; const SinCosLut: TSinCosDynArray);
    procedure DrawTrack;
    procedure Linearize;

    function InRangePointD(Y, X: Double): Boolean; inline;
    function GetPointD_Work(const Image: TWordDynArray; Y, X: Double): Double; inline;
    function GetPointD_Final(const Image: TWordDynArray; Y, X: Double): Double; inline;
    function GetMeanSD(const Image: TWordDynArray; AStartRadius, AEndRadius, AStartAngle, AEndAngle, ASigma: Double): TPointD;
    procedure GetGradientsD(const Image: TWordDynArray; Y, X: Double; out GY: Double; out GX: Double);

    function DecodeSample(precision: Integer; radius, prevRadius, angleSin, angleCos: Double; out sampleMeanSD: TPointD): TPointD;

    procedure AddCorrectRef(AngleIdx, ScanIdx: Integer);
    function HasCorrectRef(const AList: TInputScanDynArray; AngleIdx, ScanIdx: Integer): Boolean;

    property ProfileRef: TProfile read FProfileRef;
    property ImageFileName: String read FImageFileName write FImageFileName;
    property ImageShortName: String read GetImageShortName;

    property DPI: Integer read FDPI;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;

    property ConcentricGrooveRadius: Double read FConcentricGrooveRadius;
    property SetDownRadius: Double read FSetDownRadius;
    property GrooveStartAngle: Double read FGrooveStartAngle;
    property GrooveStartAngleQuality: Double read FGrooveStartAngleQuality;
    property GrooveStartPoint: TPointD read FGrooveStartPoint;
    property PointsPerRevolution: Integer read FPointsPerRevolution;
    property RadiansPerRevolutionPoint: Double read FRadiansPerRevolutionPoint;

    property CenterExtents: TRect read FCenterExtents;
    property Center: TPointD read FCenter;
    property Skew: TPointD read FSkew;
    property RelativeAngle: Double read FRelativeAngle;
    property CropData: TCropData read FCropData;

    property CenterQuality: Double read FCenterQuality;
    property Objective: Double read FObjective write FObjective;

    property Image: TWordDynArray read FImage;
    property ProcessedImage: TWordDynArray read FProcessedImage;
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
  FPointsPerRevolution := Ceil(Pi * FProfileRef.OuterSize * ADPI);
  FRadiansPerRevolutionPoint := Pi * 2.0 / FPointsPerRevolution;
end;

procedure TInputScan.SetRevolutionFromSampleRate(ASampleRate: Integer);
begin
  FPointsPerRevolution := Ceil(ASampleRate / FProfileRef.RevolutionsPerSecond);
  FRadiansPerRevolutionPoint := Pi * 2.0 / FPointsPerRevolution;
end;

function TInputScan.GetImageShortName: String;
begin
  Result := ChangeFileExt(ExtractFileName(FImageFileName), '');
end;

function TInputScan.MakeStencil: TStencil;
begin
  Result.Y[NegativeValue] := -1;
  Result.Y[ZeroValue] := 2;
  Result.Y[PositiveValue] := -1;

  Result.X[NegativeValue] := -FProfileRef.LeadOutGrooveThickness * 0.5 * FDPI;
  Result.X[ZeroValue] := 0;
  Result.X[PositiveValue] := FProfileRef.LeadOutGrooveThickness * 0.5 * FDPI;
end;

procedure TInputScan.FindConcentricGroove_GridSearch;
const
  CPointsPerRevolution = 512;
var
  extents: TRect;
  sinCosLUT: TSinCosDynArray;
  mnRadius, mxRadius: Integer;
  stencil: TStencil;
  results: array of record
    Objective: Int64;
    X: TVector;
  end;

  procedure DoSkew(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    cx, cy, iLut, px, py, ff, resIdx: Integer;
    r: Double;
    f: Int64;
    vs: TValueSign;
    pxArr, pyArr: array[TValueSign, 0 .. CPointsPerRevolution div 4 - 1] of Integer;
  begin
    if not InRange(AIndex, mnRadius, mxRadius) then
      Exit;

    r := AIndex;
    resIdx := AIndex - mnRadius;
    results[resIdx].Objective := Low(Int64);

    for vs := Low(TValueSign) to High(TValueSign) do
      for iLut := 0 to High(sinCosLUT) do
      begin
        pxArr[vs, ilut] := round(sinCosLUT[iLut].Cos * (r + stencil.X[vs]));
        pyArr[vs, ilut] := round(sinCosLUT[iLut].Sin * (r + stencil.X[vs]))
      end;

    for cy := extents.Top to extents.Bottom do
      for cx := extents.Left to extents.Right do
      begin
        f := 0;
        for vs := Low(TValueSign) to High(TValueSign) do
        begin
          ff := 0;

          for iLut := 0 to High(sinCosLUT) do
          begin
            px := pxArr[vs, iLut];
            py := pyArr[vs, iLut];

            ff += FProcessedImage[(cy - py) * Width + cx + px];
            ff += FProcessedImage[(cy - py) * Width + cx - px];
            ff += FProcessedImage[(cy + py) * Width + cx + px];
            ff += FProcessedImage[(cy + py) * Width + cx - px];
          end;

          f += ff * stencil.Y[vs];
        end;

        if f > results[resIdx].Objective then
        begin
          results[resIdx].Objective := f;
          results[resIdx].X := [cx, cy, r];
        end;
      end;
  end;

var
  iRes: Integer;
  bestf: Int64;
  X: TDoubleDynArray;
begin
  // grid search algorithm to find the concentric groove

  BuildSinCosLUT(CPointsPerRevolution div 4, sinCosLUT, 0.0, Pi / 2.0);

  stencil := MakeStencil;

  extents := FCenterExtents;
  extents.Inflate(extents.Right - extents.CenterPoint.X, extents.Bottom - extents.CenterPoint.Y);

  X := [FCenter.X, FCenter.Y, FConcentricGrooveRadius];

  mnRadius := Floor(FProfileRef.MinConcentricGroove * FDPI * 0.5);
  mxRadius := Ceil(FProfileRef.MaxConcentricGroove * FDPI * 0.5);
  SetLength(results, mxRadius - mnRadius + 1);
  ProcThreadPool.DoParallelLocalProc(@DoSkew, mnRadius, mxRadius);

  bestf := Low(Int64);
  for iRes := 0 to High(results) do
    if results[iRes].Objective > bestf then
    begin
      bestf := results[iRes].Objective;
      X := results[iRes].X;
    end;

  FCenter.X := X[0];
  FCenter.Y := X[1];
  FConcentricGrooveRadius := X[2];
  FCenterQuality := bestf / High(Word);

  //WriteLn(ImageShortName, FCenter.X:12:3, FCenter.Y:12:3, FConcentricGrooveRadius:12:3, FCenterQuality:12:0);
end;

procedure TInputScan.GradientConcentricGroove(const arg: TDoubleDynArray; var func: Double; grad: TDoubleDynArray;
  obj: Pointer);
var
  meanSD: ^TPointD absolute obj;
  iLut: Integer;
  px, py, cx, cy, r, sky, cs, sn, imgInt, gInt, gimgx, gimgy, gcx, gcy, gr, gsky, rsx, sy: Double;
  stencil: TStencil;
  vs: TValueSign;
begin
  cx := arg[0];
  cy := arg[1];
  r := arg[2];
  sky := arg[3];

  func := 1e6;
  if not InRange(sky, CScannerTolLo, CScannerTolHi) then
    Exit;

  stencil := MakeStencil;

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
      rsx := r + stencil.X[vs];
      sy := stencil.Y[vs];

      px := cs * rsx + cx;
      py := (sn * rsx + cy) * sky;

      if InRangePointD(py, px) then
      begin
        imgInt := CompressRange((GetPointD_Work(FProcessedImage, py, px) - meanSD^.X) * meanSD^.Y);

        func -= imgInt * sy;

        if Assigned(grad) then
        begin
          GetGradientsD(FProcessedImage, py, px, gimgy, gimgx);

          gInt := meanSD^.Y * sy * (1.0 - Sqr(imgInt));

          gcx -= gimgx * gInt;
          gcy -= gimgy * gInt;
          gr -= (gimgx * cs + gimgy * sn * sky) * gInt;
          gsky -= gimgy * (sn * rsx + cy) * gInt;
        end;
      end;
    end;
  end;

  if Assigned(grad) then
  begin
    grad[0] := gcx;
    grad[1] := gcy;
    grad[2] := gr;
    grad[3] := gsky;
  end;
end;

procedure TInputScan.FindConcentricGroove_Gradient;
var
  ff: Double;
  meanSD: TPointD;
  X: TDoubleDynArray;
begin
  BuildSinCosLUT(Ceil(Pi * FProfileRef.ConcentricGroove * FDPI), FSinCosLUT);

  meanSD := GetMeanSD(FProcessedImage, FProfileRef.MinConcentricGroove * 0.5 * FDPI, FProfileRef.MaxConcentricGroove * 0.5 * FDPI, -Pi, Pi, 1.0);

  X := [FCenter.X, FCenter.Y, FConcentricGrooveRadius, 1.0];

  ff := LBFGSScaledMinimize(@GradientConcentricGroove, X, [1.0, 1.0, 1.0, 1e-4], 1e-6, 4, @meanSD);

  FCenter.X := X[0];
  FCenter.Y := X[1];
  FConcentricGrooveRadius := X[2];
  FSkew.Y := X[3];
  FCenterQuality := -ff;
end;

procedure TInputScan.FindGrooveStart;
var
  i: Integer;
  v, best, sn, cs, bestr, x, y, bestx, besty, angle: Double;
  hasCrop: Boolean;
  vs: TValueSign;
  stencil: TStencil;
  sliding: array[TValueSign] of Double;
begin
  best := -Infinity;
  bestx := 0;
  besty := 0;
  bestr := 0;
  for vs := Low(TValueSign) to High(TValueSign) do
    sliding[vs] := 0;

  hasCrop := (NormalizedAngleDiff(FCropData.StartAngle, FCropData.EndAngle) <> 0) or
      (NormalizedAngleDiff(FCropData.StartAngleMirror, FCropData.EndAngleMirror) <> 0);

  stencil := MakeStencil;

  for i := 0 to FPointsPerRevolution - 1  do
  begin
    angle := NormalizeAngle(i * FRadiansPerRevolutionPoint);

    if hasCrop and
        (InNormalizedAngle(angle, FCropData.StartAngle, FCropData.EndAngle) or
        InNormalizedAngle(angle, FCropData.StartAngleMirror, FCropData.EndAngleMirror)) then
    begin
      for vs := Low(TValueSign) to High(TValueSign) do
        sliding[vs] := 0;
      Continue;
    end;

    SinCos(i * FRadiansPerRevolutionPoint, sn, cs);

    for vs := Low(TValueSign) to High(TValueSign) do
    begin
      x := (cs * (FSetDownRadius + stencil.X[vs]) + FCenter.X) * FSkew.X;
      y := (sn * (FSetDownRadius + stencil.X[vs]) + FCenter.Y) * FSkew.Y;

      if InRangePointD(y, x) then
        sliding[vs] := sliding[vs] * 0.99 + GetPointD_Final(FImage, y, x) * 0.01;
    end;

    v := 0;
    for vs := Low(TValueSign) to High(TValueSign) do
      v += sliding[vs] * stencil.Y[vs];

    if v > best then
    begin
      best := v;
      bestx := x;
      besty := y;
      bestr := angle;
    end;

    //writeln(i:6,x:8,y:8,sliding:9:3,best:9:3,bestx:8,besty:8);
  end;

  FGrooveStartAngleQuality := best;
  FGrooveStartAngle := bestr;
  FGrooveStartPoint.X := bestx;
  FGrooveStartPoint.Y := besty;
end;

constructor TInputScan.Create(AProfileRef: TProfile; ADefaultDPI: Integer; ASilent: Boolean);
begin
  FProfileRef := AProfileRef;
  FDPI := ADefaultDPI;
  FSilent := ASilent;
  FCenterQuality := -Infinity;
  FGrooveStartAngleQuality := -Infinity;
  FObjective := Infinity;
  FSkew.X := 1.0;
  FSkew.Y := 1.0;

  SetRevolutionFromDPI(FDPI);
end;

destructor TInputScan.Destroy;
begin
  inherited Destroy;
end;

procedure TInputScan.InitImage(AWidth, AHeight, ADPI: Integer);
begin
  FDPI := ADPI;
  FWidth := AWidth;
  FHeight := AHeight;
  SetLength(FImage, FHeight * FWidth);
  FProcessedImage := FImage;
  FCenter.X := AWidth * 0.5;
  FCenter.Y := AHeight * 0.5;

  FindCenterExtents;
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

    FProcessedImage := FImage;
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

    FProcessedImage := FImage;
  finally
    tiff.Free;
    fs.Free;
  end;
end;

procedure TInputScan.Blur;
const
  CPica = 0.03; // inches
var
  labelRadius, lblPos, cx, cy: Integer;
  labelOffsets: TIntegerDynArray;
  srcImage: TWordDynArray;

  function GetMean(const offsets: TIntegerDynArray; ayx: Integer): Integer;
  var
    i: Integer;
    px: Integer;
  begin
    Result := 0;
    for i := 0 to High(offsets) do
    begin
      px := srcImage[ayx + offsets[i]];
      Result += px;
    end;
    Result := Result div Length(offsets);
  end;

  procedure DoY(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    x, y, yx, sqy: Integer;
  begin
    if not InRange(AIndex, 0, FHeight - 1) then
      Exit;

    y := AIndex;
    sqy := Sqr(y - cy);

    for x := 0 to FWidth - 1 do
    begin
      yx := y * Width + x;

      if sqy + Sqr(x - cx) <= lblPos then
        FProcessedImage[yx] := GetMean(labelOffsets, yx)
      else
        FProcessedImage[yx] := srcImage[yx];
    end;
  end;

begin
  if not FSilent then WriteLn('Blur');

  cx := Round(FCenter.X);
  cy := Round(FCenter.Y);
  lblPos := Round(Sqr(FProfileRef.MinConcentricGroove * 0.5 * FDPI));

  labelRadius := Ceil(CPica * FDPI);
  labelOffsets := MakeRadiusOffsets(labelRadius);

  srcImage := FImage;
  FProcessedImage := nil;
  SetLength(FProcessedImage, Height * Width);

  ProcThreadPool.DoParallelLocalProc(@DoY, 0, FHeight - 1);
end;


procedure TInputScan.FindTrack(AUseGradient, AFindCroppedArea: Boolean; AForcedSampleRate: Integer);
begin
  if not FSilent then WriteLn('FindTrack');

  if AForcedSampleRate >= 0 then
    SetRevolutionFromSampleRate(AForcedSampleRate)
  else
    SetRevolutionFromDPI(FDPI);

  FSetDownRadius := lerp(FProfileRef.FirstMusicGroove, FProfileRef.StylusSetDown, 0.0) * FDPI * 0.5;
  FConcentricGrooveRadius := FProfileRef.ConcentricGroove * FDPI * 0.5;

  FindConcentricGroove_GridSearch;
  if AUseGradient then
    FindConcentricGroove_Gradient;
  if AFindCroppedArea then
    FindCroppedArea;
  FindGrooveStart;

  if not FSilent then
  begin
    WriteLn('Center:', FCenter.X:12:3, ',', FCenter.Y:12:3);
    WriteLn('Skew:', FSkew.X:12:6, ',', FSkew.Y:12:6);
    WriteLn('ConcentricGrooveRadius:', FConcentricGrooveRadius:12:3);
    WriteLn('CenterQuality:', FCenterQuality:12:6);
    WriteLn('SetDownRadius:', FSetDownRadius:12:3);
    WriteLn('GrooveStartPoint:', FGrooveStartPoint.X:12:3, ',', FGrooveStartPoint.Y:12:3);
    Writeln('Inner raw sample rate: ', Round(Pi * FProfileRef.LastMusicGroove * FDPI * FProfileRef.RevolutionsPerSecond), ' Hz');
  end
  else
  begin
    WriteLn(ImageShortName, ', CenterX:', FCenter.X:12:3, ', CenterY:', FCenter.Y:12:3, ', SkewY:', FSkew.Y:12:6, ', ConcentricGroove:', FConcentricGrooveRadius:10:3, ', Quality:', FCenterQuality:12:3);
  end;
end;

procedure TInputScan.CorrectByModel(ARelativeAngle, ACenterX, ACenterY, ASkewX, ASkewY: Double);
begin
  if not IsNan(ARelativeAngle) then FRelativeAngle := ARelativeAngle;
  if not IsNan(ACenterX) then FCenter.X := ACenterX;
  if not IsNan(ACenterY) then FCenter.Y := ACenterY;
  if not IsNan(ASkewX) then FSkew.X := ASkewX;
  if not IsNan(ASkewY) then FSkew.Y := ASkewY;
end;

procedure TInputScan.FindCenterExtents;
const
  CCorneringThres = 3.0;
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

  radiusLimit := Round(FProfileRef.OuterSize * 0.5 * FDPI);

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

function TInputScan.NelderMeadCrop(const x: TVector; obj: Pointer): TScalar;
var
  a0a, a1a, a0b, a1b, cx, cy, skx, sky: Double;

  procedure GetCoords(iLut: Integer; out py, px: Double; out cropped: Boolean);
  var
    bt, r: Double;
    ra: ^TRadiusAngle;
    sc: ^TSinCos;
  begin
    ra := @FCropData.RadiusAngleLut[iLut];
    sc := @FCropData.SinCosLut[iLut];

    bt := ra^.Angle;
    r := ra^.Radius;

    px := (sc^.Cos * r + cx) * skx;
    py := (sc^.Sin * r + cy) * sky;

    cropped := InNormalizedAngle(bt, a0a, a0b) or InNormalizedAngle(bt, a1a, a1b);
  end;

const
  CMinCrop = 30.0;
  CMaxCrop = 120.0;
var
  iLut: Integer;
  px, py: Double;
  cropped: Boolean;
  cnt: array[Boolean] of Integer;
  acc: array[Boolean, Boolean] of TDoubleDynArray;
  corr: array[Boolean] of Double;
begin
  Result := 1e6;

  if not InRange(NormalizeAngle(x[1] - x[0]), DegToRad(CMinCrop), DegToRad(CMaxCrop)) or
     not InRange(NormalizeAngle(x[3] - x[2]), DegToRad(CMinCrop), DegToRad(CMaxCrop)) then
    Exit;

  a0a := NormalizeAngle(x[0]);
  a0b := NormalizeAngle(x[1]);
  a1a := NormalizeAngle(x[2]);
  a1b := NormalizeAngle(x[3]);

  cx := FCenter.X;
  cy := FCenter.Y;
  skx := FSkew.X;
  sky := FSkew.Y;

  for cropped := False to True do
  begin
    SetLength(acc[cropped, False], Length(FCropData.RadiusAngleLut));
    SetLength(acc[cropped, True], Length(FCropData.RadiusAngleLut));
    cnt[cropped] := 0;
  end;

  for iLut := 0 to High(FCropData.RadiusAngleLut) do
  begin
    GetCoords(iLut, py, px, cropped);
    acc[cropped, False, cnt[cropped]] := GetPointD_Work(FImage, py, px);
    acc[cropped, True, cnt[cropped]] := GetPointD_Work(FProcessedImage, py, px);
    Inc(cnt[cropped]);
  end;

  for cropped := False to True do
  begin
    SetLength(acc[cropped, False], cnt[cropped]);
    SetLength(acc[cropped, True], cnt[cropped]);
    corr[cropped] := PearsonCorrelation(acc[cropped, False], acc[cropped, True]);
  end;

  Result := corr[True] - corr[False];

  //WriteLn(ImageShortName, ', begin:', RadToDeg(a0a):9:3, ', end:', RadToDeg(a0b):9:3, result:18:6);
end;

function TInputScan.MakeRadiusOffsets(ARadius: Integer): TIntegerDynArray;
var
  x, y, r, pos: Integer;
begin
  SetLength(Result, Sqr(ARadius * 2 + 1));
  pos := 0;
  for y := -ARadius to ARadius do
    for x := -ARadius to ARadius do
    begin
      r := round(Sqrt(Sqr(y) + Sqr(x)));

      if r <= ARadius then
      begin
        Result[pos] := y * Width + x;
        Inc(pos);
      end;
    end;
  SetLength(Result, pos);
end;

procedure TInputScan.Crop(const RadiusAngleLut: TRadiusAngleDynArray; const SinCosLut: TSinCosDynArray);
var
  X: TVector;
begin
  X := [NormalizeAngle(DegToRad(-30.0)), NormalizeAngle(DegToRad(30.0)), NormalizeAngle(DegToRad(-30.0) + Pi), NormalizeAngle(DegToRad(30.0) + Pi)];

  FCropData.RadiusAngleLut := RadiusAngleLut;
  FCropData.SinCosLut := SinCosLut;
  try
    NelderMeadMinimize(@NelderMeadCrop, X, [DegToRad(15.0), DegToRad(15.0), DegToRad(15.0), DegToRad(15.0)], 1e-3);
  finally
    FCropData.RadiusAngleLut := nil;
    FCropData.SinCosLut := nil;
  end;

  FCropData.StartAngle := NormalizeAngle(X[0]);
  FCropData.EndAngle := NormalizeAngle(X[1]);
  FCropData.StartAngleMirror := NormalizeAngle(X[2]);
  FCropData.EndAngleMirror := NormalizeAngle(X[3]);
end;

procedure TInputScan.FindCroppedArea;
const
  CAngleCount = 720;
  CAngleMargin = 3;
var
  iAngle, iRadius, beginRadius, endRadius: Integer;
  toRad, angle, sn, cs: Double;
  prevAngleCropped, isMirror: Boolean;
  angleCropped: TBooleanDynArray;
  angleData: TDoubleDynArray;
begin
  beginRadius := Ceil(FProfileRef.LastMusicGroove * 0.5 * FDPI);
  endRadius := Floor(FProfileRef.FirstMusicGroove * 0.5 * FDPI);

  SetLength(angleCropped, CAngleCount);
  SetLength(angleData, endRadius - beginRadius + 1);

  toRad := 2.0 * Pi / Length(angleCropped);

  for iAngle := 0 to High(angleCropped) do
  begin
    angle := iAngle * toRad;
    SinCos(angle, sn, cs);

    for iRadius := beginRadius to endRadius do
      angleData[iRadius - beginRadius] := GetPointD_Work(FImage, (sn * iRadius + FCenter.Y) * FSkew.Y, (cs * iRadius + FCenter.X) * FSkew.X);

    angleCropped[iAngle] := IsZero(StdDev(angleData));
  end;

  isMirror := False;
  prevAngleCropped := angleCropped[High(angleCropped)];
  for iAngle := 0 to High(angleCropped) do
  begin
    if angleCropped[iAngle] and not prevAngleCropped then
    begin
      angle := NormalizeAngle((iAngle - CAngleMargin) * toRad);

      if isMirror then
        FCropData.StartAngleMirror := angle
      else
        FCropData.StartAngle := angle;
    end
    else if not angleCropped[iAngle] and prevAngleCropped then
    begin
      angle := NormalizeAngle((iAngle + CAngleMargin) * toRad);

      if isMirror then
        FCropData.EndAngleMirror := angle
      else
        FCropData.EndAngle := angle;

      isMirror := not isMirror;
    end;

    prevAngleCropped := angleCropped[iAngle];
  end;
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
      Result := GetPointD_Final(FImage, py, px)
  end;

  procedure Resample(X1, X2, FixLen: Integer);
  var
    iHerp, y, x, iPred, yoff, xii, leftFix, rightFix: Integer;
    rt, alpha, xi: Double;
    pred: TDoubleDynArray2;
    herpData: array[-1 .. 2] of Double;
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

        for iHerp := Low(herpData) to High(herpData) do
          if not InRange(xii + iHerp, 0, High(pred[y])) then
            herpData[iHerp] := FImage[yoff + EnsureRange(X1 + xii + iHerp, 0, FWidth - 1)]
          else
            herpData[iHerp] := pred[y, xii + iHerp];

        FImage[yoff + x] := EnsureRange(Round(herp(herpData[-1], herpData[0], herpData[1], herpData[2], alpha)), 0, High(Word));
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
  if not FSilent then WriteLn('FixCISScanners');

  recurence := (C2400DPIRecurence * FDPI) div 2400;
  offset := (C2400DPIOffset * FDPI) div 2400;
  recCnt := (FWidth - 1) div Recurence;

  phase := FindPhaseAlongX(recurence, recCnt, loss);
  WriteLn(ImageShortName, ', Phase: ', phase:8, ', Loss: ', loss:12:3);

  for iRec := 0 to recCnt - 1 do
  begin
    x1 := phase + recurence * (iRec + 0) + 1;
    x2 := Min(FWidth - 1, phase + recurence * (iRec + 1) + 1);

    Resample(x1, x2, offset);
  end;
end;

procedure TInputScan.DrawTrack;
const
  CDecoderPrecision = 2;
  CMinGrooveWidth = 0.001; // inches
  CPixelValue = High(Word) - 1;
  CPrevPositions = 4;

  function FindGrooveRadiuses(AAngle: Double): TDoubleDynArray;
  var
    iRadius, rBeg, rEnd, pos, inGrooveCnt, minGrooveWidth, lastGap: Integer;
    sn, cs, px: Double;
    meanSd: TPointD;
  begin
    SinCos(AAngle, sn, cs);

    rBeg := Floor(FProfileRef.MinConcentricGroove * 0.5 * FDPI);
    rEnd := Ceil(FProfileRef.OuterSize * 0.5 * FDPI);
    minGrooveWidth := Ceil(CMinGrooveWidth * FDPI);

    meanSd := GetMeanSD(FImage,
        rBeg, rEnd,
        NormalizeAngle(AAngle - FRadiansPerRevolutionPoint * 0.5),
        NormalizeAngle(AAngle + FRadiansPerRevolutionPoint * 0.5),
        1.0);

    SetLength(Result, rEnd - rBeg + 1);

    pos := 0;
    inGrooveCnt := 0;
    lastGap := rBeg;
    for iRadius := rBeg to rEnd do
    begin
      px := GetPointD_Final(FImage, (iRadius * sn + FCenter.Y) * FSkew.Y, (iRadius * cs + FCenter.X) * FSkew.X);

      if px > meanSd.X then
      begin
        Inc(inGrooveCnt);
      end
      else
      begin
        if inGrooveCnt >= minGrooveWidth then
        begin
          Result[pos] := lastGap + inGrooveCnt * 0.5;
          Inc(pos);
        end;

        lastGap := iRadius;
        inGrooveCnt := 0;
      end;
    end;

    SetLength(Result, pos);
  end;

var
  iPP, iRadius, iStart, iCur, dir, px, py, yx, pxCount: Integer;
  sn, cs, r, fbRatio, rInc, prevRadius: Double;
  allDone, isPrevPosition: Boolean;
  meanSd: TPointD;

  startData: array[0 .. 1] of record
    Angle: Double;
    Radiuses: TDoubleDynArray;
  end;

  curData: array[0 .. 3] of record
    Angle: Double;
    AngleInc: Double;
    PrevPositionsPos: Integer;
    Done: TBooleanDynArray;
    Radius: TDoubleDynArray;
    PrevPositions: array of array[0 .. CPrevPositions - 1] of Integer;
  end;

begin
  // find start angles

  startData[0].Angle := NormalizeAngle(NormalizedAngleDiff(FCropData.EndAngle, FCropData.StartAngleMirror) * 0.5 + FCropData.EndAngle);
  startData[1].Angle := NormalizeAngle(NormalizedAngleDiff(FCropData.EndAngleMirror, FCropData.StartAngle) * 0.5 + FCropData.EndAngleMirror);

  // find groove radiuses of start angles

  for iStart := Low(startData) to High(startData) do
    startData[iStart].Radiuses := FindGrooveRadiuses(startData[iStart].Angle);

  // init

  for iCur := Low(curData) to High(curData) do
  begin
    iStart := iCur shr 1;
    dir := IfThen(Odd(iCur), -1, 1);

    curData[iCur].AngleInc := FRadiansPerRevolutionPoint * dir;
    curData[iCur].Angle := startData[iStart].Angle + curData[iCur].AngleInc * 0.5;
    curData[iCur].PrevPositionsPos := 0;

    SetLength(curData[iCur].Done, Length(startData[iStart].Radiuses));
    SetLength(curData[iCur].Radius, Length(startData[iStart].Radiuses));
    SetLength(curData[iCur].PrevPositions, Length(startData[iStart].Radiuses));

    for iRadius := 0 to High(curData[iCur].Radius) do
    begin
      curData[iCur].Radius[iRadius] := startData[iStart].Radiuses[iRadius];

      for iPP := 0 to CPrevPositions - 1 do
        curData[iCur].PrevPositions[iRadius, iPP] := -1;
    end;
  end;

  fbRatio := CutoffToFeedbackRatio(CLoopbackLowCutoffFreq, FPointsPerRevolution) * FProfileRef.RecordingGrooveWidth * 0.5 * FDPI;
  pxCount := 0;

  // loop

  repeat
    allDone := True;

    for iCur := Low(curData) to High(curData) do
    begin
      iStart := iCur shr 1;
      dir := IfThen(Odd(iCur), -1, 1);

      SinCos(curData[iCur].Angle, sn, cs);

      for iRadius := 0 to High(curData[iCur].Radius) do
      begin
        if curData[iCur].Done[iRadius] then
          Continue;

        r := curData[iCur].Radius[iRadius];

        px := round((r * cs + FCenter.X) * FSkew.X);
        py := round((r * sn + FCenter.Y) * FSkew.Y);

        yx  := py * FWidth + px;

        isPrevPosition := False;
        for iPP := 0 to CPrevPositions - 1 do
          isPrevPosition := isPrevPosition or (curData[iCur].PrevPositions[iRadius, iPP] = yx);

        if InRangePointD(py, px) and (isPrevPosition or (FProcessedImage[yx] <> CPixelValue)) then
        begin
          FProcessedImage[yx] := CPixelValue;

          prevRadius := FProfileRef.OuterSize * 0.5 * FDPI;
          if iRadius < High(curData[iCur].Radius) then
            prevRadius := curData[iCur].Radius[iRadius + 1];

          rInc := GetMono(AdjustSample(DecodeSample(CDecoderPrecision, r, prevRadius, sn, cs, meanSd), meanSd));

          curData[iCur].Radius[iRadius] += rInc * fbRatio;

          Inc(pxCount);

          allDone := False;
        end
        else
        begin
          curData[iCur].Done[iRadius] := True;
        end;

        curData[iCur].PrevPositions[iRadius, curData[iCur].PrevPositionsPos] := yx;
      end;

      Inc(curData[iCur].PrevPositionsPos);
      if curData[iCur].PrevPositionsPos >= CPrevPositions then
        curData[iCur].PrevPositionsPos := 0;

      curData[iCur].Angle += curData[iCur].AngleInc;
    end;

  until allDone;

  WriteLn(ImageShortName, RadToDeg(startData[0].Angle):9:3, RadToDeg(startData[1].Angle):9:3, Length(startData[0].Radiuses):4, Length(startData[1].Radiuses):4, pxCount:8);
end;

procedure TInputScan.Linearize;
var
  iAngle, iy, ix, yx, rBeg, rEnd: Integer;
  iCurve: Word;
  mx, ind: Cardinal;
  t, r, sqy: Double;
  ext: TRect;
  Freqs: array[0 .. High(Word)] of Cardinal;
  AngleIndicator: TCardinalDynArray;
  AngleExtents: array of TRect;
begin
  // identify angles

  SetLength(AngleIndicator, Length(FImage));
  SetLength(AngleExtents, FPointsPerRevolution);

  for iAngle := 0 to FPointsPerRevolution - 1 do
  begin
    AngleExtents[iAngle].Left := High(Integer);
    AngleExtents[iAngle].Top := High(Integer);
    AngleExtents[iAngle].Right := Low(Integer);
    AngleExtents[iAngle].Bottom := Low(Integer);
  end;

  rBeg := Floor(FProfileRef.MinConcentricGroove * 0.5 * FDPI);
  rEnd := Ceil(FProfileRef.OuterSize * 0.5 * FDPI);

  yx := 0;
  for iy := 0 to FHeight - 1 do
  begin
    sqy := Sqr(iy - FCenter.Y);

    for ix := 0 to FWidth - 1 do
    begin
      r := Sqrt(sqy + Sqr(ix - FCenter.X));

      if InRange(r, rBeg, rEnd) then
      begin
        t := ArcTan2(iy - FCenter.Y, ix - FCenter.X);
        iAngle := Round(NormalizedAngleTo02Pi(t) * (FPointsPerRevolution - 1) / (2.0 * Pi));
        ind := iAngle + 1;
        AngleIndicator[yx] := ind;
        AngleExtents[iAngle].Left := Min(AngleExtents[iAngle].Left, ix);
        AngleExtents[iAngle].Top := Min(AngleExtents[iAngle].Top, iy);
        AngleExtents[iAngle].Right := Max(AngleExtents[iAngle].Right, ix);
        AngleExtents[iAngle].Bottom := Max(AngleExtents[iAngle].Bottom, iy);
      end
      else
      begin
        FProcessedImage[yx] := FImage[yx];
      end;

      Inc(yx);
    end;
  end;

  // compute per angle curves

  for iAngle := 0 to FPointsPerRevolution - 1 do
  begin
    ind := iAngle + 1;
    ext := AngleExtents[iAngle];
    FillChar(Freqs, SizeOf(Freqs), 0);

    // devise pixel values frequencies

    for iy := ext.Top to ext.Bottom do
    begin
      yx := iy * FWidth + ext.Left;

      for ix := ext.Left to ext.Right do
      begin
        if AngleIndicator[yx] = ind then
          Inc(Freqs[FImage[yx]]);

        Inc(yx);
      end;
    end;

    // cumulate frequencies

    for iCurve := 1 to High(Word) do
      Freqs[iCurve] += Freqs[iCurve - 1];

    // normalize

    mx := Freqs[High(Word)];
    if mx > 0 then
      for iCurve := 0 to High(Word) do
        Freqs[iCurve] := Freqs[iCurve] * High(Word) div mx;

    // apply to image

    for iy := ext.Top to ext.Bottom do
    begin
      yx := iy * FWidth + ext.Left;

      for ix := ext.Left to ext.Right do
      begin
        if AngleIndicator[yx] = ind then
          FProcessedImage[yx] := Freqs[FImage[yx]];

        Inc(yx);
      end;
    end;
  end;
end;

function TInputScan.InRangePointD(Y, X: Double): Boolean;
begin
  Result := InRange(Y, -Low(TSerpCoeffs9), Height + Low(TSerpCoeffs9) - 2) and InRange(X, -Low(TSerpCoeffs9), Height + Low(TSerpCoeffs9) - 2);
end;

function TInputScan.GetPointD_Work(const Image: TWordDynArray; Y, X: Double): Double;
var
  ix, iy: Integer;
begin
  ix := trunc(X);
  iy := trunc(Y);

  Result := lerpXY(@Image[iy * FWidth + ix], FWidth, X - ix, Y - iy);
end;

function TInputScan.GetPointD_Final(const Image: TWordDynArray; Y, X: Double): Double;
var
  ix, iy: Integer;
begin
  ix := trunc(X);
  iy := trunc(Y);

  Result := herpXY(@Image[iy * FWidth + ix], FWidth, X - ix, Y - iy);
end;

function TInputScan.GetMeanSD(const Image: TWordDynArray; AStartRadius, AEndRadius, AStartAngle, AEndAngle,
  ASigma: Double): TPointD;
var
  iRadius, iLut, cnt: Integer;
  diff, px, py: Double;
  sinCosLUT: TSinCosDynArray;
begin
  diff := NormalizedAngleDiff(AStartAngle, AEndAngle);
  if diff = 0 then
    diff := 2.0 * Pi;

  BuildSinCosLUT(Ceil(AEndRadius * diff), sinCosLUT, AStartAngle, diff);

  cnt := 0;
  Result.X := 0.0;
  for iRadius := Floor(AStartRadius) to Ceil(AEndRadius) do
    for iLut := 0 to High(sinCosLUT) do
    begin
      px := (sinCosLUT[iLut].Cos * iRadius + FCenter.X) * FSkew.X;
      py := (sinCosLUT[iLut].Sin * iRadius + FCenter.Y) * FSkew.Y;
      if InRangePointD(py, px) then
      begin
        Result.X += GetPointD_Work(Image, py, px);
        Inc(cnt);
      end;
    end;
  Result.X /= cnt;

  cnt := 0;
  Result.Y := 0.0;
  for iRadius := Floor(AStartRadius) to Ceil(AEndRadius) do
    for iLut := 0 to High(sinCosLUT) do
    begin
      px := (sinCosLUT[iLut].Cos * iRadius + FCenter.X) * FSkew.X;
      py := (sinCosLUT[iLut].Sin * iRadius + FCenter.Y) * FSkew.Y;
      if InRangePointD(py, px) then
      begin
        Result.Y += Sqr(GetPointD_Work(Image, py, px) - Result.X);
        Inc(cnt);
      end;
    end;
  Result.Y /= cnt;
  Result.Y := Sqrt(Result.Y) * ASigma;
  Result.Y := DivDef(1.0, Result.Y, 1.0);

  //WriteLn(Result.X:16:6, Result.Y:16:6);
end;

procedure TInputScan.GetGradientsD(const Image: TWordDynArray; Y, X: Double; out GY: Double; out GX: Double);
const
  CH = 0.25;
var
  iFD: Integer;
  lgx, lgy, fdx, fdy: Double;
begin
  lgx := 0.0;
  lgy := 0.0;

  for iFD := Low(CFiniteDifferencesYFactor) to High(CFiniteDifferencesYFactor) do
  begin
    if iFD = 0 then
      Continue;

    fdx := iFD * CH;
    fdy := CFiniteDifferencesYFactor[iFD] / CH;

    lgx += GetPointD_Work(Image, y, x + fdx) * fdy;
    lgy += GetPointD_Work(Image, y + fdx, x) * fdy;
  end;

  GX := lgx;
  GY := lgy;
end;

function TInputScan.DecodeSample(precision: Integer; radius, prevRadius, angleSin, angleCos: Double; out
  sampleMeanSD: TPointD): TPointD;
var
  iSmp, posMin, posMax, decoderMax: Integer;
  r, cx, cy, skx, sky, px, py, cxa, sampleMean, sampleStdDev: Double;
  smpBuf: array[SmallInt] of Double;

  function GetSampleIdx(iSmp: Integer): Double;
  var
    r: Double;
  begin
    r := radius + (iSmp + 0.5) * cxa;
    Result := GetPointD_Final(Image, (angleSin * r + cy) * sky, (angleCos * r + cx) * skx);
    smpBuf[iSmp] := Result;
  end;

begin
  Result.X := 0.0;
  Result.Y := 0.0;

  decoderMax := 1 shl precision;

  cxa := FProfileRef.RecordingGrooveWidth * 0.5 * FDPI / decoderMax;

  cx := FCenter.X;
  cy := FCenter.Y;
  skx := FSkew.X;
  sky := FSkew.Y;

  r := radius + decoderMax * cxa;
  px := (angleCos * r + cx) * skx;
  py := (angleSin * r + cy) * sky;

  if not InRangePointD(py, px) then
    Exit;

  if not IsNan(prevRadius) then
    decoderMax := EnsureRange(Floor((prevRadius - radius) * 0.5 * CTrack2TrackToTrackWidthRatio / cxa), 2, decoderMax);

  posMin := -decoderMax;
  posMax := decoderMax - 1;
  sampleMean := Infinity;
  sampleStdDev := -Infinity;

  Result.X := 0.0;
  for iSmp := 0 to posMax do
    Result.X += GetSampleIdx(iSmp);

  Result.Y := 0.0;
  for iSmp := posMin to -1 do
    Result.Y += GetSampleIdx(iSmp);

  MeanAndStdDev(@smpBuf[posMin], posMax - posMin + 1, sampleMean, sampleStdDev);

  sampleMeanSD.X := sampleMean;
  sampleMeanSD.Y := DivDef(1.0, sampleStdDev, 0.0);

  Result.X /= decoderMax;
  Result.Y /= decoderMax;
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
  FImage[y * Width + x] := ToBW(Value.Red, Value.Green, Value.Blue);
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

