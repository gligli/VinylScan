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

  TCorrectRef = packed record
    AngleIdx, ScanIdx: Integer;
  end;

  TCropAngles = record
    StartAngle, EndAngle: Double;
    StartAngleMirror, EndAngleMirror: Double;
  end;

  TCropData = record
    RadiusAngleLut: TRadiusAngleDynArray;
    SinCosLut: TSinCosDDynArray;
  end;

  TInputScanDynArray = array of TInputScan;

  { TInputScan }

  TInputScan = class
  private
    FProfileRef: TProfile;
    FImageFileName: String;
    FGamma: Double;
    FDPI: Integer;
    FPointsPerRevolution: Integer;
    FRadiansPerRevolutionPoint: Double;
    FSilent: Boolean;
    FSinCosLUT: TSinCosDDynArray;

    FCenterExtents: TRect;
    FCenter: TPointD;
    FSkew: TPointD;
    FConcentricGrooveRadius: Double;
    FSetDownRadius: Double;
    FGrooveStartAngle: Double;
    FGrooveStartAngleQuality: Double;
    FGrooveStartPoint: TPointD;

    FRelativeAngle: Double;
    FCropAngles: TCropAngles;
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
    function MakeCropAngles(const arg: TDoubleDynArray): TCropAngles;
    function MakeRadiusOffsets(ARadius: Integer): TIntegerDynArray;

    procedure GradientConcentricGroove(const arg: TDoubleDynArray; var func: Double; grad: TDoubleDynArray; obj: Pointer);
    function NelderMeadCrop(const arg: TVector; obj: Pointer): TScalar;

    procedure FindCenterExtents;
    procedure FindConcentricGroove_GridSearch;
    procedure FindConcentricGroove_Gradient;
    procedure FindCroppedArea;
    procedure FindGrooveStart;

    procedure LoadPNG;
    procedure LoadTIFF;
  public
    constructor Create(AProfileRef: TProfile; AGamma: Double; ADefaultDPI: Integer = 2400; ASilent: Boolean = False);
    destructor Destroy; override;

    procedure InitImage(AWidth, AHeight, ADPI: Integer);
    procedure LoadImage;
    procedure FixCISScanners;
    procedure Blur;
    procedure FindTrack(AUseGradient, AFindCroppedArea: Boolean; AForcedSampleRate: Integer = -1);
    procedure CorrectByModel(ARelativeAngle, ACenterX, ACenterY, ASkewX, ASkewY: Double);
    procedure Crop(const RadiusAngleLut: TRadiusAngleDynArray; const SinCosLut: TSinCosDDynArray);
    procedure Linearize;

    function InRangePointD(Y, X: Double): Boolean; inline;
    function GetPointD_Work(const Image: TWordDynArray; Y, X: Double): Double; inline;
    function GetPointD_Final(const Image: TWordDynArray; Y, X: Double): Double; inline;
    function GetMeanSD(const Image: TWordDynArray; AStartRadius, AEndRadius, AStartAngle, AEndAngle, ASigma: Double): TPointD;
    procedure GetGradientsD(const Image: TWordDynArray; Y, X: Double; out GY: Double; out GX: Double);

    procedure AddCorrectRef(AngleIdx, ScanIdx: Integer);
    function HasCorrectRef(const AList: TInputScanDynArray; AngleIdx, ScanIdx: Integer): Boolean;

    property ProfileRef: TProfile read FProfileRef;
    property ImageFileName: String read FImageFileName write FImageFileName;
    property ImageShortName: String read GetImageShortName;

    property Gamma: Double read FGamma;
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
    property CropAngles: TCropAngles read FCropAngles;

    property CenterQuality: Double read FCenterQuality;
    property Objective: Double read FObjective write FObjective;

    property Image: TWordDynArray read FImage;
    property ProcessedImage: TWordDynArray read FProcessedImage;
  end;

  { TScanImage }

  TScanImage = class(TFPCustomImage)
  private
    FGamma: Double;
    FGammaLUT: array[Word] of Word;
    FImage: TWordDynArray;
    procedure SetGamma(AValue: Double);
  protected
    procedure SetInternalPixel(x,y:integer; Value:integer); override;
    function GetInternalPixel(x,y:integer): integer; override;
    procedure SetInternalColor(x,y:integer; const Value:TFPColor); override;
  public
    constructor Create(AWidth,AHeight:integer); override;

    property Image: TWordDynArray read FImage write FImage;
    property Gamma: Double read FGamma write SetGamma;
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

  Result.X[NegativeValue] := -FProfileRef.LeadOutGrooveThickness * FDPI;
  Result.X[ZeroValue] := 0;
  Result.X[PositiveValue] := FProfileRef.LeadOutGrooveThickness * FDPI;
end;

function TInputScan.MakeCropAngles(const arg: TDoubleDynArray): TCropAngles;
begin
  Result.StartAngle := NormalizeAngle(arg[0]);
  Result.EndAngle := NormalizeAngle(arg[1]);
  Result.StartAngleMirror := NormalizeAngle(arg[2]);
  Result.EndAngleMirror := NormalizeAngle(arg[3]);
end;

procedure TInputScan.FindConcentricGroove_GridSearch;
const
  CPointsPerRevolution = 512;
var
  extents: TRect;
  sinCosLUT: TSinCosDDynArray;
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
        imgInt := (GetPointD_Work(FProcessedImage, py, px) - meanSD^.X) * meanSD^.Y;

        func -= imgInt * sy;

        if Assigned(grad) then
        begin
          GetGradientsD(FProcessedImage, py, px, gimgy, gimgx);

          gInt := meanSD^.Y * sy;

          gcx -= gimgx * gInt;
          gcy -= gimgy * sky * gInt;
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
  ff, mnCG, mxCG: Double;
  meanSD: TPointD;
  X: TDoubleDynArray;
begin
  BuildSinCosLUT(Ceil(Pi * FProfileRef.ConcentricGroove * FDPI), FSinCosLUT);

  mnCG := FProfileRef.MinConcentricGroove * 0.5 * FDPI;
  mxCG := FProfileRef.MaxConcentricGroove * 0.5 * FDPI;
  meanSD := GetMeanSD(FProcessedImage, mnCG, mxCG, -Pi, Pi, 1.0);

  X := [FCenter.X, FCenter.Y, FConcentricGrooveRadius, 1.0];

  ff := BoxConstrainedScaledMinimize(@GradientConcentricGroove, X,
          [FCenterExtents.Left, FCenterExtents.Top, mnCG, CScannerTolLo],
          [FCenterExtents.Right, FCenterExtents.Bottom, mxCG, CScannerTolHi],
          [1.0, 1.0, 1.0, 1e-3],
          1e-9, @meanSD);

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

  hasCrop := (NormalizedAngleDiff(FCropAngles.StartAngle, FCropAngles.EndAngle) <> 0) or
      (NormalizedAngleDiff(FCropAngles.StartAngleMirror, FCropAngles.EndAngleMirror) <> 0);

  stencil := MakeStencil;

  for i := 0 to FPointsPerRevolution - 1  do
  begin
    angle := NormalizeAngle(i * FRadiansPerRevolutionPoint);

    if hasCrop and
        (InNormalizedAngle(angle, FCropAngles.StartAngle, FCropAngles.EndAngle) or
        InNormalizedAngle(angle, FCropAngles.StartAngleMirror, FCropAngles.EndAngleMirror)) then
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
  FGrooveStartPoint.X := bestx / FSkew.X;
  FGrooveStartPoint.Y := besty / FSkew.Y;
end;

constructor TInputScan.Create(AProfileRef: TProfile; AGamma: Double; ADefaultDPI: Integer; ASilent: Boolean);
begin
  FProfileRef := AProfileRef;
  FGamma := AGamma;
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
      img.Gamma := FGamma;

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
      img.Gamma := FGamma;

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

function TInputScan.NelderMeadCrop(const arg: TVector; obj: Pointer): TScalar;
const
  CMinCrop = 30.0;
  CMaxCrop = 120.0;
var
  data: ^TCropData absolute obj;
  iLut: Integer;
  cx, cy, skx, sky, r, px, py, bt: Double;
  cropped: Boolean;
  angles: TCropAngles;
  cnt: array[Boolean] of Integer;
  acc: array[Boolean] of Double;
  ra: ^TRadiusAngle;
  sc: ^TSinCosD;
begin
  Result := 1e6;

  angles := MakeCropAngles(arg);

  if not InRange(NormalizedAngleDiff(angles.StartAngle, angles.EndAngle), DegToRad(CMinCrop), DegToRad(CMaxCrop)) or
     not InRange(NormalizedAngleDiff(angles.StartAngleMirror, angles.EndAngleMirror), DegToRad(CMinCrop), DegToRad(CMaxCrop)) then
    Exit;

  cx := FCenter.X;
  cy := FCenter.Y;
  skx := FSkew.X;
  sky := FSkew.Y;

  for cropped := False to True do
  begin
    acc[cropped] := 0.0;
    cnt[cropped] := 0;
  end;

  for iLut := 0 to High(data^.RadiusAngleLut) do
  begin
    ra := @data^.RadiusAngleLut[iLut];
    sc := @data^.SinCosLut[iLut];

    bt := ra^.Angle;
    r := ra^.Radius;

    px := (sc^.Cos * r + cx) * skx;
    py := (sc^.Sin * r + cy) * sky;

    if InRangePointD(py, px) then
    begin
      cropped := InNormalizedAngle(bt, angles.StartAngle, angles.EndAngle) or
                 InNormalizedAngle(bt, angles.StartAngleMirror, angles.EndAngleMirror);
      acc[cropped] += GetPointD_Work(FImage, py, px);
      Inc(cnt[cropped]);
    end;
  end;

  Result := DivDef(acc[True], cnt[True], 0.0) - DivDef(acc[False], cnt[False], 0.0);

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

procedure TInputScan.Crop(const RadiusAngleLut: TRadiusAngleDynArray; const SinCosLut: TSinCosDDynArray);
var
  X: TVector;
  data: TCropData;
begin
  X := [NormalizeAngle(DegToRad(-30.0)), NormalizeAngle(DegToRad(30.0)), NormalizeAngle(DegToRad(-30.0) + Pi), NormalizeAngle(DegToRad(30.0) + Pi)];

  data.RadiusAngleLut := RadiusAngleLut;
  data.SinCosLut := SinCosLut;
  NelderMeadMinimize(@NelderMeadCrop, X, [DegToRad(1.0), DegToRad(1.0), DegToRad(1.0), DegToRad(1.0)], 1e-4, @data);

  FCropAngles := MakeCropAngles(X);
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
        FCropAngles.StartAngleMirror := angle
      else
        FCropAngles.StartAngle := angle;
    end
    else if not angleCropped[iAngle] and prevAngleCropped then
    begin
      angle := NormalizeAngle((iAngle + CAngleMargin) * toRad);

      if isMirror then
        FCropAngles.EndAngleMirror := angle
      else
        FCropAngles.EndAngle := angle;

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

procedure TInputScan.Linearize;
var
  ppr: Integer;

  AngleIndicator: TWordDynArray;
  AngleExtents: array of TRect;

  procedure DoAngle(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    iy, ix, yx: Integer;
    ind, iCurve: Word;
    mx, mn: Cardinal;
    ext: TRect;
    Freqs: array[0 .. High(Word)] of Cardinal;
  begin
    if not InRange(AIndex, 0, ppr - 1) then
      Exit;

    ind := AIndex + 1;
    ext := AngleExtents[AIndex];

    if (ext.Left > ext.Right) or (ext.Top > ext.Bottom) then
      Exit;

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

    mn := Freqs[0];
    mx := Freqs[High(Word)] - mn;
    if mx > 0 then
      for iCurve := 0 to High(Word) do
        Freqs[iCurve] := (Freqs[iCurve] - mn) * High(Word) div mx;

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

var
  iAngle, iy, ix, yx, rBeg, rEnd: Integer;
  ind: Word;
  t, r, sqy: Double;
begin
  FProcessedImage := nil;
  SetLength(FProcessedImage, Height * Width);

  rBeg := Floor(FProfileRef.MinConcentricGroove * 0.5 * FDPI);
  rEnd := Ceil(FProfileRef.OuterSize * 0.5 * FDPI);

  ppr := Ceil(rBeg * 2.0 * Pi * 0.25);

  Assert(ppr < High(Word));

  // identify angles

  SetLength(AngleIndicator, Length(FImage));
  SetLength(AngleExtents, ppr);

  for iAngle := 0 to ppr - 1 do
  begin
    AngleExtents[iAngle].Left := High(Integer);
    AngleExtents[iAngle].Top := High(Integer);
    AngleExtents[iAngle].Right := Low(Integer);
    AngleExtents[iAngle].Bottom := Low(Integer);
  end;

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
        iAngle := Trunc(NormalizedAngleTo02Pi(t) * ppr / (2.0 * Pi));
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

  ProcThreadPool.DoParallelLocalProc(@DoAngle, 0, ppr - 1);
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
  sinCosLUT: TSinCosDDynArray;
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

procedure TScanImage.SetGamma(AValue: Double);
var
  iLut: Integer;
begin
  if FGamma = AValue then Exit;
  FGamma := AValue;
  for iLut := Low(FGammaLUT) to High(FGammaLUT) do
    FGammaLUT[iLut] := Round(Power(iLut * (1.0 / High(FGammaLUT)), FGamma) * High(FGammaLUT));
end;

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
  FImage[y * Width + x] := ToBW(FGammaLUT[Value.Red], FGammaLUT[Value.Green], FGammaLUT[Value.Blue]);
end;

constructor TScanImage.Create(AWidth, AHeight: integer);
begin
  inherited Create(AWidth, AHeight);
  SetGamma(1.0);
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

