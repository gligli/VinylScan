unit scan2track;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Types, StrUtils, Math, Graphics, GraphType, FPCanvas, FPImage, FPWritePNG, utils, powell;

const
  C45RpmRevolutionsPerSecond = 45.0 / 60.0;
  C45RpmOuterSize = 6.0 + 7.0 / 8.0;
  C45RpmInnerSize = 1.504;
  C45RpmConcentricGroove = 3.0 + 7.0 / 8.0;
  C45RpmFirstGroove = 6.0 + 6.0 / 8.0;

type

  { TScan2Track }

  TScan2Track = class
  private
    FPNGFileName: String;
    FDPI: Integer;
    FBitsPerSample: Integer;
    FSampleRate: Integer;
    FPointsPerRevolution: Integer;
    FRadiansPerRevolutionPoint: Double;

    FCenter: TPoint;
    FConcentricGrooveOffset: Double; // offset to the center point
    FFirstGrooveOffset: Double; // offset to the center point
    FGrooveStartRadians: Double;
    FGrooveStartPoint: TPoint;

    FImage: TByteDynArray2;
    FTrack: TIntegerDynArray;

    procedure FindCenter;
    procedure FindConcentricGroove;
    procedure FindGrooveStart;

  public
    constructor Create(ASampleRate: Integer = 48000; ABitsPerSample: Integer = 16);
    destructor Destroy; override;

    procedure LoadPNG;
    procedure FindTrack;
    procedure ScanTrack;

    procedure Run;

    property PNGFileName: String read FPNGFileName write FPNGFileName;
    property SampleRate: Integer read FSampleRate;
    property BitsPerSample: Integer read FBitsPerSample;

    property Center: TPoint read FCenter;
    property ConcentricGrooveOffset: Double read FConcentricGrooveOffset;
    property FirstGrooveOffset: Double read FFirstGrooveOffset;
    property GrooveStartPoint: TPoint read FGrooveStartPoint;

    property Image: TByteDynArray2 read FImage;
    property Track: TIntegerDynArray read FTrack;
  end;

implementation

{ TScan2Track }

function PowellEvalCenter(const x: TVector; Data: Pointer): Double;
var
  Self: TScan2Track absolute Data;
  Rect: TRect;
  sz, xx, yy: Integer;
begin
  sz := Round(C45RpmInnerSize * Self.FDPI / sqrt(2));
  Rect.Left := Round(x[0]) - sz div 2;
  Rect.Top := Round(x[1]) - sz div 2;
  Rect.Right := Rect.Left + sz;
  Rect.Bottom := Rect.Top + sz;

  Result := 0;
  for yy := Rect.Top to Rect.Bottom do
    if InRange(yy, 0, High(Self.FImage)) then
      for xx := Rect.Left to Rect.Right do
        if InRange(xx, 0, High(Self.FImage[0])) then
          Result += Self.FImage[yy, xx];

  //WriteLn(x[0]:8:0,x[1]:8:0,Result:20:0);

  Result := -Result;
end;

procedure TScan2Track.FindCenter;
var
  x: TVector;
begin
  FCenter.X := Length(FImage[0]) div 2;
  FCenter.Y := Length(FImage) div 2;

  SetLength(x, 2);
  x[0] := FCenter.X;
  x[1] := FCenter.Y;

  PowellMinimize(@PowellEvalCenter, x, FDPI / 10.0, 0.5, 0.5, MaxInt, Self);

  FCenter.X := round(x[0]);
  FCenter.Y := round(x[1]);
end;

function PowellEvalConcentricGroove(const x: TVector; Data: Pointer): Double;
var
  Self: TScan2Track absolute Data;
  i, xx, yy: Integer;
  sn, cs: Double;
begin

  Result := 0;
  for i := 0 to Self.FPointsPerRevolution - 1  do
  begin
    SinCos(i * Self.FRadiansPerRevolutionPoint, sn, cs);

    xx := Round(cs * x[0]) + Self.Center.X;
    yy := Round(sn * x[0]) + Self.Center.Y;

    if InRange(yy, 0, High(Self.FImage)) and InRange(xx, 0, High(Self.FImage[0])) then
      Result += Self.FImage[yy, xx];
  end;

  //WriteLn(x[0]:12:3,Result:20:0);

  Result := -Result;
end;

procedure TScan2Track.FindConcentricGroove;
var
  x: TVector;
begin
  FConcentricGrooveOffset := C45RpmConcentricGroove * Self.FDPI * 0.5;

  SetLength(x, 1);
  x[0] := FConcentricGrooveOffset;

  PowellMinimize(@PowellEvalConcentricGroove, x, FDPI / 20.0, 0.0, 0.5, MaxInt, Self);

  FConcentricGrooveOffset := x[0];
end;

procedure TScan2Track.FindGrooveStart;
var
  i, x, y, bestx, besty: Integer;
  v, best, sn, cs, bestr: Double;
begin
  best := -Infinity;
  bestx := 0;
  besty := 0;
  bestr := 0;
  v := 0;

  for i := 0 to Self.FPointsPerRevolution - 1  do
  begin
    SinCos(i * Self.FRadiansPerRevolutionPoint, sn, cs);

    x := Round(cs * FFirstGrooveOffset) + Self.Center.X;
    y := Round(sn * FFirstGrooveOffset) + Self.Center.Y;

    if InRange(y, 0, High(Self.FImage)) and InRange(x, 0, High(Self.FImage[0])) then
    begin
      v := v * 99 + Self.FImage[y, x];
      v /= 100;

      if v > best then
      begin
        best := v;
        bestx := x;
        besty := y;
        bestr := i * Self.FRadiansPerRevolutionPoint;
      end;
    end;

    writeln(i:6,x:8,y:8,v:9:3,best:9:3,bestx:8,besty:8);
  end;

  FGrooveStartRadians := bestr;
  FGrooveStartPoint.X := bestx;
  FGrooveStartPoint.Y := besty;
end;

constructor TScan2Track.Create(ASampleRate: Integer; ABitsPerSample: Integer);
begin
  FSampleRate := ASampleRate;
  FBitsPerSample := ABitsPerSample;

  FPointsPerRevolution := Round(FSampleRate * C45RpmRevolutionsPerSecond);
  FRadiansPerRevolutionPoint := Pi * 2.0 / FPointsPerRevolution;

  FDPI := 2400;
end;

destructor TScan2Track.Destroy;
begin
  inherited Destroy;
end;

procedure TScan2Track.LoadPNG;
var
  PNG: TPortableNetworkGraphic;
  x, y: Integer;
  p: PByte;
begin
  WriteLn('LoadPNG');

  PNG := TPortableNetworkGraphic.Create;
  try
    PNG.LoadFromFile(FPNGFileName);

    SetLength(FImage, PNG.Height, PNG.Width);

    WriteLn(PNG.Width:6, 'x', PNG.Height:6);
    Assert(PNG.PixelFormat=pf8bit);

    PNG.BeginUpdate;
    try
      for y := 0 to High(FImage) do
      begin
        p := PNG.RawImage.Data;
        inc(p, y * PNG.RawImage.Description.BytesPerLine);
        for x := 0 to High(FImage[0]) do
        begin
          FImage[y, x] := p^;
          Inc(p, PNG.RawImage.Description.BitsPerPixel shr 3);
        end;
      end;
    finally
      PNG.EndUpdate;
    end;

    // TODO: guess DPI

  finally
    PNG.Free;
  end;
end;

procedure TScan2Track.FindTrack;
begin
  WriteLn('FindTrack');

  FindCenter;

  WriteLn('Center:', FCenter.X:6, ',', FCenter.Y:6);

  FFirstGrooveOffset := C45RpmFirstGroove * Self.FDPI * 0.5;

  WriteLn('FirstGrooveOffset:', FFirstGrooveOffset:12:3);

  FindConcentricGroove;

  WriteLn('ConcentricGrooveOffset:', FConcentricGrooveOffset:12:3);

  FindGrooveStart;

  WriteLn('GrooveStartRadians:', FGrooveStartRadians:12:3);
  WriteLn('GrooveStartPoint:', FGrooveStartPoint.X:6, ',', FGrooveStartPoint.Y:6);
end;

procedure TScan2Track.ScanTrack;
begin
  WriteLn('ScanTrack');


end;

procedure TScan2Track.Run;
begin
  LoadPNG;
  FindTrack;
  ScanTrack;
end;

end.

