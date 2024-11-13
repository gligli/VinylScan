unit scan2track;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Types, StrUtils, Math, Graphics, GraphType, FPCanvas, FPImage, FPWritePNG, utils, powell;

const
  C45RpmOuterSize = 7.0;
  C45RpmInnerSize = 1.5;

type

  { TScan2Track }

  TScan2Track = class
  private
    FPNGFileName: String;
    FDPI: Integer;
    FBitsPerSample: Integer;
    FSampleRate: Integer;

    FCenter: TPoint;

    FImage: TByteDynArray2;
    FTrack: TIntegerArray;

    procedure FindCenter;

  public
    constructor Create(ASampleRate: Integer = 48000; ABitsPerSample: Integer = 16);
    destructor Destroy; override;

    procedure LoadPNG;
    procedure FindTrack;
    procedure ScanTrack;

    procedure Run;

    property SampleRate: Integer read FSampleRate;
    property BitsPerSample: Integer read FBitsPerSample;

    property PNGFileName: String read FPNGFileName write FPNGFileName;

    property Track: TIntegerArray read FTrack;
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

  WriteLn(x[0]:8:0,x[1]:8:0,Result:20:0);

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

  PowellMinimize(@PowellEvalCenter, x, FDPI / 10, 0.5, 0.5, MaxInt, Self);

  FCenter.X := round(x[0]);
  FCenter.Y := round(x[1]);
end;

constructor TScan2Track.Create(ASampleRate: Integer; ABitsPerSample: Integer);
begin
  FSampleRate := ASampleRate;
  FBitsPerSample := ABitsPerSample;

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

    WriteLn(PNG.Width:6,'x',PNG.Height:6);
    Assert(PNG.PixelFormat=pf8bit);

    for y := 0 to High(FImage) do
    begin
      p := PNG.ScanLine[y];
      for x := 0 to High(FImage[0]) do
      begin
        FImage[y, x] := p^;
        Inc(p);
      end;
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

  WriteLn('Center:',FCenter.X:6,',',FCenter.Y:6);

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

