unit scancorrelator;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Types, Math, Graphics, GraphType, IntfGraphics, FPCanvas, FPImage, FPWritePNG, StrUtils, ZStream,
  utils, bufstream, fgl, powell, minasa, minlbfgs, inputscan;

type

  { TScanCorrelator }

  TScanCorrelator = class
  private
    FInputScans: array of TInputScan;
    FOutputPNGFileName: String;
    FDPI: Integer;

    FPointsPerRevolution: Integer;
    FRadiansPerRevolutionPoint: Double;
    FMaxOutputImageValue: Double;

    FOutputImage: TSingleDynArray2;
  public
    constructor Create(const AFileNames: TStrings; ADPI: Integer = 2400);
    destructor Destroy; override;

    procedure LoadPNGs;
    procedure Correlate;
    procedure Save;
    procedure Run;

    property OutputPNGFileName: String read FOutputPNGFileName write FOutputPNGFileName;

    property PointsPerRevolution: Integer read FPointsPerRevolution;
    property RadiansPerRevolutionPoint: Double read FRadiansPerRevolutionPoint;

    property OutputImage: TSingleDynArray2 read FOutputImage;
  end;

  { TScanImage }

  TScanImage = class(TFPCustomImage)
  private
    FScanCorrelator: TScanCorrelator;
    FFactor: Single;
  protected
    procedure SetInternalPixel (x,y:integer; Value:integer); override;
    function GetInternalPixel (x,y:integer) : integer; override;
  public
    constructor Create (AWidth,AHeight:integer); override;

    property ScanCorrelator: TScanCorrelator read FScanCorrelator write FScanCorrelator;
  end;

implementation

{ TScanCorrelator }

constructor TScanCorrelator.Create(const AFileNames: TStrings; ADPI: Integer);
var
  i: Integer;
begin
  FDPI := ADPI;
  FPointsPerRevolution := Ceil(Pi * C45RpmOuterSize * ADPI);
  FRadiansPerRevolutionPoint := Pi * 2.0 / FPointsPerRevolution;

  SetLength(FInputScans, AFileNames.Count);
  for i := 0 to AFileNames.Count - 1 do
  begin
    FInputScans[i] := TInputScan.Create(FPointsPerRevolution, ADPI);
    FInputScans[i].PNGFileName := AFileNames[i];
  end;

  WriteLn('PointsPerRevolution:', FPointsPerRevolution:12);

  SetLength(FOutputImage, Ceil(C45RpmOuterSize * ADPI), Ceil(C45RpmOuterSize * ADPI));
end;

destructor TScanCorrelator.Destroy;
var
  i: Integer;
begin
  for i := 0 to High(FInputScans) do
    FInputScans[i].Free;

  inherited Destroy;
end;

procedure TScanCorrelator.LoadPNGs;
var
  i: Integer;
begin
  WriteLn('LoadPNGs');
  for i := 0 to High(FInputScans) do
    FInputScans[i].Run;
end;

procedure TScanCorrelator.Correlate;
var
  ox, oy, i: Integer;
  t, r, sn, cs, x, y, center, acc: Double;
begin
  WriteLn('Correlate');

  center := Length(FOutputImage) / 2.0;
  FMaxOutputImageValue := 0;

  for oy := 0 to High(FOutputImage) do
    for ox := 0 to High(FOutputImage[0]) do
    begin
      r := Sqrt(Sqr(center - ox) + Sqr(center - oy));
      t := ArcTan2(center - oy, center - ox);

      if InRange(r, C45RpmInnerSize * FDPI * 0.5, C45RpmOuterSize * FDPI * 0.5) then
      begin
        acc := 0;
        for i := 0 to High(FInputScans) do
        begin
          SinCos(FInputScans[i].GrooveStartAngle - t, sn, cs);

          x := cs * r + FInputScans[i].Center.X;
          y := sn * r + FInputScans[i].Center.Y;

          if InRange(y, 0, FInputScans[i].Height - 1) and InRange(x, 0, FInputScans[i].Width - 1) then
            acc += FInputScans[i].GetPointD(FInputScans[i].Image, y, x);
        end;
        if Length(FInputScans) > 0 then
          acc /= Length(FInputScans);

        FOutputImage[oy, ox] := acc;
        FMaxOutputImageValue := Max(FMaxOutputImageValue, acc);
      end
      else
      begin
        FOutputImage[oy, ox] := 1.0;
      end;
    end;
end;

procedure TScanCorrelator.Save;
var
  i, ox, oy: Integer;
  png: TFPWriterPNG;
  factor: Single;
  fs: TFileStream;
  fpimg: TScanImage;
begin
  WriteLn('Save ', FOutputPNGFileName);

  factor := High(Word);
  if not IsZero(FMaxOutputImageValue) then
    factor /= FMaxOutputImageValue;

  fs := TFileStream.Create(FOutputPNGFileName, fmCreate or fmShareDenyNone);
  fpimg := TScanImage.Create(Length(FOutputImage[0]), Length(FOutputImage));
  png := TFPWriterPNG.Create;
  try
    fpimg.ScanCorrelator := Self;
    fpimg.UsePalette := True;
    for i := 0 to High(Word) do
      fpimg.Palette.Add(FPColor(i, i, i, High(Word)));

    png.CompressedText := True;
    png.CompressionLevel := clmax;
    png.GrayScale := True;
    png.WordSized := True;
    png.Indexed := False;
    png.UseAlpha := False;

    png.ImageWrite(fs, fpimg);
  finally
    png.Free;
    fpimg.Free;
    fs.Free;
  end;
end;

procedure TScanCorrelator.Run;
begin
  LoadPNGs;
  Correlate;
  Save;
end;

{ TScanImage }

procedure TScanImage.SetInternalPixel(x, y: integer; Value: integer);
begin
  // nothing (read only)
end;

function TScanImage.GetInternalPixel(x, y: integer): integer;
begin
  if IsNan(FFactor) then
  begin
    FFactor := High(Word);
    if not IsZero(FScanCorrelator.FMaxOutputImageValue) then
      FFactor /= FScanCorrelator.FMaxOutputImageValue;
  end;

  Result := EnsureRange(Round(FScanCorrelator.FOutputImage[y, x] * FFactor), 0, High(Word));
end;

constructor TScanImage.Create(AWidth, AHeight: integer);
begin
  inherited Create(AWidth, AHeight);

  FFactor := NaN;
end;

end.

