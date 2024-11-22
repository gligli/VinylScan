unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, scan2track, scancorrelator, utils, math;

const
  CReducShift = 2;
  CReducFactor = 1.0 / (1 shl CReducShift);

type

  { TMainForm }

  TMainForm = class(TForm)
    btScan2Track: TButton;
    btScansCorrelator: TButton;
    btTest: TButton;
    edInputPNG: TEdit;
    edOutputPNG: TEdit;
    edOutputWAV: TEdit;
    Image: TImage;
    mmInputPNGs: TMemo;
    pnSettings: TPanel;
    procedure btScan2TrackClick(Sender: TObject);
    procedure btScansCorrelatorClick(Sender: TObject);
    procedure btTestClick(Sender: TObject);
  private

    procedure DrawImage(const Img: TWordDynArray2); overload;
    procedure DrawImage(const Img: TSingleDynArray2); overload;
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.btScan2TrackClick(Sender: TObject);
var
  s2t: TScan2Track;
  C: TCanvas;
  cx, cy, sx, sy, offf, offc: Integer;
begin
  C := Image.Picture.Bitmap.Canvas;

  C.Brush.Style := bsClear;
  C.Pen.Color := clRed;
  C.Pen.Color := clRed;
  C.Pen.Style := psSolid;

  s2t := TScan2Track.Create;
  try
    s2t.OutputWAVFileName := edOutputWAV.Text;
    s2t.Scan.PNGFileName := edInputPNG.Text;

    s2t.Scan.LoadPNG;

    DrawImage(s2t.Scan.Image);

    s2t.Scan.FindTrack;

    cx := Round(s2t.Scan.Center.X * CReducFactor);
    cy := Round(s2t.Scan.Center.Y * CReducFactor);
    sx := Round(s2t.Scan.GrooveStartPoint.X * CReducFactor);
    sy := Round(s2t.Scan.GrooveStartPoint.Y * CReducFactor);
    offf := Round(s2t.Scan.FirstGrooveRadius) shr CReducShift;
    offc := Round(s2t.Scan.ConcentricGrooveRadius) shr CReducShift;

    C.Line(cx - 8, cy, cx + 9, cy);
    C.Line(cx, cy - 8, cx, cy + 9);

    C.Line(sx - 8, sy, sx + 9, sy);
    C.Line(sx, sy - 8, sx, sy + 9);

    C.EllipseC(cx, cy, offf, offf);
    C.EllipseC(cx, cy, offc, offc);

    HorzScrollBar.Position := sx - Width div 2;
    VertScrollBar.Position := sy - Height div 2;
    Application.ProcessMessages;

    s2t.EvalTrack;

  finally
    s2t.Free;
  end;
end;

procedure TMainForm.btScansCorrelatorClick(Sender: TObject);
var
  sc: TScanCorrelator;
begin
  sc := TScanCorrelator.Create(mmInputPNGs.Lines);
  try
    sc.OutputPNGFileName := edOutputPNG.Text;

    sc.LoadPNGs;

    sc.Analyze;
    sc.Crop;
    sc.Correct;
    sc.Rebuild;

    DrawImage(sc.OutputImage);

    if Trim(sc.OutputPNGFileName) <> '' then
      sc.Save;
  finally
    sc.Free;
  end;
end;

procedure TMainForm.btTestClick(Sender: TObject);
var
  img: TSingleDynArray2;
  i: Integer;
  fn: String;
  sc1, sc100, scm: TScanCorrelator;
  sl: TStringList;
begin
  SetLength(img, 2048, 2048);

  img[512, 16 * 2 + 1024] := 1.0;
  img[1024, 128 * 2 + 1024] := 1.0;
  img[1536, 64 * 2 + 1024] := 1.0;
  img[2047, 256 * 2 + 1024] := 1.0;

  for i := 0 to 511 do
  begin
    img[i + 512, round(herp(0, 16, 128, 64, i / 512) * 2 + 1024)] := 1.0;
    img[i + 1024, round(herp(16, 128, 64, 256, i / 512) * 2 + 1024)] := 1.0;
    img[i + 1536, round(herp(128, 64, 256, 512, i / 512) * 2 + 1024)] := 1.0;
  end;

  DrawImage(img);

  fn := GetTempFileName;
  sl := TStringList.Create;
  sc1 := TScanCorrelator.Create(sl, 1);
  sc100 := TScanCorrelator.Create(sl, 100);
  sl.Text := 'data\think_mock.png';
  scm := TScanCorrelator.Create(sl);
  try
    scm.OutputPNGFileName := fn;
    scm.Run;
    sc1.OutputPNGFileName := fn;
    sc1.Run;
    sc100.OutputPNGFileName := fn;
    sc100.Run;

    DrawImage(scm.OutputImage);
  finally
    sc100.Free;
    sc1.Free;
    scm.Free;
    sl.Free;
    DeleteFile(fn);
  end;
end;

procedure TMainForm.DrawImage(const Img: TWordDynArray2);
var
  x, y, ix, iy: Integer;
  sc: PInteger;
  b: Byte;
  acc: Integer;
begin
  Image.Picture.Bitmap.PixelFormat := pf32bit;
  Image.Picture.Bitmap.Width := Length(Img[0]) shr CReducShift;
  Image.Picture.Bitmap.Height := Length(Img) shr CReducShift;

  Image.Picture.Bitmap.BeginUpdate;
  try
    for y := 0 to Image.Picture.Bitmap.Height - 1 do
    begin
      sc := Image.Picture.Bitmap.ScanLine[y];
      for x := 0 to Image.Picture.Bitmap.Width - 1 do
      begin
        acc  := 0;
        for iy := 0 to (1 shl CReducShift) - 1 do
          for ix := 0 to (1 shl CReducShift) - 1 do
            acc += Img[(y shl CReducShift) + iy, (x shl CReducShift) + ix];

        b := EnsureRange(Round(acc * (High(Byte)  / (High(Word) shl (CReducShift * 2)))), 0, High(Byte));

        sc^ := ToRGB(b, b, b);

        Inc(sc);
      end;
    end;
  finally
    Image.Picture.Bitmap.EndUpdate;
  end;

  Application.ProcessMessages;
end;

procedure TMainForm.DrawImage(const Img: TSingleDynArray2);
var
  x, y, ix, iy: Integer;
  sc: PInteger;
  b: Byte;
  acc: Double;
begin
  Image.Picture.Bitmap.PixelFormat := pf32bit;
  Image.Picture.Bitmap.Width := Length(Img[0]) shr CReducShift;
  Image.Picture.Bitmap.Height := Length(Img) shr CReducShift;

  Image.Picture.Bitmap.BeginUpdate;
  try
    for y := 0 to Image.Picture.Bitmap.Height - 1 do
    begin
      sc := Image.Picture.Bitmap.ScanLine[y];
      for x := 0 to Image.Picture.Bitmap.Width - 1 do
      begin
        acc  := 0;
        for iy := 0 to (1 shl CReducShift) - 1 do
          for ix := 0 to (1 shl CReducShift) - 1 do
            acc += Img[(y shl CReducShift) + iy, (x shl CReducShift) + ix];

        b := EnsureRange(Round(acc * (High(Byte)  / (1 shl (CReducShift * 2)))), 0, High(Byte));

        sc^ := ToRGB(b, b, b);

        Inc(sc);
      end;
    end;
  finally
    Image.Picture.Bitmap.EndUpdate;
  end;

  Application.ProcessMessages;
end;

end.

