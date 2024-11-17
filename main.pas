unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, scan2track, scancorrelator, utils, math;

const
  CReducShift = 2;
  CReducFactor = 1.0 / (1 shl CReducShift);

type

  { TForm1 }

  TForm1 = class(TForm)
    btScan2Track: TButton;
    btScansCorrelator: TButton;
    edInputPNG: TEdit;
    edOutputPNG: TEdit;
    Image: TImage;
    mmInputPNGs: TMemo;
    procedure btScan2TrackClick(Sender: TObject);
    procedure btScansCorrelatorClick(Sender: TObject);
  private

    procedure DrawImage(const Img: TSingleDynArray2);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.btScan2TrackClick(Sender: TObject);
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

  s2t := TScan2Track.Create;//(8000);
  try
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

procedure TForm1.btScansCorrelatorClick(Sender: TObject);
var
  sc: TScanCorrelator;
begin
  sc := TScanCorrelator.Create(mmInputPNGs.Lines);
  try
    sc.OutputPNGFileName := edOutputPNG.Text;

    sc.LoadPNGs;

    sc.Correlate;

    DrawImage(sc.OutputImage);

    sc.Save;
  finally
    sc.Free;
  end;
end;

procedure TForm1.DrawImage(const Img: TSingleDynArray2);
var
  x, y, ix, iy: Integer;
  sc: PInteger;
  b: Byte;
  acc: Double;
  C: TCanvas;
begin
  C := Image.Picture.Bitmap.Canvas;

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
        acc /=  1 shl (CReducShift * 2);

        b := EnsureRange(round(acc * 255.0), 0, 255);
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

