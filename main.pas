unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, scan2track, utils;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Image1: TImage;
    ScrollBox1: TScrollBox;
    procedure Button1Click(Sender: TObject);
    procedure Image1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
const
  CReducShift = 4;
var
  s2t: TScan2Track;
  i, j, cx, cy, offf, offc: Integer;
  sc: PInteger;
  b: Byte;
  C: TCanvas;
begin
  s2t := TScan2Track.Create;
  try
    s2t.PNGFileName := 'data\think0.png';
    s2t.Run;

    Image1.Picture.Bitmap.PixelFormat := pf32bit;
    Image1.Picture.Bitmap.Width := Length(s2t.Image[0]) shr CReducShift;
    Image1.Picture.Bitmap.Height := Length(s2t.Image) shr CReducShift;

    Image1.Picture.Bitmap.BeginUpdate;
    try
      for j := 0 to Length(s2t.Image) shr CReducShift - 1 do
      begin
        sc := Image1.Picture.Bitmap.ScanLine[j];
        for i := 0 to Length(s2t.Image[0]) shr CReducShift - 1 do
        begin
          b := s2t.Image[j shl CReducShift, i shl CReducShift];
          sc^ := ToRGB(b, b, b);

          Inc(sc);
        end;
      end;
    finally
      Image1.Picture.Bitmap.EndUpdate;
    end;

    C := Image1.Picture.Bitmap.Canvas;

    cx := s2t.Center.X shr CReducShift;
    cy := s2t.Center.Y shr CReducShift;
    offf := Round(s2t.FirstGrooveOffset) shr CReducShift;
    offc := Round(s2t.ConcentricGrooveOffset) shr CReducShift;

    C.AutoRedraw := True;
    C.Brush.Style := bsClear;
    C.Pen.Color := clRed;
    C.Pen.Color := clRed;
    C.Pen.Style := psSolid;
    C.Line(cx - 8, cy, cx + 9, cy);
    C.Line(cx, cy - 8, cx, cy + 9);

    C.EllipseC(cx, cy, offf, offf);
    C.EllipseC(cx, cy, offc, offc);

  finally
    s2t.Free;
  end;
end;

procedure TForm1.Image1Click(Sender: TObject);
begin

end;

end.

