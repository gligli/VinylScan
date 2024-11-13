unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, scan2track;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Image1: TImage;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  s2t: TScan2Track;
  i, j: Integer;
begin
  s2t := TScan2Track.Create;
  try
    s2t.PNGFileName := 'data\think1.png';
    s2t.Run;

    Image1.Picture.Bitmap.PixelFormat := pf32bit;
    Image1.Picture.Bitmap.Width := Length(s2t.Image[0]) shr 5;
    Image1.Picture.Bitmap.Height := Length(s2t.Image) shr 5;

    for j := 0 to Length(s2t.Image) shr 5 - 1 do
      for i := 0 to Length(s2t.Image[0]) shr 5 - 1 do
        Image1.Picture.Bitmap.Canvas.Pixels[i, j] := s2t.Image[j shl 5, i shl 5];

  finally
    s2t.Free;
  end;
end;

end.

