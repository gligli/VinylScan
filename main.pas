unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Types, scan2track, scancorrelator, utils, math, inputscan, FilterIIRLPBessel, FilterIIRHPBessel;

const
  CReducShift = 0;
  CReducFactor = 1.0 / (1 shl CReducShift);

type

  { TMainForm }

  TMainForm = class(TForm)
    btInPNGs: TButton;
    btOutPNG: TButton;
    btInPNG: TButton;
    btOutWAV: TButton;
    btScan2Track: TButton;
    btScansCorrelator: TButton;
    btTest: TButton;
    cbDPI: TComboBox;
    cbImgDerv: TComboBox;
    cbSR: TComboBox;
    cbMethod: TComboBox;
    edInputPNG: TEdit;
    edOutputPNG: TEdit;
    edOutputWAV: TEdit;
    Image: TImage;
    mmInputPNGs: TMemo;
    odInPNGs: TOpenDialog;
    odInPNG: TOpenDialog;
    pnSettings: TPanel;
    sdOutPNG: TSaveDialog;
    sdOutWAV: TSaveDialog;
    procedure btOutPNGClick(Sender: TObject);
    procedure btInPNGClick(Sender: TObject);
    procedure btOutWAVClick(Sender: TObject);
    procedure btInPNGsClick(Sender: TObject);
    procedure btScan2TrackClick(Sender: TObject);
    procedure btScansCorrelatorClick(Sender: TObject);
    procedure btTestClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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

  s2t := TScan2Track.Create(StrToIntDef(cbSR.Text, 48000), 16, StrToIntDef(cbDPI.Text, 2400));
  try
    s2t.Method := TMinimizeMethod(cbMethod.ItemIndex);
    s2t.OutputWAVFileName := edOutputWAV.Text;
    s2t.Scan.PNGFileName := edInputPNG.Text;
    s2t.Scan.ImageDerivationOperator := TImageDerivationOperator(cbImgDerv.ItemIndex);

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

procedure TMainForm.btInPNGsClick(Sender: TObject);
begin
  if odInPNGs.Execute then
    mmInputPNGs.Lines.Assign(odInPNGs.Files);
end;

procedure TMainForm.btInPNGClick(Sender: TObject);
begin
  if odInPNG.Execute then
    edInputPNG.Text := odInPNG.FileName;
end;

procedure TMainForm.btOutWAVClick(Sender: TObject);
begin
  if sdOutWAV.Execute then
    edOutputWAV.Text := sdOutWAV.FileName;
end;

procedure TMainForm.btOutPNGClick(Sender: TObject);
begin
  if sdOutPNG.Execute then
    edOutputPNG.Text := sdOutPNG.FileName;
end;

procedure TMainForm.btScansCorrelatorClick(Sender: TObject);
var
  sc: TScanCorrelator;
begin
  sc := TScanCorrelator.Create(mmInputPNGs.Lines, StrToIntDef(cbDPI.Text, 2400));
  try
    sc.Method := TMinimizeMethod(cbMethod.ItemIndex);
    sc.ImageDerivationOperator := TImageDerivationOperator(cbImgDerv.ItemIndex);
    sc.OutputPNGFileName := edOutputPNG.Text;

    sc.LoadPNGs;

    sc.Process;

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
  sc1, sc100, scmP, scmA, scmL: TScanCorrelator;
  sl: TStringList;
  fltLP: TFilterIIRLPBessel;
  fltHP: TFilterIIRHPBessel;
  smps: TSmallIntDynArray;
begin
  SetLength(smps, 48000 * 2);
  fltLP := TFilterIIRLPBessel.Create(nil);
  fltHP := TFilterIIRHPBessel.Create(nil);
  try
    fltLP.SampleRate := 48000;
    fltLP.FreqCut1 := 1000;
    fltLP.Order := 4;

    fltHP.SampleRate := 48000;
    fltHP.FreqCut1 := 1000;
    fltHP.Order := 4;

    for i := 0 to 48000 - 1 do
    begin
      smps[i] := Make16BitSample(fltLP.FilterFilter((i mod 1000) / 1000 * 2.0 - 1.0));
      smps[i + 48000] := Make16BitSample(fltHP.FilterFilter((i mod 1000) / 1000 * 2.0 - 1.0));
    end;
  finally
    fltLP.Free;
    fltHP.Free;
  end;
  CreateWAV(1, 16, 48000, 'ut.wav', smps);

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
  sl.Add('data\think_mock.png');
  sl.Add('data\think_mock2.png');
  sl.Add('data\think_mock3.png');
  scmL := TScanCorrelator.Create(sl);
  scmA := TScanCorrelator.Create(sl);
  scmP := TScanCorrelator.Create(sl);
  try
    sc1.OutputPNGFileName := fn;
    sc100.OutputPNGFileName := fn;
    scmL.OutputPNGFileName := fn;
    scmA.OutputPNGFileName := fn;
    scmP.OutputPNGFileName := fn;

    scmL.Method := mmLBFGS;
    scmA.Method := mmASA;
    scmP.Method := mmPowell;

    sc1.LoadPNGs;
    sc1.Process;
    sc1.Save;

    sc100.LoadPNGs;
    sc100.Process;
    sc100.Save;

    scmL.LoadPNGs;
    scmL.Process;
    DrawImage(scmL.OutputImage);
    scmL.Save;

    scmA.LoadPNGs;
    scmA.Process;
    DrawImage(scmA.OutputImage);
    scmA.Save;

    scmP.LoadPNGs;
    scmP.Process;
    DrawImage(scmP.OutputImage);
    scmP.Save;
  finally
    sc100.Free;
    sc1.Free;
    scmL.Free;
    scmA.Free;
    scmP.Free;
    sl.Free;
    DeleteFile(fn);
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  pnSettings.ControlStyle := pnSettings.ControlStyle + [csOpaque];
end;

procedure TMainForm.DrawImage(const Img: TWordDynArray2);
var
  x, y, ix, iy: Integer;
  sc: PByte;
  b: Byte;
  acc: Integer;
begin
  Image.Picture.Bitmap.PixelFormat := pf8bit;
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

        sc^ := b;

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
  sc: PByte;
  b: Byte;
  acc: Double;
begin
  Image.Picture.Bitmap.PixelFormat := pf8bit;
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

        sc^ := b;

        Inc(sc);
      end;
    end;
  finally
    Image.Picture.Bitmap.EndUpdate;
  end;

  Application.ProcessMessages;
end;

end.

