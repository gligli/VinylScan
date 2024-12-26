unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Types, scan2track, scancorrelator, utils, math, inputscan, FilterIIRLPBessel, FilterIIRHPBessel;

const
  CReducShift = 2;
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
    chkCorrect: TCheckBox;
    cbDPI: TComboBox;
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

  public
    procedure DrawExtents(AScan: TInputScan);
    procedure DrawImage(const Img: TWordDynArray2);
    procedure DrawPoints(const APoints: TPointFList; AColor: TColor);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.btScan2TrackClick(Sender: TObject);
var
  s2t: TScan2Track;
begin
  s2t := TScan2Track.Create(StrToIntDef(cbSR.Text, 48000), 16, StrToIntDef(cbDPI.Text, 2400));
  try
    s2t.OutputWAVFileName := edOutputWAV.Text;
    s2t.Scan.PNGFileName := edInputPNG.Text;

    s2t.Scan.LoadPNG;

    DrawImage(s2t.Scan.Image);

    s2t.Scan.FindTrack;

    DrawExtents(s2t.Scan);

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
    sc.OutputPNGFileName := edOutputPNG.Text;
    sc.CorrectAngles := chkCorrect.Checked;

    sc.LoadPNGs;

    if Length(sc.InputScans) > 0 then
    begin
      DrawImage(sc.InputScans[0].Image);
      DrawExtents(sc.InputScans[0]);
    end;

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
  img: TWordDynArray2;
  i: Integer;
  fn: String;
  sc1, sc100, scm: TScanCorrelator;
  sl: TStringList;
  fltLP: TFilterIIRLPBessel;
  fltHP: TFilterIIRHPBessel;
  smps: TSmallIntDynArray;
  mm: TMinimizeMethod;
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

  img[512, 16 * 2 + 1024] := High(Word);
  img[1024, 128 * 2 + 1024] := High(Word);
  img[1536, 64 * 2 + 1024] := High(Word);
  img[2047, 256 * 2 + 1024] := High(Word);

  for i := 0 to 511 do
  begin
    img[i + 512, round(herp(0, 16, 128, 64, i / 512) * 2 + 1024)] := High(Word);
    img[i + 1024, round(herp(16, 128, 64, 256, i / 512) * 2 + 1024)] := High(Word);
    img[i + 1536, round(herp(128, 64, 256, 512, i / 512) * 2 + 1024)] := High(Word);

    img[i + 512, round(cerp(0, 16, 128, 64, i / 512) * 2 + 1024) - 256] := High(Word);
    img[i + 1024, round(cerp(16, 128, 64, 256, i / 512) * 2 + 1024) - 256] := High(Word);
    img[i + 1536, round(cerp(128, 64, 256, 512, i / 512) * 2 + 1024) - 256] := High(Word);

    img[i + 512, round(lerp(16, 128, i / 512) * 2 + 1024) - 512] := High(Word);
    img[i + 1024, round(lerp(128, 64, i / 512) * 2 + 1024) - 512] := High(Word);
    img[i + 1536, round(lerp(64, 256, i / 512) * 2 + 1024) - 512] := High(Word);
  end;

  DrawImage(img);

  fn := GetTempFileName;
  sl := TStringList.Create;
  try
    sc1 := TScanCorrelator.Create(sl, 1);
    sc100 := TScanCorrelator.Create(sl, 100);
    try
      sc1.OutputPNGFileName := fn;
      sc100.OutputPNGFileName := fn;

      sc1.LoadPNGs;
      sc1.Process;
      sc1.Save;

      sc100.LoadPNGs;
      sc100.Process;
      sc100.Save;
    finally
      sc100.Free;
      sc1.Free;
    end;

    sl.Add('data\think_mock.png');
    sl.Add('data\think_mock2.png');
    sl.Add('data\think_mock3.png');

    for mm := Succ(mmNone) to High(TMinimizeMethod) do
    begin
      scm := TScanCorrelator.Create(sl);
      try
        scm.OutputPNGFileName := fn;
        scm.Method := mm;

        scm.LoadPNGs;
        scm.Process;
        DrawImage(scm.OutputImage);
        scm.Save;
      finally
        scm.Free;
      end;
    end;

  finally
    sl.Free;
    DeleteFile(fn);
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  pnSettings.ControlStyle := pnSettings.ControlStyle + [csOpaque];
  Image.ControlStyle := Image.ControlStyle + [csOpaque];
end;

procedure TMainForm.DrawExtents(AScan: TInputScan);
var
  C: TCanvas;
  cx, cy, sx, sy, rfx, rfy, rcx, rcy, rax, ray: Integer;
begin
  C := Image.Picture.Bitmap.Canvas;

  C.Brush.Style := bsClear;
  C.Pen.Color := clRed;
  C.Pen.Style := psDot;

  cx := Round(AScan.Center.X * CReducFactor);
  cy := Round(AScan.Center.Y * CReducFactor);
  sx := Round(AScan.GrooveStartPoint.X * CReducFactor);
  sy := Round(AScan.GrooveStartPoint.Y * CReducFactor);
  rfx := Round(AScan.FirstGrooveRadius) shr CReducShift;
  rfy := Round(AScan.FirstGrooveRadius) shr CReducShift;
  rcx := Round(AScan.ConcentricGrooveRadius) shr CReducShift;
  rcy := Round(AScan.ConcentricGrooveRadius) shr CReducShift;
  rax := Round(C45RpmAdapterSize * 0.5 * AScan.DPI) shr CReducShift;
  ray := Round(C45RpmAdapterSize * 0.5 * AScan.DPI) shr CReducShift;

  C.Line(cx - 8, cy, cx + 9, cy);
  C.Line(cx, cy - 8, cx, cy + 9);

  C.Line(sx - 8, sy, sx + 9, sy);
  C.Line(sx, sy - 8, sx, sy + 9);

  C.EllipseC(cx, cy, rfx, rfy);
  C.EllipseC(cx, cy, rcx, rcy);
  C.EllipseC(cx, cy, rax, ray);

  HorzScrollBar.Position := cx - Width div 2;
  VertScrollBar.Position := cy - Height div 2;

  Application.ProcessMessages;
end;

procedure TMainForm.DrawImage(const Img: TWordDynArray2);
var
  x, y, ix, iy: Integer;
  sc: PCardinal;
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

procedure TMainForm.DrawPoints(const APoints: TPointFList; AColor: TColor);
var
  i: Integer;
  sc: PCardinal;
begin
  Image.Picture.Bitmap.BeginUpdate;
  try
    for i := 0 to APoints.Count - 1 do
    begin
      sc := Image.Picture.Bitmap.ScanLine[round(APoints[i].Y) shr CReducShift];
      Inc(sc, round(APoints[i].X) shr CReducShift);
      sc^ := SwapRB(AColor);
    end;
  finally
    Image.Picture.Bitmap.EndUpdate;
  end;

  Application.ProcessMessages;
end;

end.

