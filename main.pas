unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, windows, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Types, scan2track, scancorrelator, utils, math, inputscan, FilterIIRLPBessel, FilterIIRHPBessel;

type

  { TMainForm }

  TMainForm = class(TForm)
    btInPNGs: TButton;
    btOutPNG: TButton;
    btInPNG: TButton;
    btOutWAV: TButton;
    btScan2Track: TButton;
    btScansCorrelator: TButton;
    chkBlend: TCheckBox;
    chkDefaultDPI: TCheckBox;
    chkCorrect: TCheckBox;
    cbDPI: TComboBox;
    cbSR: TComboBox;
    chkBrickLim: TCheckBox;
    chkOptimize: TCheckBox;
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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FReducRatio: Integer;
    FReducFactor: Double;
    FLastTickCount: QWord;
    FPoints: TPointFList;

    function OnSample(Sender: TScan2Track; X, Y, Percent: Double; Finished: Boolean): Boolean;
  public
    procedure UnitTests;

    procedure SetReduc(AImageWidth, AImageHeight: Integer);
    procedure DrawExtents(AScan: TInputScan);
    procedure DrawImage(const Img: TWordDynArray; AWidth, AHeight: Integer);
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
    s2t.OnSample := @OnSample;
    s2t.OutputWAVFileName := edOutputWAV.Text;
    s2t.Scan.ImageFileName := edInputPNG.Text;

    s2t.LoadPNG;

    DrawImage(s2t.Scan.Image, s2t.Scan.Width, s2t.Scan.Height);
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
    sc.OutputPNGFileName := edOutputPNG.Text;
    sc.BrickwallLimitScans := chkBrickLim.Checked;
    sc.AnalyzeMinimize := chkOptimize.Checked;
    sc.CorrectAngles := chkCorrect.Checked;
    sc.RebuildBlended := chkBlend.Checked;
    sc.RebuildScaled := not chkDefaultDPI.Checked;

    sc.LoadScans;

    if Length(sc.InputScans) > 0 then
    begin
      DrawImage(sc.InputScans[0].LeveledImage, sc.InputScans[0].Width, sc.InputScans[0].Height);
      DrawExtents(sc.InputScans[0]);
    end;

    sc.Process;

    DrawImage(sc.OutputImage, sc.OutputWidth, sc.OutputHeight);

    if Trim(sc.OutputPNGFileName) <> '' then
      sc.Save;
  finally
    sc.Free;
  end;
end;

procedure TMainForm.UnitTests;
var
  i: Integer;
  fn: String;
  sc1, sc100, scm: TScanCorrelator;
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

  fn := GetTempFileName;
  sl := TStringList.Create;
  try
    sc1 := TScanCorrelator.Create(sl, 1);
    sc100 := TScanCorrelator.Create(sl, 100);
    try
      sc1.OutputPNGFileName := fn;
      sc100.OutputPNGFileName := fn;

      sc1.LoadScans;
      sc1.Process;
      sc1.Save;

      sc100.LoadScans;
      sc100.Process;
      sc100.Save;
    finally
      sc100.Free;
      sc1.Free;
    end;

    sl.Add('data\think_mock.png');
    sl.Add('data\think_mock2.png');
    sl.Add('data\think_mock3.png');

    scm := TScanCorrelator.Create(sl);
    try
      scm.OutputPNGFileName := fn;
      scm.CorrectAngles := True;
      scm.RebuildBlended := True;

      scm.LoadScans;
      scm.Process;
      DrawImage(scm.OutputImage, scm.OutputWidth, scm.OutputHeight);
      scm.Save;
    finally
      scm.Free;
    end;

  finally
    sl.Free;
    DeleteFile(fn);
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  sl: TStringList;
  sc: TScanCorrelator;
  s2t: TScan2Track;
begin
  pnSettings.ControlStyle := pnSettings.ControlStyle + [csOpaque];
  Image.ControlStyle := Image.ControlStyle + [csOpaque];
  FPoints := TPointFList.Create;

  sl := TStringList.Create;
  sc := TScanCorrelator.Create(sl);
  s2t := TScan2Track.Create;
  try
    chkBrickLim.Checked := sc.BrickwallLimitScans;
    chkOptimize.Checked := sc.AnalyzeMinimize;
    chkCorrect.Checked := sc.CorrectAngles;
    chkBlend.Checked := sc.RebuildBlended;
    chkDefaultDPI.Checked := not sc.RebuildScaled;
    cbDPI.Text := IntToStr(sc.OutputDPI);
    cbSR.Text := IntToStr(s2t.SampleRate);
  finally
    s2t.Free;
    sc.Free;
    sl.Free;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FPoints.Free;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_F10:
      btScansCorrelatorClick(nil);
    VK_F11:
      btScan2TrackClick(nil);
    VK_F12:
      UnitTests;
  end;
end;

function TMainForm.OnSample(Sender: TScan2Track; X, Y, Percent: Double; Finished: Boolean): Boolean;
const
  CSecondsAtATime = 0.1;
var
  tc: QWord;
begin
  Result := True;

  FPoints.Add(TPointF.Create(X, Y));

  if Finished or (FPoints.Count >= Sender.SampleRate * CSecondsAtATime) then
  begin
    Result := not ((GetForegroundWindow = Handle) and (GetAsyncKeyState(VK_ESCAPE) and $8000 <> 0));

    DrawPoints(FPoints, clLime);

    tc := GetTickCount64;
    Write(Percent:6:2, '%,', DivDef(FPoints.Count / Sender.SampleRate * 1000.0, tc - FLastTickCount, 0.0):6:3, 'x', #13);
    FLastTickCount := tc;

    FPoints.Clear;
  end;
end;

procedure TMainForm.SetReduc(AImageWidth, AImageHeight: Integer);
begin
  FReducRatio := Max(1, Floor(AImageWidth / Width));
  FReducFactor := 1.0 / FReducRatio;
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

  cx := Round(AScan.Center.X * FReducFactor);
  cy := Round(AScan.Center.Y * FReducFactor);
  sx := Round(AScan.GrooveStartPoint.X * FReducFactor);
  sy := Round(AScan.GrooveStartPoint.Y * FReducFactor);
  rfx := Round(AScan.FirstGrooveRadius) div FReducRatio;
  rfy := Round(AScan.FirstGrooveRadius * AScan.SkewY) div FReducRatio;
  rcx := Round(AScan.ConcentricGrooveRadius) div FReducRatio;
  rcy := Round(AScan.ConcentricGrooveRadius * AScan.SkewY) div FReducRatio;
  rax := Round(C45RpmAdapterSize * 0.5 * AScan.DPI) div FReducRatio;
  ray := Round(C45RpmAdapterSize * 0.5 * AScan.DPI * AScan.SkewY) div FReducRatio;

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

procedure TMainForm.DrawImage(const Img: TWordDynArray; AWidth, AHeight: Integer);
var
  x, y, ix, iy: Integer;
  sc: PCardinal;
  b: Byte;
  acc: Integer;
begin
  SetReduc(AWidth, AHeight);

  Image.Picture.Bitmap.PixelFormat := pf32bit;
  Image.Picture.Bitmap.Width := AWidth div FReducRatio;
  Image.Picture.Bitmap.Height := AHeight div FReducRatio;

  Image.Picture.Bitmap.BeginUpdate;
  try
    for y := 0 to Image.Picture.Bitmap.Height - 1 do
    begin
      sc := Image.Picture.Bitmap.ScanLine[y];
      for x := 0 to Image.Picture.Bitmap.Width - 1 do
      begin
        acc  := 0;
        for iy := 0 to FReducRatio - 1 do
          for ix := 0 to FReducRatio - 1 do
            acc += Img[((y * FReducRatio) + iy) * AWidth + ((x * FReducRatio) + ix)];

        b := EnsureRange(Round(acc * (High(Byte)  / (High(Word) * Sqr(FReducRatio)))), 0, High(Byte));

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
      sc := Image.Picture.Bitmap.ScanLine[round(APoints[i].Y * FReducFactor)];
      Inc(sc, round(APoints[i].X * FReducFactor));
      sc^ := SwapRB(AColor);
    end;
  finally
    Image.Picture.Bitmap.EndUpdate;
  end;

  Application.ProcessMessages;
end;

end.

