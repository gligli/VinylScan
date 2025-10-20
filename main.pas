unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, windows, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Spin, ComCtrls, Types, scan2track,
  scancorrelator, utils, math, inputscan, profiles, FilterIIRLPBessel, FilterIIRHPBessel;

type

  { TMainForm }

  TMainForm = class(TForm)
    btInPNGs: TButton;
    btOutPNG: TButton;
    btInPNG: TButton;
    btOutWAV: TButton;
    btScan2Track: TButton;
    btScansCorrelator: TButton;
    cbProfile: TComboBox;
    cbQSRatio: TComboBox;
    chkFixCIS: TCheckBox;
    chkDefaultDPI: TCheckBox;
    chkCorrect: TCheckBox;
    cbDPI: TComboBox;
    cbSR: TComboBox;
    chkAlignBlur: TCheckBox;
    chkAnalyze: TCheckBox;
    edInputPNG: TEdit;
    edOutputPNG: TEdit;
    edOutputWAV: TEdit;
    ffProfile: TLabel;
    Image: TImage;
    llQSRatio: TLabel;
    llDPI: TLabel;
    llBlend: TLabel;
    llSR: TLabel;
    llPrec: TLabel;
    mmInputPNGs: TMemo;
    odInPNGs: TOpenDialog;
    odInPNG: TOpenDialog;
    pbS2T: TProgressBar;
    pnSettings: TPanel;
    sdOutPNG: TSaveDialog;
    sdOutWAV: TSaveDialog;
    sePrec: TSpinEdit;
    seBlend: TSpinEdit;
    procedure btOutPNGClick(Sender: TObject);
    procedure btInPNGClick(Sender: TObject);
    procedure btOutWAVClick(Sender: TObject);
    procedure btInPNGsClick(Sender: TObject);
    procedure btScan2TrackClick(Sender: TObject);
    procedure btScansCorrelatorClick(Sender: TObject);
    procedure cbProfileChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FProfiles: TProfiles;

    FReducRatio: Integer;
    FReducFactor: Double;
    FLastTickCount: QWord;
    FPoints: TPointValueList;

    function OnSample(Sender: TScan2Track; Sample, pxPosition: TPointD; Percent: Double; Finished: Boolean): Boolean;
  public
    procedure UnitTests;

    procedure SetReduc(AImageWidth, AImageHeight: Integer);
    procedure DrawExtents(AScan: TInputScan);
    procedure DrawImage(const Img: TWordDynArray; AWidth, AHeight: Integer);
    procedure DrawImage(const Img: TDoubleDynArray; AWidth, AHeight: Integer);
    procedure DrawPoints(const APoints: TPointValueList);

    property ReducFactor: Double read FReducFactor;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.btScan2TrackClick(Sender: TObject);
var
  fn: String;
  s2t: TScan2Track;
begin
  if not pnSettings.Enabled then
    Exit;

  pnSettings.Enabled := False;
  try
    fn := edInputPNG.Text;

    s2t := TScan2Track.Create(FProfiles.CurrentProfileRef, fn, StrToIntDef(cbDPI.Text, 2400), False, StrToIntDef(cbSR.Text, 48000), sePrec.Value);
    try
      s2t.OnSample := @OnSample;
      s2t.OutputWAVFileName := edOutputWAV.Text;

      s2t.LoadScan;

      DrawImage(s2t.InputScan.ProcessedImage, s2t.InputScan.Width, s2t.InputScan.Height);
      DrawExtents(s2t.InputScan);

      s2t.EvalTrack;

    finally
      s2t.Free;
    end;
  finally
    pnSettings.Enabled := True;
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
  if not pnSettings.Enabled then
    Exit;

  pnSettings.Enabled := False;
  sc := TScanCorrelator.Create(FProfiles.CurrentProfileRef, mmInputPNGs.Lines, StrToIntDef(cbDPI.Text, 2400));
  try
    sc.OutputPNGFileName := edOutputPNG.Text;
    sc.FixCISScanners := chkFixCIS.Checked;
    sc.AlignPass := chkAnalyze.Checked;
    sc.AlignOnBlurred := chkAlignBlur.Checked;
    sc.CorrectPass := chkCorrect.Checked;
    sc.RebuildBlendCount := seBlend.Value;
    sc.RebuildScaled := not chkDefaultDPI.Checked;
    sc.QualitySpeedRatio := StrToFloatDef(cbQSRatio.Text, 1.0, InvariantFormatSettings);

    sc.LoadScans;

    if Length(sc.InputScans) > 0 then
    begin
      DrawImage(sc.InputScans[0].Image, sc.InputScans[0].Width, sc.InputScans[0].Height);
      DrawExtents(sc.InputScans[0]);
    end;

    sc.Process;

    DrawImage(sc.OutputImage, sc.OutputWidth, sc.OutputHeight);

    if Trim(sc.OutputPNGFileName) <> '' then
      sc.Save;
  finally
    sc.Free;
    pnSettings.Enabled := True;
  end;
end;

procedure TMainForm.cbProfileChange(Sender: TObject);
begin
  if InRange(cbProfile.ItemIndex, 0, High(FProfiles.Profiles)) then
    FProfiles.CurrentProfileRef := FProfiles.Profiles[cbProfile.ItemIndex];
end;

procedure TMainForm.UnitTests;
const
  CHerpSample : array[0 .. 4*4 - 1] of Word = (
    1000, 4000, 5000, 6000,
    1100, 4100, 5100, 6100,
    1900, 4900, 5900, 6900,
    0,10000,0,10000
  );

  CRankSampleA: array[0..9] of Double = (1,10,3,8,6,42,123,21,69,38);
  CRankSampleB: array[0..9] of Double = (1.5,10.5,3.5,8.5,6.5,42.5,123.5,21.5,69.5,38.5);

  CSampleRank: array[0..9] of Double = (1,5,2,4,3,8,10,6,9,7);
var
  i: Integer;
  fn: String;
  sc1, sc100, scm: TScanCorrelator;
  sl: TStringList;
  fltLP: TFilterIIRLPBessel;
  fltHP: TFilterIIRHPBessel;
  smps: TSmallIntDynArray;
  h1,h2: Double;
  ra, rb: TSpearmanRankDynArray;
begin
  h1 := herpXY(@CHerpSample[5], 4, 0.5, 0.1);
  h2 := herpXY_asm(@CHerpSample[5], 4, 0.5, 0.1);

  Assert(h1 = h2);

  SetLength(ra, Length(CRankSampleA));
  SetLength(rb, Length(CRankSampleB));
  SpearmanPrepareRanks(CRankSampleA, ra);
  SpearmanPrepareRanks(CRankSampleB, rb);
  Assert(SpearmanRankCorrelation(ra, rb) = 1.0);
  for i := 0 to High(CSampleRank) do
  begin
    Assert(ra[i].Rank = CSampleRank[i]);
    Assert(rb[i].Rank = CSampleRank[i]);
  end;

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
    sc1 := TScanCorrelator.Create(FProfiles.CurrentProfileRef, sl, 1);
    sc100 := TScanCorrelator.Create(FProfiles.CurrentProfileRef, sl, 100);
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

    scm := TScanCorrelator.Create(FProfiles.CurrentProfileRef, sl);
    try
      scm.OutputPNGFileName := fn;
      scm.CorrectPass := True;
      scm.RebuildBlendCount := MaxInt;

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
  iProfile: Integer;
  sl: TStringList;
  sc: TScanCorrelator;
  s2t: TScan2Track;
begin
  pnSettings.ControlStyle := pnSettings.ControlStyle + [csOpaque];
  Image.ControlStyle := Image.ControlStyle + [csOpaque];
  FPoints := TPointValueList.Create;

  FProfiles := TProfiles.Create(ExtractFilePath(ParamStr(0)) + 'profiles');

  for iProfile := 0 to High(FProfiles.Profiles) do
  begin
    cbProfile.AddItem(FProfiles.Profiles[iProfile].Name, nil);
    if FProfiles.Profiles[iProfile] = FProfiles.CurrentProfileRef then
      cbProfile.ItemIndex := iProfile;
  end;

  sl := TStringList.Create;
  sc := TScanCorrelator.Create(FProfiles.CurrentProfileRef, sl);
  s2t := TScan2Track.Create(FProfiles.CurrentProfileRef, '');
  try
    chkFixCIS.Checked := sc.FixCISScanners;
    chkAnalyze.Checked := sc.AlignPass;
    chkAlignBlur.Checked := sc.AlignOnBlurred;
    chkCorrect.Checked := sc.CorrectPass;
    seBlend.Value := sc.RebuildBlendCount;
    chkDefaultDPI.Checked := not sc.RebuildScaled;
    cbQSRatio.Text := FormatFloat('0.0', sc.QualitySpeedRatio, InvariantFormatSettings);
    cbDPI.Text := IntToStr(sc.OutputDPI);
    cbSR.Text := IntToStr(s2t.SampleRate);
    sePrec.Value := s2t.DecoderPrecision;
  finally
    s2t.Free;
    sc.Free;
    sl.Free;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FPoints.Free;
  FProfiles.Free;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if pnSettings.Enabled then
    case Key of
      VK_F10:
        btScansCorrelatorClick(nil);
      VK_F11:
        btScan2TrackClick(nil);
      VK_F12:
        UnitTests;
    end;
end;

function TMainForm.OnSample(Sender: TScan2Track; Sample, pxPosition: TPointD; Percent: Double; Finished: Boolean): Boolean;
const
  CPointsPerSample = 1;
var
  SecondsAtATime: Double;
  tc: QWord;
begin
  Result := True;

  SecondsAtATime := 0.25 / Sender.ProfileRef.RevolutionsPerSecond;

  FPoints.Add(TPointValue.Create(pxPosition.X, pxPosition.Y, clLime));

  if Finished or (FPoints.Count >= Sender.SampleRate * SecondsAtATime * CPointsPerSample) then
  begin
    Result := not ((GetForegroundWindow = Handle) and (GetAsyncKeyState(VK_ESCAPE) and $8000 <> 0));

    DrawPoints(FPoints);

    tc := GetTickCount64;
    Write(Percent:6:2, '%,', DivDef(FPoints.Count / (Sender.SampleRate * CPointsPerSample) * 1000.0, tc - FLastTickCount, 0.0):6:3, 'x', #13);
    pbS2T.Position := Round(Percent);
    FLastTickCount := tc;

    FPoints.Clear;

    if Finished or not Result then
      WriteLn;
  end;
end;

procedure TMainForm.SetReduc(AImageWidth, AImageHeight: Integer);
begin
  FReducRatio := Max(1, Ceil(AImageHeight / ClientHeight));
  FReducFactor := 1.0 / FReducRatio;
end;

procedure TMainForm.DrawExtents(AScan: TInputScan);
var
  C: TCanvas;
  cx, cy, sx, sy, rfx, rfy, rcx, rcy, rax, ray: Integer;
  cer: TRect;
  sk: TPointD;

  procedure CropLine(AAngle: Double);
  var
    sn, cs: Double;
  begin
    SinCos(AAngle, sn, cs);
    C.Line(Round(cs * rfx + cx), Round(sn * rfx + cy), Round(cs * rcx + cx), Round(sn * rcx + cy));
  end;

begin
  C := Image.Picture.Bitmap.Canvas;

  C.Brush.Style := bsClear;
  C.Pen.Color := clRed;
  C.Pen.Style := psDot;

  sk := AScan.Skew;

  sk.X *= FReducFactor;
  sk.Y *= FReducFactor;

  cer := AScan.CenterExtents;
  cer.Left := Round(cer.Left * sk.X);
  cer.Top := Round(cer.Top * sk.Y);
  cer.Right := Round(cer.Right * sk.X);
  cer.Bottom := Round(cer.Bottom * sk.Y);

  cx := Round(AScan.Center.X * sk.X);
  cy := Round(AScan.Center.Y * sk.Y);
  sx := Round(AScan.GrooveStartPoint.X * sk.X);
  sy := Round(AScan.GrooveStartPoint.Y * sk.Y);
  rfx := Round(AScan.SetDownRadius * sk.X);
  rfy := Round(AScan.SetDownRadius * sk.Y);
  rcx := Round(AScan.ConcentricGrooveRadius * sk.X);
  rcy := Round(AScan.ConcentricGrooveRadius * sk.Y);
  rax := Round(AScan.ProfileRef.AdapterSize * 0.5 * AScan.DPI * sk.X);
  ray := Round(AScan.ProfileRef.AdapterSize * 0.5 * AScan.DPI * sk.Y);

  C.Rectangle(cer);

  C.Line(cx - 8, cy, cx + 9, cy);
  C.Line(cx, cy - 8, cx, cy + 9);

  C.Line(sx - 8, sy, sx + 9, sy);
  C.Line(sx, sy - 8, sx, sy + 9);

  C.EllipseC(cx, cy, rfx, rfy);
  C.EllipseC(cx, cy, rcx, rcy);
  C.EllipseC(cx, cy, rax, ray);

  if (NormalizedAngleDiff(AScan.CropData.StartAngle, AScan.CropData.EndAngle) <> 0) or
      (NormalizedAngleDiff(AScan.CropData.StartAngleMirror, AScan.CropData.EndAngleMirror) <> 0) then
  begin
    CropLine(AScan.CropData.StartAngle);
    CropLine(AScan.CropData.EndAngle);
    CropLine(lerp(AScan.CropData.StartAngle, AScan.CropData.EndAngle, 0.5));

    CropLine(AScan.CropData.StartAngleMirror);
    CropLine(AScan.CropData.EndAngleMirror);
    CropLine(lerp(AScan.CropData.StartAngleMirror, AScan.CropData.EndAngleMirror, 0.5));
  end;

  HorzScrollBar.Position := sx - Width div 2;
  VertScrollBar.Position := sy - Height div 2;

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

procedure TMainForm.DrawImage(const Img: TDoubleDynArray; AWidth, AHeight: Integer);
var
  x, y, ix, iy: Integer;
  sc: PCardinal;
  b: Byte;
  acc: Double;
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

        b := Round(EnsureRange(acc / Sqr(FReducRatio), -1, 1) * High(ShortInt) - Low(ShortInt));

        sc^ := ToRGB(b, b, b);

        Inc(sc);
      end;
    end;
  finally
    Image.Picture.Bitmap.EndUpdate;
  end;

  Application.ProcessMessages;
end;

procedure TMainForm.DrawPoints(const APoints: TPointValueList);
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
      sc^ := SwapRB(round(APoints[i].Value));
    end;
  finally
    Image.Picture.Bitmap.EndUpdate;
  end;

  Application.ProcessMessages;
end;

end.

