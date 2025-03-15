unit GraphOpt;

{$MODE Delphi}

interface

uses
  SysUtils, LCLIntf, LCLType, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, Spin, ExtCtrls, ComCtrls;

type

  { TGraphOptDlg }

  TGraphOptDlg = class(TForm)
    TabbedNotebook1: TPageControl;
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    LimitGroupBox: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    ts1: TTabSheet;
    ts2: TTabSheet;
    ts3: TTabSheet;
    ts4: TTabSheet;
    XminSpinEdit: TSpinEdit;
    XmaxSpinEdit: TSpinEdit;
    Label3: TLabel;
    YminSpinEdit: TSpinEdit;
    Label4: TLabel;
    YmaxSpinEdit: TSpinEdit;
    ColorGroupBox: TGroupBox;
    BorderColorBtn: TButton;
    BorderColorShape: TShape;
    GraphColorBtn: TButton;
    GraphColorShape: TShape;
    FrameGroupBox: TGroupBox;
    GraphBorderCheckBox: TCheckBox;
    LgdBorderCheckBox: TCheckBox;
    GridRadioGroup: TRadioGroup;
    LgdFontDialog: TFontDialog;
    AxesFontDialog: TFontDialog;
    TitleFontDialog: TFontDialog;
    GraphColorDialog: TColorDialog;
    PointColorDialog: TColorDialog;
    LineColorDialog: TColorDialog;
    BorderColorDialog: TColorDialog;
    ConfigReadDialog: TOpenDialog;
    ConfigSaveDialog: TSaveDialog;
    XAxisGroupBox: TGroupBox;
    XPlotCheckBox: TCheckBox;
    XScaleCheckBox: TCheckBox;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    XMinEdit: TEdit;
    XMaxEdit: TEdit;
    XStepEdit: TEdit;
    YAxisGroupBox: TGroupBox;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    YPlotCheckBox: TCheckBox;
    YScaleCheckBox: TCheckBox;
    YMinEdit: TEdit;
    YMaxEdit: TEdit;
    YStepEdit: TEdit;
    PointGroupBox: TGroupBox;
    PointComboBox: TComboBox;
    PointColorBtn: TButton;
    PointColorShape: TShape;
    Label14: TLabel;
    PointSizeSpinEdit: TSpinEdit;
    LineGroupBox: TGroupBox;
    LineComboBox: TComboBox;
    LineColorBtn: TButton;
    LineColorShape: TShape;
    Label15: TLabel;
    LineWidthSpinEdit: TSpinEdit;
    StepGroupBox: TGroupBox;
    Label12: TLabel;
    StepSpinEdit: TSpinEdit;
    TitleGroupBox: TGroupBox;
    Label13: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    GraphTitleEdit: TEdit;
    XTitleEdit: TEdit;
    YTitleEdit: TEdit;
    FontGroupBox: TGroupBox;
    TitleFontBtn: TButton;
    AxesfontBtn: TButton;
    LgdFontBtn: TButton;
    ConfigGroupBox: TGroupBox;
    ConfigReadBtn: TButton;
    ConfigSaveBtn: TButton;
    CurvGroupBox: TGroupBox;
    ApplyBtn1: TButton;
    XMinLogSpinEdit: TSpinEdit;
    XMaxLogSpinEdit: TSpinEdit;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    YMinLogSpinEdit: TSpinEdit;
    Label22: TLabel;
    YMaxLogSpinEdit: TSpinEdit;
    AxisGroupBox: TGroupBox;
    AxisWidthSpinEdit: TSpinEdit;
    GroupBox3: TGroupBox;
    CurvParamComboBox: TComboBox;
    ApplyBtn2: TButton;
    AxisColorDialog: TColorDialog;
    ForceOriginGroupBox: TGroupBox;
    ForceOriginCheckBox: TCheckBox;
    AxisColorBtn: TButton;
    AxisColorShape: TShape;
    Label11: TLabel;
    Label18: TLabel;

    procedure BorderColorBtnClick(Sender: TObject);
    procedure GraphColorBtnClick(Sender: TObject);
    procedure XScaleCheckBoxClick(Sender: TObject);
    procedure YScaleCheckBoxClick(Sender: TObject);
    procedure ForceOriginCheckBoxClick(Sender: TObject);
    procedure TitleFontBtnClick(Sender: TObject);
    procedure AxesfontBtnClick(Sender: TObject);
    procedure LgdFontBtnClick(Sender: TObject);
    procedure ConfigReadBtnClick(Sender: TObject);
    procedure ConfigSaveBtnClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure CurvParamComboBoxChange(Sender: TObject);
    procedure PointColorBtnClick(Sender: TObject);
    procedure LineColorBtnClick(Sender: TObject);
    procedure ApplyBtn1Click(Sender: TObject);
    procedure ApplyBtn2Click(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure AxisColorBtnClick(Sender: TObject);

  private
    procedure UpdateForm;
    procedure SetCurves(I1, I2 : Integer) ;

  public
    procedure ReadConfigFile(const FileName : string);
    procedure WriteConfigFile(const FileName : string);

  end;

var
  GraphOptDlg: TGraphOptDlg;

implementation

{$R graphopt.lfm}

uses
  utypes, umath, uwinplot, uwinstr;

var
  Xwin1       : Integer = 15;
  Ywin1       : Integer = 15;
  Xwin2       : Integer = 85;
  Ywin2       : Integer = 80;
  BackColor   : LongInt = $FFFFFF;
  GraphColor  : LongInt = $FFFFFF;
  Plot_Border : Boolean = True;
  Plot_Lgd    : Boolean = True;
  Grid        : TGrid   = BothGrid;
  Plot_Ox     : Boolean = True;
  Plot_Oy     : Boolean = True;
  AxisWidth   : Integer = 1;
  AxisColor   : LongInt = $000000;
  ForceOrigin : Boolean = False;

procedure TGraphOptDlg.UpdateForm;
{ Update form controls }
var
  Scale     : TScale;
  A, B, C   : Float;
  I         : Integer;
  Symbol    : Integer;
  Size      : Integer;
  Color     : TColor;
  Style     : TPenStyle;
  Width     : Integer;
  MultiCurv : Boolean;

begin
  { Graph limits }
  XminSpinEdit.Value := Xwin1;
  YminSpinEdit.Value := Ywin1;
  XmaxSpinEdit.Value := Xwin2;
  YmaxSpinEdit.Value := Ywin2;

  { Aspect / Colors }
  GraphBorderCheckBox.Checked := Plot_Border;
  LgdBorderCheckBox.Visible := (GetMaxCurv > 1);
  LgdBorderCheckBox.Checked := Plot_Lgd;
  BorderColorShape.Brush.Color := BackColor;
  GraphColorShape.Brush.Color := GraphColor;

  { Grid }
  GridRadioGroup.ItemIndex := Ord(Grid);

  { Axes }
  XPlotCheckBox.Checked := Plot_Ox;
  YPlotCheckBox.Checked := Plot_Oy;
  AxisWidthSpinEdit.Value := AxisWidth;
  AxisColorShape.Brush.Color := AxisColor;
  ForceOriginCheckBox.Checked := ForceOrigin;
  
  GetOxScale(Scale, A, B, C);
  if ForceOrigin then A := 0.0;

  if Scale = LinScale then
    begin
      XScaleCheckBox.Checked := False;
      XMinEdit.Text := FloatToStr(A);
      XMaxEdit.Text := FloatToStr(B);
      XStepEdit.Text := FloatToStr(C);
    end
  else
    begin
      XScaleCheckBox.Checked := True;
      XMinLogSpinEdit.Value := Round(A);
      XMaxLogSpinEdit.Value := Round(B);
    end;

  GetOyScale(Scale, A, B, C);
  if ForceOrigin then A := 0.0;

  if Scale = LinScale then
    begin
      YScaleCheckBox.Checked := False;
      YMinEdit.Text := FloatToStr(A);
      YMaxEdit.Text := FloatToStr(B);
      YStepEdit.Text := FloatToStr(C);
    end
  else
    begin
      YScaleCheckBox.Checked := True;
      YMinLogSpinEdit.Value := Round(A);
      YMaxLogSpinEdit.Value := Round(B);
    end;

  Label7.Visible := not XScaleCheckBox.Checked;
  Label10.Visible := not YScaleCheckBox.Checked;
  XStepEdit.Visible := Label7.Visible;
  YStepEdit.Visible := Label10.Visible;

  Label19.Visible := XScaleCheckBox.Checked;
  Label20.Visible := Label19.Visible;
  XMinLogSpinEdit.Visible := Label19.Visible;
  XMaxLogSpinEdit.Visible := Label19.Visible;

  Label21.Visible := YScaleCheckBox.Checked;
  Label22.Visible := Label21.Visible;
  YMinLogSpinEdit.Visible := Label21.Visible;
  YMaxLogSpinEdit.Visible := Label21.Visible;

  AxisWidthSpinEdit.Value := AxisWidth;
  AxisColorShape.Brush.Color := AxisColor;
  ForceOriginCheckBox.Checked := ForceOrigin;

  { Curves }

  MultiCurv := (GetMaxCurv > 1);
  GroupBox3.Visible := MultiCurv;
  LgdFontBtn.Visible := MultiCurv;
  CurvGroupBox.Visible := MultiCurv;
  Label18.Visible := MultiCurv;

  if MultiCurv then
    begin
      StepGroupBox.Left := 152;
      StepGroupBox.Width := 129;
    end
  else
    begin
      StepGroupBox.Left := 8;
      StepGroupBox.Width := 273;
    end;

  with CurvParamComboBox do
    begin
      Clear;
      for I := 1 to GetMaxCurv do
        Items.Add(GetCurvLegend(I));
      ItemIndex := 0;
      Text := Items[ItemIndex];
    end;

  I := Succ(CurvParamComboBox.ItemIndex);

  GetPointParam(I, Symbol, Size, Color);
  PointComboBox.ItemIndex := Symbol;
  PointSizeSpinEdit.Value := Size;
  PointColorShape.Brush.Color := Color;

  GetLineParam(I, Style, Width, Color);
  LineComboBox.ItemIndex := Ord(Style);
  LineWidthSpinEdit.Value := Width;
  LineColorShape.Brush.Color := Color;

  StepSpinEdit.Value := GetCurvStep(I);

  { Titles }
  GraphTitleEdit.Text := GetGraphTitle;
  XTitleEdit.Text := GetOxTitle;
  YTitleEdit.Text := GetOyTitle;
end;

procedure TGraphOptDlg.ReadConfigFile(const FileName : string);
{ Reads the configuration file }
var
  F            : TextFile;
  Font         : TFont;
  Name         : TFontName;
  I, NCurv, N  : Integer;
  Symbol, Size : Integer;
  Step         : Integer;
  Style, Width : Integer;
  Color        : TColor;

  function ReadBoolean : Boolean;
  begin
    Readln(F, N);
    ReadBoolean := Boolean(N);
  end;

  function ReadColor : TColor;
  var
    S       : String;
    ErrCode : Integer;
  begin
    Readln(F, S);
    S[1] := '$';
    Val(S, Color, ErrCode);
    if ErrCode = 0 then
      ReadColor := Color
    else
      ReadColor := 0;
  end;

  procedure ReadFont;
  begin
    Readln(F, Name);
    Readln(F, Size);

    Font.Name := Name;
    Font.Size := Size;
    Font.Color := ReadColor;

    Font.Style := [];
    if ReadBoolean then Font.Style := Font.Style + [fsBold];
    if ReadBoolean then Font.Style := Font.Style + [fsItalic];
    if ReadBoolean then Font.Style := Font.Style + [fsUnderline];
    if ReadBoolean then Font.Style := Font.Style + [fsStrikeOut];
  end;

begin
  if not FileExists(FileName) then Exit;
  AssignFile(F, Filename);
  Reset(F);

  { Graph limits }
  Readln(F, Xwin1);
  Readln(F, Ywin1);
  Readln(F, Xwin2);
  Readln(F, Ywin2);

  { Aspect / Colors }
  Plot_Border := ReadBoolean;
  Plot_Lgd := ReadBoolean;
  BackColor := ReadColor;
  GraphColor := ReadColor;

  { Grid }
  Readln(F, N);
  Grid := TGrid(N);

  { Axes }
  Plot_Ox := ReadBoolean;
  Plot_Oy := ReadBoolean;
  Readln(F, AxisWidth);
  AxisColor := ReadColor;
  ForceOrigin := ReadBoolean;

  { Fonts }
  Font := TFont.Create;
  ReadFont; TitleFontDialog.Font := Font;
  ReadFont; AxesFontDialog.Font := Font;
  ReadFont; LgdFontDialog.Font := Font;

  { Curves - Don't read more curves than defined by the main program }
  Readln(F, NCurv);
  if NCurv > GetMaxCurv then NCurv := GetMaxCurv;

  for I := 1 to NCurv do
    begin
      Readln(F, Symbol);
      Readln(F, Size);
      Color := ReadColor;
      SetPointParam(I, Symbol, Size, Color);

      Readln(F, Step);
      SetCurvStep(I, Step);

      Readln(F, Style);
      Readln(F, Width);
      Color := ReadColor;
      SetLineParam(I, TPenStyle(Style), Width, Color);
    end;

  CloseFile(F);

  UpdateForm;
end;

procedure TGraphOptDlg.WriteConfigFile(const FileName : string);
{ Writes the configuration file }
var
  F      : TextFile;
  I      : Integer;
  NCurv  : Integer;
  Symbol : Integer;
  Size   : Integer;
  Color  : TColor;
  Style  : TPenStyle;
  Width  : Integer;

  procedure SaveBoolean(Condition : Boolean);
  begin
    Writeln(F, Ord(Condition));
  end;

  procedure SaveColor(Color : TColor);
  begin
    Writeln(F, '#', IntToHex(Color, 6));
  end;

  procedure SaveFont(Font : TFont);
  begin
    Writeln(F, Font.Name);
    Writeln(F, Font.Size);
    SaveColor(Font.Color);
    SaveBoolean(fsBold in Font.Style);
    SaveBoolean(fsItalic in Font.Style);
    SaveBoolean(fsUnderline in Font.Style);
    SaveBoolean(fsStrikeOut in Font.Style);
  end;

begin
  AssignFile(F, FileName);
  Rewrite(F);

  { Graph limits }
  Writeln(F, XminSpinEdit.Value);
  Writeln(F, YminSpinEdit.Value);
  Writeln(F, XmaxSpinEdit.Value);
  Writeln(F, YmaxSpinEdit.Value);

  { Aspect / Colors }
  SaveBoolean(GraphBorderCheckBox.Checked);
  SaveBoolean(LgdBorderCheckBox.Checked);
  SaveColor(BorderColorShape.Brush.Color);
  SaveColor(GraphColorShape.Brush.Color);

  { Grid }
  Writeln(F, GridRadioGroup.ItemIndex);

  { Axes }
  SaveBoolean(XPlotCheckBox.Checked);
  SaveBoolean(YPlotCheckBox.Checked);
  Writeln(F, AxisWidthSpinEdit.Value);
  SaveColor(AxisColorShape.Brush.Color);
  SaveBoolean(ForceOriginCheckBox.Checked);

  { Fonts }
  SaveFont(TitleFontDialog.Font);
  SaveFont(AxesFontDialog.Font);
  SaveFont(LgdFontDialog.Font);

  { Curves }
  NCurv := GetMaxCurv;
  Writeln(F, NCurv);
  for I := 1 to NCurv do
    begin
      GetPointParam(I, Symbol, Size, Color);
      Writeln(F, Symbol);
      Writeln(F, Size);
      SaveColor(Color);
      Writeln(F, GetCurvStep(I));
      GetLineParam(I, Style, Width, Color);
      Writeln(F, Ord(Style));
      Writeln(F, Width);
      SaveColor(Color);
    end;

  CloseFile(F);
end;

procedure TGraphOptDlg.FormActivate(Sender: TObject);
begin
  UpdateForm;
end;

procedure TGraphOptDlg.BorderColorBtnClick(Sender: TObject);
begin
  if BorderColorDialog.Execute then
    BorderColorShape.Brush.Color := BorderColorDialog.Color;
end;

procedure TGraphOptDlg.GraphColorBtnClick(Sender: TObject);
begin
  if GraphColorDialog.Execute then
    GraphColorShape.Brush.Color := GraphColorDialog.Color;
end;

procedure TGraphOptDlg.AxisColorBtnClick(Sender: TObject);
begin
  if AxisColorDialog.Execute then
    AxisColorShape.Brush.Color := AxisColorDialog.Color;
end;

procedure TGraphOptDlg.XScaleCheckBoxClick(Sender: TObject);
begin
  Label7.Visible := not XScaleCheckBox.Checked;

  XMinEdit.Visible := Label7.Visible;
  XMaxEdit.Visible := Label7.Visible;
  XStepEdit.Visible := Label7.Visible;

  Label19.Visible := XScaleCheckBox.Checked;
  Label20.Visible := Label19.Visible;
  XMinLogSpinEdit.Visible := Label19.Visible;
  XMaxLogSpinEdit.Visible := Label19.Visible;

  ForceOriginCheckBox.Enabled :=
    (not XScaleCheckBox.Checked) and (not YScaleCheckBox.Checked);
end;

procedure TGraphOptDlg.YScaleCheckBoxClick(Sender: TObject);
begin
  Label10.Visible := not YScaleCheckBox.Checked;

  YMinEdit.Visible := Label10.Visible;
  YMaxEdit.Visible := Label10.Visible;
  YStepEdit.Visible := Label10.Visible;

  Label21.Visible := YScaleCheckBox.Checked;
  Label22.Visible := Label21.Visible;
  YMinLogSpinEdit.Visible := Label21.Visible;
  YMaxLogSpinEdit.Visible := Label21.Visible;

  ForceOriginCheckBox.Enabled :=
    (not XScaleCheckBox.Checked) and (not YScaleCheckBox.Checked);
end;

procedure TGraphOptDlg.ForceOriginCheckBoxClick(Sender: TObject);
begin
  ForceOrigin := ForceOriginCheckBox.Checked;
  XMinEdit.Enabled := not ForceOrigin;
  YMinEdit.Enabled := XMinEdit.Enabled;
end;

procedure TGraphOptDlg.CurvParamComboBoxChange(Sender: TObject);
var
  I      : Integer;
  Symbol : Integer;
  Size   : Integer;
  Color  : TColor;
  Style  : TPenStyle;
  Width  : Integer;

begin
  I := Succ(CurvParamComboBox.ItemIndex);

  GetPointParam(I, Symbol, Size, Color);
  PointComboBox.ItemIndex := Symbol;
  PointSizeSpinEdit.Value := Size;
  PointColorShape.Brush.Color := Color;

  GetLineParam(I, Style, Width, Color);
  LineComboBox.ItemIndex := Ord(Style);
  LineWidthSpinEdit.Value := Width;
  LineColorShape.Brush.Color := Color;

  StepSpinEdit.Value := GetCurvStep(I);
end;

procedure TGraphOptDlg.PointColorBtnClick(Sender: TObject);
begin
  if PointColorDialog.Execute then
    PointColorShape.Brush.Color := PointColorDialog.Color;
end;

procedure TGraphOptDlg.LineColorBtnClick(Sender: TObject);
begin
  if LineColorDialog.Execute then
    LineColorShape.Brush.Color := LineColorDialog.Color;
end;

procedure TGraphOptDlg.SetCurves(I1, I2 : Integer);
{ Applies the settings to curve # I1 to I2 }
var
  I                         : Integer;
  Symbol, Size, Step, Width : Integer;
  PointColor, LineColor     : TColor;
  Style                     : TPenStyle;
begin
  Symbol := PointComboBox.ItemIndex;
  Size := PointSizeSpinEdit.Value;
  Step := StepSpinEdit.Value;
  Width := LineWidthSpinEdit.Value;
  PointColor := PointColorShape.Brush.Color;
  LineColor := LineColorShape.Brush.Color;
  Style := TPenStyle(LineComboBox.ItemIndex);

  for I := I1 to I2 do
    begin
      SetPointParam(I, Symbol, Size, PointColor);
      SetLineParam(I, Style, Width, LineColor);
      SetCurvStep(I, Step);
    end;
end;

procedure TGraphOptDlg.ApplyBtn1Click(Sender: TObject);
var
  I : Integer;
begin
  I := CurvParamComboBox.ItemIndex + 1;
  SetCurves(I, I);
end;

procedure TGraphOptDlg.ApplyBtn2Click(Sender: TObject);
begin
  SetCurves(1, GetMaxCurv);
end;

procedure TGraphOptDlg.TitleFontBtnClick(Sender: TObject);
begin
  TitleFontDialog.Execute;
end;

procedure TGraphOptDlg.AxesfontBtnClick(Sender: TObject);
begin
  AxesfontDialog.Execute;
end;

procedure TGraphOptDlg.LgdFontBtnClick(Sender: TObject);
begin
  LgdFontDialog.Execute;
end;

procedure TGraphOptDlg.ConfigReadBtnClick(Sender: TObject);
begin
  if ConfigReadDialog.Execute then
    ReadConfigFile(ConfigReadDialog.FileName);
end;

procedure TGraphOptDlg.ConfigSaveBtnClick(Sender: TObject);
begin
  if ConfigSaveDialog.Execute then
    WriteConfigFile(ConfigSaveDialog.FileName);
end;

procedure TGraphOptDlg.OKBtnClick(Sender: TObject);
var
  A, B, C : Float;
begin
  { Graph limits }
  Xwin1 := XminSpinEdit.Value;
  Ywin1 := YminSpinEdit.Value;
  Xwin2 := XmaxSpinEdit.Value;
  Ywin2 := YmaxSpinEdit.Value;

  { Aspect / Colors }
  Plot_Border := GraphBorderCheckBox.Checked;
  Plot_Lgd := LgdBorderCheckBox.Checked;
  BackColor := BorderColorShape.Brush.Color;
  GraphColor := GraphColorShape.Brush.Color;

  { Grid }
  Grid := TGrid(GridRadioGroup.ItemIndex);

  { Axes }
  Plot_Ox := XPlotCheckBox.Checked;
  Plot_Oy := YPlotCheckBox.Checked;
  ForceOrigin := ForceOriginCheckBox.Checked;
  AxisWidth := AxisWidthSpinEdit.Value;
  AxisColor := AxisColorShape.Brush.Color;

  { Ox axis }
  if XScaleCheckBox.Checked then
    begin
      A := IntPower(10.0, XMinLogSpinEdit.Value);
      B := IntPower(10.0, XMaxLogSpinEdit.Value);
      if A < B then
        SetOxScale(LogScale, A, B, 10.0);
    end
  else
    begin
      if ForceOrigin then
        A := 0.0
      else
        A := ReadNumFromEdit(XMinEdit);
      B := ReadNumFromEdit(XMaxEdit);
      C := ReadNumFromEdit(XStepEdit);
      if (A < B) and (C > 0.0) and (C < B - A) then
        SetOxScale(LinScale, A, B, C);
    end;

  { Oy axis }
  if YScaleCheckBox.Checked then
    begin
      A := IntPower(10.0, YMinLogSpinEdit.Value);
      B := IntPower(10.0, YMaxLogSpinEdit.Value);
      if A < B then
        SetOyScale(LogScale, A, B, 10.0);
    end
  else
    begin
      if ForceOrigin then
        A := 0.0
      else
        A := ReadNumFromEdit(YMinEdit);
      B := ReadNumFromEdit(YMaxEdit);
      C := ReadNumFromEdit(YStepEdit);
      if (A < B) and (C > 0.0) and (C < B - A) then
        SetOyScale(LinScale, A, B, C);
    end;

  { Curves }
  if GetMaxCurv = 1 then SetCurves(1, 1);

  { Titles }
  SetGraphTitle(GraphTitleEdit.Text);
  SetOxTitle(XTitleEdit.Text);
  SetOyTitle(YTitleEdit.Text);
end;

end.
