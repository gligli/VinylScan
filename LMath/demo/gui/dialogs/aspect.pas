unit Aspect;

{$MODE Delphi}

interface

uses LCLIntf, LCLType, LMessages, Classes, Graphics, Forms, Controls, Buttons,
  StdCtrls, ExtCtrls, Spin;

type
  TVarOx  = (Ox_X, Ox_Yobs, Ox_Ycalc);
  TVarOy  = (Oy_Yobs, Oy_Ycalc, Oy_Res, Oy_NormRes);
  TAspect = (Pts_Only, Curve_Obs, Curve_Calc, Pts_Curve_Obs, Pts_Curve_Calc);

type
  TAspectDlg = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    RadioGroup1: TRadioGroup;
    RadioGroupOx: TRadioGroup;
    RadioGroupOy: TRadioGroup;
    GroupBox1: TGroupBox;
    CheckBox1: TCheckBox;
    SpinEdit2: TSpinEdit;
    Label2: TLabel;
    Label1: TLabel;
    SpinEdit1: TSpinEdit;
    procedure RadioGroup1Click(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure SetVarOx(Var_Ox : TVarOx);
    procedure SetVarOy(Var_Oy : TVarOy);
    function GetVarOx : TVarOx;
    function GetVarOy : TVarOy;
    function GetCurvAspect : TAspect;
    function GetNptGraph : Integer;
    function GetNSD : Integer;
    function GetShowErrorBars : Boolean;
    function GetVarChange : Boolean;
  end;

var
  AspectDlg : TAspectDlg;

implementation

{$R aspect.lfm}

uses
  SysUtils, utypes, uwinstr, Help;

const
  HelpLines = 10;

  HelpText : array[1..HelpLines] of String[68] = (
  '1. Select the variables to be plotted on the axes.',
  '',
  '2. Select the plotting style for the curves.',
  '',
  '3. Enter the number of points to be computed for plotting the',
  '   fitted curves.',
  '',
  '4. Indicate if an error bar must be displayed for each point,',
  '   by means of the standard deviations estimated by the program',
  '   from the selected error model.');

var
  VarOx     : TVarOx  = Ox_X;     { Variable to be plotted on Ox }
  VarOy     : TVarOy  = Oy_Yobs;  { Variable to be plotted on Oy }
  VarChange : Boolean = False;    { Change in VarOx or VarOy }

procedure TAspectDlg.RadioGroup1Click(Sender: TObject);
begin
  Label1.Visible := (RadioGroup1.ItemIndex in [2,4]);
  SpinEdit1.Visible := Label1.Visible;
end;

procedure TAspectDlg.OKBtnClick(Sender: TObject);
var
  VarOx1 : TVarOx;
  VarOy1 : TVarOy;
begin
  VarOx1 := TVarOx(RadioGroupOx.ItemIndex);
  VarOy1 := TVarOy(RadioGroupOy.ItemIndex);

  VarChange := (VarOx1 <> VarOx) or (VarOy1 <> VarOy);

  VarOx := VarOx1;
  VarOy := VarOy1;
end;

procedure TAspectDlg.HelpBtnClick(Sender: TObject);
var
  I : Integer;
begin
  HelpDlg.Caption := 'Axes and Curves Help';
  HelpDlg.Memo1.Lines.Clear;
  for I := 1 to HelpLines do
    HelpDlg.Memo1.Lines.Add(HelpText[I]);
  HelpDlg.ShowModal;
end;

procedure TAspectDlg.SetVarOx(Var_Ox : TVarOx);
begin
  VarOx := Var_Ox;
end;

procedure TAspectDlg.SetVarOy(Var_Oy : TVarOy);
begin
  VarOy := Var_Oy;
end;

function TAspectDlg.GetVarOx : TVarOx;
begin
  GetVarOx := TVarOx(RadioGroupOx.ItemIndex);
end;

function TAspectDlg.GetVarOy : TVarOy;
begin
  GetVarOy := TVarOy(RadioGroupOy.ItemIndex);
end;

function TAspectDlg.GetCurvAspect : TAspect;
begin
  GetCurvAspect := TAspect(RadioGroup1.ItemIndex);
end;

function TAspectDlg.GetNptGraph : Integer;
begin
  GetNptGraph := SpinEdit1.Value;
end;

function TAspectDlg.GetShowErrorBars : Boolean;
begin
  GetShowErrorBars := CheckBox1.Checked;
end;

function TAspectDlg.GetNSD : Integer;
begin
  GetNSD := SpinEdit2.Value;
end;

function TAspectDlg.GetVarChange : Boolean;
begin
  GetVarChange := VarChange;
  VarChange := False;
end;

end.
