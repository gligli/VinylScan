unit Algorith;

{$MODE Delphi}

interface

uses
  SysUtils, LCLIntf, LCLType, LMessages, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, Spin, ComCtrls,
  utypes;

type

  { TAlgorithmDlg }

  TAlgorithmDlg = class(TForm)
    TabbedNotebook1: TPageControl;

    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;

    RadioGroup0: TRadioGroup;
    RadioGroup1: TRadioGroup;

    GroupBox0: TGroupBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;

    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;

    SaveDialog1: TSaveDialog;

    Label0: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;

    SpinEdit0: TSpinEdit;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    SpinEdit4: TSpinEdit;
    SpinEdit5: TSpinEdit;
    SpinEdit6: TSpinEdit;
    SpinEdit7: TSpinEdit;
    SpinEdit8: TSpinEdit;
    SpinEdit9: TSpinEdit;
    SpinEdit10: TSpinEdit;
    SpinEdit11: TSpinEdit;
    SpinEdit12: TSpinEdit;
    SpinEdit13: TSpinEdit;
    SpinEdit14: TSpinEdit;

    Button1: TButton;
    ts1: TTabSheet;
    ts2: TTabSheet;

    procedure RadioGroup0Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);

    function GetMaxIter   : Integer;
    function GetTol       : Float;
    function GetSVDTol    : Float;
    function GetMCMCFile  : String;
    function GetEstParam  : Boolean;
    function GetShowParam : Boolean;
  end;

var
  AlgorithmDlg: TAlgorithmDlg;

implementation
{$R algorith.lfm}
uses
  umath, umarq, ubfgs, usimplex,
  usimann, ugenalg, umcmc, unlfit, Help;



const
  HelpLines = 8;

  HelpText : array[1..HelpLines] of String[68] = (
  '1. Select the optimization algorithms and their options.',
  '',
  '2. If two algorithms are applied consecutively, the option',
  '   "Initial parameters / Estimate" must be unchecked before ',
  '   applying the second algorithm.',
  '',
  '3. The option "Initial parameters / Show" must be checked if you',
  '   wish to modify the initial parameters values and their bounds.');

var
  MCMCFile : String;

procedure TAlgorithmDlg.RadioGroup0Click(Sender: TObject);
begin
  GroupBox0.Visible := (RadioGroup0.ItemIndex = 1);
end;

procedure TAlgorithmDlg.RadioGroup1Click(Sender: TObject);
var
  Index : Integer;
begin
  Index := RadioGroup1.ItemIndex;
  GroupBox1.Visible := (Index in [0..2]);  { Marquardt / BFGS / Simplex }
  GroupBox2.Visible := GroupBox1.Visible;
  GroupBox3.Visible := (Index = 3);        { Simulated annealing }
  GroupBox4.Visible := (Index = 4);        { Genetic algorithm }
  GroupBox5.Visible := (Index = 5);        { Metropolis-Hastings }
end;

procedure TAlgorithmDlg.Button1Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
    MCMCFile := SaveDialog1.FileName;
end;

procedure TAlgorithmDlg.OKBtnClick(Sender: TObject);
begin
  { Nonlinear regression }
  case RadioGroup1.ItemIndex of
    3 : InitSAParams(SpinEdit4.Value,
                     SpinEdit5.Value,
                     SpinEdit3.Value,
                     SpinEdit6.Value * 0.01);
    4 : InitGAParams(SpinEdit7.Value,
                     SpinEdit8.Value,
                     SpinEdit9.Value * 0.01,
                     SpinEdit10.Value * 0.01,
                     SpinEdit11.Value * 0.01);
    5 : InitMHParams(SpinEdit12.Value,
                     SpinEdit13.Value,
                     SpinEdit14.Value);
  end;

  if RadioGroup1.ItemIndex in [0..4] then
    SetOptAlgo(TOptAlgo(RadioGroup1.ItemIndex));

  if CheckBox3.Checked then
    case RadioGroup1.ItemIndex of
      0 : SaveMarquardt('marquard.txt');
      1 : SaveBFGS('bfgs.txt');
      2 : SaveSimplex('simplex.txt');
    end;

  if CheckBox4.Checked then
    SA_CreateLogFile('simann.txt');

  if CheckBox5.Checked then
    SA_CreateLogFile('genalg.txt');
end;

procedure TAlgorithmDlg.HelpBtnClick(Sender: TObject);
var
  I : Integer;
begin
  HelpDlg.Caption := 'Algorithms Help';
  HelpDlg.Memo1.Lines.Clear;
  for I := 1 to HelpLines do
    HelpDlg.Memo1.Lines.Add(HelpText[I]);
  HelpDlg.ShowModal;
end;

function TAlgorithmDlg.GetMaxIter : Integer;
begin
  GetMaxIter := SpinEdit1.Value;
end;

function TAlgorithmDlg.GetTol : Float;
begin
  GetTol := Exp10(SpinEdit2.Value);
end;

function TAlgorithmDlg.GetSVDTol : Float;
begin
  GetSVDTol := Exp10(SpinEdit0.Value);
end;

function TAlgorithmDlg.GetMCMCFile : String;
begin
  GetMCMCFile := MCMCFile;
end;

function TAlgorithmDlg.GetEstParam : Boolean;
begin
  GetEstParam := CheckBox1.Checked;
end;

function TAlgorithmDlg.GetShowParam : Boolean;
begin
  if RadioGroup1.ItemIndex in [3..5] then  { SA / GA / MCMC }
    GetShowParam := True
  else
    GetShowParam := CheckBox2.Checked;
end;

end.
