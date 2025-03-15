unit ErrFunc;
{ Choix de la fonction d'erreur }

interface

uses WinTypes, WinProcs, Classes, Graphics, Forms, Controls, Buttons,
  StdCtrls, ExtCtrls, Spin,
  { DMath units }
  utypes, uerrmod;

type
  TErrFuncDlg = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    RadioGroup1: TRadioGroup;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    GroupBoxVar: TGroupBox;
    LabelVar1: TLabel;
    EditVar1: TEdit;
    LabelVar2: TLabel;
    EditVar2: TEdit;
    LabelVar3: TLabel;
    EditVar3: TEdit;
    CheckBox1: TCheckBox;
    Label2: TLabel;
    SpinEdit1: TSpinEdit;
    RadioGroup2: TRadioGroup;
    procedure RadioGroup1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    function GetModel: TErrModel;
    function GetParam: TVector;
    function GetErrInY : Boolean;
    procedure HelpBtnClick(Sender: TObject);
  end;

var
  ErrFuncDlg : TErrFuncDlg;

implementation

{$R *.DFM}

uses
  uWinStr, Help;

const
  HelpLines = 9;

  HelpText : array[1..HelpLines] of String[68] =
  ('1. Select the function which computes the standard deviation',
   '   of an observation y. The relevant formula is displayed in blue.',
   '',
   '2. Indicate if the standard deviation is expressed as a function',
   '   of the independent (x) or dependent (y) variable.',
   '',
   '3. Enter the function parameters e1, e2 ...',
   '   The parameter e0 (residual standard deviation)',
   '   is estimated by the program.');

var
  ErrModel : TErrModel;
  Theta    : TVector;
  ErrInY   : Boolean;

procedure TErrFuncDlg.RadioGroup1Click(Sender: TObject);
{ Choix du modèle de variance }
begin
  { Initialiser le modèle de variance
    et afficher la formule correspondante }
  case RadioGroup1.ItemIndex of
    0 : ErrModel := ERR_CONST;
    1 : ErrModel := ERR_LIN;
    2 : ErrModel := ERR_POL2;
    3 : ErrModel := ERR_POL3;
    4 : ErrModel := ERR_EXPO;
    5 : ErrModel := ERR_POWER;
  end;

  ErrInY := (RadioGroup2.ItemIndex = 1);
  Label1.Caption := ErrFuncName(ErrModel, ErrInY);

  { Afficher les options }
  RadioGroup2.Visible := (ErrModel <> ERR_CONST);
  GroupBoxVar.Visible := (ErrModel <> ERR_CONST);
  LabelVar2.Visible := (ErrModel in [ERR_POL2, ERR_POL3]);
  LabelVar3.Visible := (ErrModel = ERR_POL3);
  EditVar2.Visible := LabelVar2.Visible;
  EditVar3.Visible := LabelVar3.Visible;
end;

procedure TErrFuncDlg.RadioGroup2Click(Sender: TObject);
{ Choix de la ponderation en f(X) ou f(Y) }
begin
  ErrInY := (RadioGroup2.ItemIndex = 1);
  Label1.Caption := ErrFuncName(ErrModel, ErrInY);
end;

procedure TErrFuncDlg.OKBtnClick(Sender: TObject);
var
  S : string;
  X : Float;
begin
  S := EditVar1.Text;
  if IsNumeric(S, X) then Theta[1] := X;

  S := EditVar2.Text;
  if IsNumeric(S, X) then Theta[2] := X;

  S := EditVar3.Text;
  if IsNumeric(S, X) then Theta[3] := X;
end;

procedure TErrFuncDlg.HelpBtnClick(Sender: TObject);
var
  I : Integer;
begin
  HelpDlg.Caption := 'Error model Help';
  HelpDlg.Memo1.Lines.Clear;
  for I := 1 to HelpLines do
    HelpDlg.Memo1.Lines.Add(HelpText[I]);
  HelpDlg.ShowModal;
end;

function TErrFuncDlg.GetModel: TErrModel;
begin
  GetModel := ErrModel;
end;

function TErrFuncDlg.GetParam: TVector;
begin
  GetParam := Theta;
end;

function TErrFuncDlg.GetErrInY : Boolean;
begin
  GetErrInY := ErrInY;
end;

begin
  DimVector(Theta, 3);
end.

