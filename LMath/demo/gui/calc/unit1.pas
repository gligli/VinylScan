unit Unit1;

{$MODE Delphi}

interface

uses
  SysUtils, LCLIntf, LCLType, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    LabelA: TLabel;
    LabelB: TLabel;
    LabelC: TLabel;
    LabelD: TLabel;
    LabelVar: TLabel;
    LabelFormula: TLabel;
    LabelResult: TLabel;
    LabelNum: TLabel;
    LabelOp: TLabel;
    LabelFunc: TLabel;
    LabelBksp: TLabel;
    EditA: TEdit;
    EditB: TEdit;
    EditC: TEdit;
    EditD: TEdit;
    EditFormula: TEdit;
    EditResult: TEdit;
    Btn0: TButton;
    Btn1: TButton;
    Btn2: TButton;
    Btn3: TButton;
    Btn4: TButton;
    Btn5: TButton;
    Btn6: TButton;
    Btn7: TButton;
    Btn8: TButton;
    Btn9: TButton;
    BtnDec: TButton;
    BtnPar1: TButton;
    BtnPar2: TButton;
    BtnBksp: TButton;
    BtnAdd: TButton;
    BtnSub: TButton;
    BtnMul: TButton;
    BtnDiv: TButton;
    BtnIntDiv: TButton;
    BtnMod: TButton;
    BtnPow: TButton;
    BtnNot: TButton;
    BtnAnd: TButton;
    BtnOr: TButton;
    BtnXor: TButton;
    BtnEqv: TButton;
    BtnImp: TButton;
    BtnLeft: TButton;
    BtnRight: TButton;
    BtnAbs: TButton;
    BtnSgn: TButton;
    BtnInt: TButton;
    BtnSqrt: TButton;
    BtnExp: TButton;
    BtnLn: TButton;
    BtnLog10: TButton;
    BtnLog2: TButton;
    BtnSin: TButton;
    BtnCos: TButton;
    BtnTan: TButton;
    BtnArcSin: TButton;
    BtnArcCos: TButton;
    BtnArcTan: TButton;
    BtnArcTan2: TButton;
    BtnSinh: TButton;
    BtnCosh: TButton;
    BtnTanh: TButton;
    BtnArcSinh: TButton;
    BtnArcCosh: TButton;
    BtnArcTanh: TButton;
    BtnDeg: TButton;
    BtnRad: TButton;
    BtnRnd: TButton;
    BtnFact: TButton;
    BtnGamma: TButton;
    BtnErf: TButton;
    BtnIGamma: TButton;
    BtnBeta: TButton;
    BtnIBeta: TButton;
    BtnLambert: TButton;
    BtnBinom: TButton;
    BtnEval: TButton;
    BtnClear: TButton;

    procedure FormCreate(Sender: TObject);
    procedure ButtonClick(Sender: TObject);
    procedure BtnBkspClick(Sender: TObject);
    procedure BtnEvalClick(Sender: TObject);
    procedure BtnClearClick(Sender: TObject);
    procedure EditFormulaKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  utypes, uErrors, ueval;

procedure TForm1.FormCreate(Sender: TObject);
begin
  InitEval;
end;

procedure TForm1.ButtonClick(Sender: TObject);
var
  I, K, L    : Integer;
  S, Formula : string;
begin
  I := (Sender as TButton).TabOrder;

  case I of
    8 : S := '7';
    9 : S := '8';
   10 : S := '9';
   11 : S := '4';
   12 : S := '5';
   13 : S := '6';
   14 : S := '1';
   15 : S := '2';
   16 : S := '3';
   17 : S := '0';
   18 : S := '.';
   19 : S := '(';
   20 : S := ')';
   22 : S := '+';
   23 : S := '-';
   24 : S := '*';
   25 : S := '/';
   26 : S := '\';
   27 : S := '%';
   28 : S := '^';
   29 : S := '!';
   30 : S := '&';
   31 : S := '|';
   32 : S := '$';
   33 : S := '=';
   34 : S := '@';
   35 : S := '<';
   36 : S := '>';
   37 : S := 'abs()';
   38 : S := 'sgn()';
   39 : S := 'int()';
   40 : S := 'sqrt()';
   41 : S := 'exp()';
   42 : S := 'ln()';
   43 : S := 'log10()';
   44 : S := 'log2()';
   45 : S := 'sin()';
   46 : S := 'cos()';
   47 : S := 'tan()';
   48 : S := 'arcsin()';
   49 : S := 'arccos()';
   50 : S := 'arctan()';
   51 : S := 'arctan2()';
   52 : S := 'deg()';
   53 : S := 'sinh()';
   54 : S := 'cosh()';
   55 : S := 'tanh()';
   56 : S := 'arcsinh()';
   57 : S := 'arccosh()';
   58 : S := 'arctanh()';
   59 : S := 'rnd()';
   60 : S := 'rad()';
   61 : S := 'fact()';
   62 : S := 'binomial()';
   63 : S := 'gamma()';
   64 : S := 'igamma()';
   65 : S := 'beta()';
   66 : S := 'ibeta()';
   67 : S := 'erf()';
   68 : S := 'lambertW()';
  end;

  { Insert S at the current cursor position }
  K := EditFormula.SelStart;
  Formula := EditFormula.Text;
  Insert(S, Formula, K + 1);
  EditFormula.Text := Formula;

  { Move cursor at the end of the inserted string,
    or between the 2 parentheses if it's a function (other tha rnd() }
  L := Length(S);
  if (I in [37..58]) or (I in [60..68]) then Dec(L);
  EditFormula.SelStart := K + L;

  EditFormula.SetFocus;
end;

procedure TForm1.BtnBkspClick(Sender: TObject);
var
  L : Integer;
begin
  L := Length(EditFormula.Text) - 1;
  EditFormula.Text := Copy(EditFormula.Text, 1, L);
  EditFormula.SelStart := L;
  EditFormula.SetFocus;
end;

procedure TForm1.BtnClearClick(Sender: TObject);
begin
  EditFormula.Text := '';
end;

procedure TForm1.BtnEvalClick(Sender: TObject);
var
  Res : Float;
begin
  SetVariable('A', StrToFloat(EditA.Text));
  SetVariable('B', StrToFloat(EditB.Text));
  SetVariable('C', StrToFloat(EditC.Text));
  SetVariable('D', StrToFloat(EditD.Text));

  SetErrcode(FOk);

  Res := Eval(EditFormula.Text);

  case MathErr of
    FOk        : EditResult.Text := FloatToStr(Res);
    FDomain    : EditResult.Text := 'Argument domain error';
    FSing      : EditResult.Text := 'Function singularity';
    FOverflow  : EditResult.Text := 'Overflow range error';
    FUnderflow : EditResult.Text := 'Underflow range error';
    FTLoss     : EditResult.Text := 'Total loss of precision';
    FPLoss     : EditResult.Text := 'Partial loss of precision';
  end;
end;

procedure TForm1.EditFormulaKeyDown(Sender: TObject;
                       var Key: Word; Shift: TShiftState);
begin
  if Key = vk_Return then BtnEvalClick(Sender);
end;

end.
