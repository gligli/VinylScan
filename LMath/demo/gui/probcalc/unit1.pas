unit Unit1;

{$MODE Delphi}

interface

uses
  SysUtils, LCLIntf, LCLType, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    LabelFormula: TLabel;
    LabelResult: TLabel;
    EditFormula: TEdit;
    EditResult: TEdit;
    BtnPBinom: TButton;
    BtnFBinom: TButton;
    BtnPPoisson: TButton;
    BtnFPoisson: TButton;
    BtnDExpo: TButton;
    BtnFExpo: TButton;
    BtnDGamma: TButton;
    BtnFGamma: TButton;
    BtnDBeta: TButton;
    BtnFBeta: TButton;
    BtnDNorm: TButton;
    BtnFNorm: TButton;
    BtnPNorm: TButton;
    BtnInvNorm: TButton;
    BtnDStudent: TButton;
    BtnFStudent: TButton;
    BtnPStudent: TButton;
    BtnInvStudent: TButton;
    BtnDKhi2: TButton;
    BtnFKhi2: TButton;
    BtnPKhi2: TButton;
    BtnInvKhi2: TButton;
    BtnDSnedecor: TButton;
    BtnFSnedecor: TButton;
    BtnPSnedecor: TButton;
    BtnInvSnedecor: TButton;
    BtnEval: TButton;
    BtnClear: TButton;

    procedure FormCreate(Sender: TObject);
    procedure ButtonClick(Sender: TObject);
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
  I : Integer;
  S, Formula : string;
begin
  I := (Sender as TButton).TabOrder;

  case I of
    4 : S := 'pBinom(';
    5 : S := 'fBinom(';
    6 : S := 'pPoisson(';
    7 : S := 'fPoisson(';
    8 : S := 'dExpo(';
    9 : S := 'fExpo(';
   10 : S := 'dGamma(';
   11 : S := 'fGamma(';
   12 : S := 'dBeta(';
   13 : S := 'fBeta(';
   14 : S := 'dNorm(';
   15 : S := 'fNorm(';
   16 : S := 'pNorm(';
   17 : S := 'invNorm(';
   18 : S := 'dStudent(';
   19 : S := 'fStudent(';
   20 : S := 'pStudent(';
   21 : S := 'invStudent(';
   22 : S := 'dKhi2(';
   23 : S := 'fKhi2(';
   24 : S := 'pKhi2(';
   25 : S := 'invKhi2(';
   26 : S := 'dSnedecor(';
   27 : S := 'fSnedecor(';
   28 : S := 'pSnedecor(';
   29 : S := 'invSnedecor(';
  end;

  { Insert S at the current cursor position }
  I := EditFormula.SelStart;
  Formula := EditFormula.Text;
  Insert(S, Formula, I + 1);
  EditFormula.Text := Formula;

  { Move cursor at the end of the inserted string }
  EditFormula.SelStart := I + Length(S);

  EditFormula.SetFocus;
end;

procedure TForm1.BtnEvalClick(Sender: TObject);
var
  Res : Float;
begin
  SetErrCode(FOk);

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

procedure TForm1.BtnClearClick(Sender: TObject);
begin
  EditFormula.Text := '';
end;

end.
