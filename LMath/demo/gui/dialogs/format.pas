unit Format;

{$MODE Delphi}

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, Spin;

type
  TFormatDlg = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    BitBtn1: TBitBtn;
    procedure CheckBox1Click(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure SpinEdit2Change(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    procedure DisplayNumber;
  public
  end;

var
  FormatDlg: TFormatDlg;

implementation

{$R format.lfm}

uses
  ustrings;

procedure TFormatDlg.DisplayNumber;
var
  NumLength, MaxDec  : Integer;
  FloatPoint, NSZero : Boolean;
begin
  MaxDec := SpinEdit2.Value;
  FloatPoint := CheckBox1.Checked;
  NSZero := CheckBox2.Checked;
  if FloatPoint then
    NumLength := MaxDec + 10
  else
    NumLength := MaxDec + 3 + SpinEdit1.Value;
  SetFormat(NumLength, MaxDec, FloatPoint, NSZero);
  Label3.Caption := FloatStr(-1.2345);
end;

procedure TFormatDlg.FormActivate(Sender: TObject);
begin
  DisplayNumber;
end;

procedure TFormatDlg.CheckBox1Click(Sender: TObject);
begin
  Label1.Visible := not CheckBox1.Checked;
  SpinEdit1.Visible := Label1.Visible;
  DisplayNumber;
end;

procedure TFormatDlg.SpinEdit1Change(Sender: TObject);
begin
  DisplayNumber;
end;

procedure TFormatDlg.SpinEdit2Change(Sender: TObject);
begin
  DisplayNumber;
end;

procedure TFormatDlg.CheckBox2Click(Sender: TObject);
begin
  DisplayNumber;
end;

end.
