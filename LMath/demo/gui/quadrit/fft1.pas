{ **********************************************************************
  This program demonstrates the behavior of a dynamical system: the
  quadratic iterator (also known as the logistic sequence).

  The system is defined by the relationship:

                       x(n+1) = a.x(n).[1 - x(n)]

  According to the value of a the sequence may be:

    * periodic (e.g. for a = 3.5 the period is 4)
    * chaotic (e.g. a = 4)
    * presenting an alternance of periodic and chaotic phases,
       a phenomenon known as intermittency (e.g. a = 3.82812)

  This program demonstrates the period-doubling route of the
  quadratic iterator by computing the FFT

  The period corresponds to the lowest frequency of the spectrum

  Examples : a = 3     ==> Period 2
             a = 3.5   ==> Period 4
             a = 3.55  ==> Period 8
             a = 3.566 ==> Period 16

  In the chaotic part (a > 3.57) there are also regions
  of periodic behavior, for instance:

    a = 3.63  ==> Period 6
    a = 3.74  ==> Period 5
    a = 3.83  ==> Period 3

  For more information about the quadratic iterator and other chaotic
  systems, see "The Chaos Hypertextbook" (http://hypertextbook.com/chaos/)
  by Glenn Elert.
  ********************************************************************** }

unit fft1;

{$MODE Delphi}

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;

    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;

    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;

    Edit1: TEdit;
    Edit2: TEdit;

    CheckBox1: TCheckBox;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;

    Image1: TImage;

    SaveDialog1: TSaveDialog;

    procedure CheckBox1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    procedure ClearGraphics;
    procedure PlotGraph(Canvas : TCanvas);
  public
    { Dйclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  utypes, umath, ucomplex, ufft, umeansd, uwinplot, GraphOpt;

procedure TForm1.ClearGraphics;
begin
  with Form1.Image1 do
    begin
      Canvas.Brush.Color := GraphOptDlg.BorderColorShape.Brush.Color;
      Canvas.FillRect(Rect(0, 0, Width, Height));
      Canvas.Brush.Color := GraphOptDlg.GraphColorShape.Brush.Color;
      Canvas.FillRect(Rect(GraphOptDlg.XminSpinEdit.Value * Width  div 100,
                           GraphOptDlg.YminSpinEdit.Value * Height div 100,
                           GraphOptDlg.XmaxSpinEdit.Value * Width  div 100,
                           GraphOptDlg.YmaxSpinEdit.Value * Height div 100));
    end;
end;

procedure TForm1.PlotGraph(Canvas : TCanvas);
begin
  ClearGraphics;

  InitGraphics(Image1.Width, Image1.Height);

  Canvas.Brush.Color := GraphOptDlg.GraphColorShape.Brush.Color;
  Canvas.Pen.Color := GraphOptDlg.AxisColorShape.Brush.Color;
  Canvas.Pen.Width := GraphOptDlg.AxisWidthSpinEdit.Value;

  SetWindow(Canvas,
            GraphOptDlg.XminSpinEdit.Value,
            GraphOptDlg.XmaxSpinEdit.Value,
            GraphOptDlg.YminSpinEdit.Value,
            GraphOptDlg.YmaxSpinEdit.Value,
            GraphOptDlg.GraphBorderCheckBox.Checked);

  Canvas.Brush.Color := GraphOptDlg.BorderColorShape.Brush.Color;
  Canvas.Font := GraphOptDlg.AxesFontDialog.Font;

  if GraphOptDlg.XplotCheckBox.Checked then PlotOxAxis(Canvas);
  if GraphOptDlg.YplotCheckBox.Checked then PlotOyAxis(Canvas);

  PlotGrid(Canvas, TGrid(GraphOptDlg.GridRadioGroup.ItemIndex));

  Canvas.Font := GraphOptDlg.TitleFontDialog.Font;
  WriteGraphTitle(Canvas);
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  SetOxScale(LinScale, 0, 0.5, 0.0625);
  SetOyScale(LogScale, 1.0, 1.0E+5, 10);

  SetOxTitle('Frequency');
  SetOyTitle('FFT relative modulus');

  SetMaxCurv(1);
  // GetPointParam(1, Symbol, Size, Color);
  SetPointParam(1, 0, 0, clBlue);


  Randomize;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  Edit2.Enabled := not CheckBox1.Checked;
end;

procedure TForm1.Button1Click(Sender: TObject);
{ Orbit diagram }
var
  A, X0, X           : Float;
  N_hide, N_show, I  : Integer;
  MaxIndex, MidIndex : Integer;
  Freq, FFT_mod      : TVector;
  FFT_in, FFT_out    : TCompVector;
  MinMod             : Float;
begin
  A := StrToFloat(Edit1.Text);
  if (A < 1.0) or (A > 4.0) then
    begin
      MessageDlg('a must be in [1, 4]', mtError, [mbOk], 0);
      Exit;
    end;

  if CheckBox1.Checked then
    X0 := Random
  else
    X0 := StrToFloat(Edit2.Text);

  if (X0 <= 0) or (X0 >= 1.0) then
    begin
      MessageDlg('x0 must be in (0, 1)', mtError, [mbOk], 0);
      Exit;
    end;

  N_hide := SpinEdit1.Value;
  N_show := Trunc(IntPower(2, SpinEdit2.Value));

  MaxIndex := N_show - 1;
  MidIndex := N_show div 2;

  DimVector(Freq, MidIndex);
  DimVector(FFT_mod, MidIndex);

  DimVector(FFT_in, MaxIndex);
  DimVector(FFT_out, MaxIndex);

  X := X0; FFT_in[0] := Cmplx(X0, 0.0);

  for I := 1 to MaxIndex do
    begin
      X := A * X * (1.0 - X);
      FFT_in[I] := Cmplx(X, 0.0);
    end;

  FFT(N_show, FFT_in, FFT_out);

  for I := 0 to MidIndex do
    begin
      Freq[I] := I / N_show;
      FFT_mod[I] := CAbs(FFT_out[I]);
    end;

  MinMod := Min(FFT_mod, 1, MidIndex);

  if MinMod > 0.0 then
    for I := 0 to MidIndex do
      FFT_mod[I] := FFT_mod[I] / MinMod;

  PlotGraph(Image1.Canvas);
  PlotCurve(Image1.Canvas, Freq, FFT_mod, 1, Pred(MidIndex), 1);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  GraphOptDlg.ShowModal;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
    Image1.Picture.SaveToFile(SaveDialog1.FileName);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  Form1.Close;
end;

end.
