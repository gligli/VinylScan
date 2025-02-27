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

  The program plots an "orbit" diagram, in which the value of x(n)
  is plotted vs. n for a given number of iterations

  For more information about the quadratic iterator and other chaotic
  systems, see "The Chaos Hypertextbook" (http://hypertextbook.com/chaos/)
  by Glenn Elert.
  ********************************************************************** }

unit orbit1;

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
    
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    
    Edit1: TEdit;
    Edit2: TEdit;

    CheckBox1: TCheckBox;
    
    SpinEdit1: TSpinEdit;

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
  utypes, uwinplot, GraphOpt;

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
  SetOxScale(LinScale, 0, 100, 20);
  SetOyScale(LinScale, 0, 1, 0.2);

  SetOxTitle('Iteration');
  SetOyTitle('x');

  SetMaxCurv(1);

  Randomize;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  Edit2.Enabled := not CheckBox1.Checked;
end;

procedure TForm1.Button1Click(Sender: TObject);
{ Orbit diagram }
var
  A, X0 : Float;
  I, N  : Integer;
  T, X  : TVector;
begin
  N := SpinEdit1.Value;

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

  DimVector(T, N);
  DimVector(X, N);

  T[0] := 0;
  X[0] := X0;

  for I := 1 to N do
    begin
      T[I] := I;
      X[I] := A * X[I-1] * (1.0 - X[I-1]);
    end;

  PlotGraph(Image1.Canvas);
  PlotCurve(Image1.Canvas, T, X, 0, N, 1);
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
