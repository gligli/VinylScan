unit main;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Memo1: TMemo;
    SpinEdit1: TSpinEdit;
    Button1: TButton;
    Button2: TButton;
    Button4: TButton;
    Image1: TImage;
    Button3: TButton;
    SaveDialog1: TSaveDialog;

    procedure FormActivate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    function GetNCurv : Integer;
    procedure GetFuncNames;
    procedure InitGraphVar;
    procedure ClearGraphics;
    procedure PlotCurves(Canvas : TCanvas);
    procedure PlotGraph(Canvas : TCanvas);
  public
    { Dйclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R main.lfm}

uses
  utypes, umath, ueval, uwinplot, GraphOpt;

const
  MaxCurv = 9;

function TForm1.GetNCurv : Integer;
begin
  Result := 0;
  while Memo1.Lines[Result] <> '' do
    Inc(Result);
end;

procedure TForm1.GetFuncNames;
var
  I : Integer;
begin
  for I := 1 to MaxCurv do
    SetCurvLegend(I, Memo1.Lines[I - 1]);
end;

procedure TForm1.InitGraphVar;
var
  I,
  Symbol,
  Size,
  Width  : Integer;
  Style  : TPenStyle;
  Color  : TColor;
begin
  SetMaxCurv(MaxCurv);
  GetFuncNames;

  for I := 1 to MaxCurv do
    begin
      GetPointParam(I, Symbol, Size, Color);
      SetPointParam(I, 0, 1, Color);

      GetLineParam(I, Style, Width, Color);
      SetLineParam(I, Style, 2, Color);
    end;
end;

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

procedure TForm1.PlotCurves(Canvas : TCanvas);
var
  Scale             : TScale;
  Xmin, Xmax, XStep : Float;
  Npts, I, K        : Integer;
  X, Y              : TVector;
  H                 : Float;
begin
  Npts := SpinEdit1.Value;

  DimVector(X, Npts);
  DimVector(Y, Npts);

  GetOxScale(Scale, Xmin, Xmax, XStep);

  X[0] := Xmin;
  H := (Xmax - Xmin) / Npts;

  for I := 1 to Npts do
    X[I] := X[I - 1] + H;

  if Scale = LogScale then
    for I := 0 to Npts do
      X[I] := Exp10(X[I]);

  for K := 1 to GetNCurv do
    begin
      for I := 0 to Npts do
        begin
          SetVariable('X', X[I]);
          Y[I] := Eval(Memo1.Lines[K - 1]);
        end;
      PlotCurve(Canvas, X, Y, 0, Npts, K);
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

  if GraphOptDlg.LgdBorderCheckBox.Checked then
    begin
      Canvas.Font := GraphOptDlg.LgdFontDialog.Font;
      WriteLegend(Canvas, GetNCurv, True, True);
    end;

  PlotCurves(Canvas);
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  { Default number of points = Nb of pixels on Ox }
  SpinEdit1.Value := Image1.Width;

  { Initialize the formula parser }
  InitEval;

  { Initialize graphic variables }
  InitGraphVar;

  { Plot default graph }
  PlotGraph(Image1.Canvas);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  GetFuncNames;
  GraphOptDlg.ShowModal;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  I, NCurv : Integer;
begin
  NCurv := GetNCurv;

  for I := 1 to NCurv do
    SetCurvLegend(I, Memo1.Lines[I - 1]);

  PlotGraph(Image1.Canvas);
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
