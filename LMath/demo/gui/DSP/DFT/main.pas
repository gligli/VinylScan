unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Spin, Buttons, ComCtrls,
  lmnumericedits, lmcoordsys, uEval, uTypes, uComplex, uIntervals, Transforms, uVecUtils, uCompVecUtils,
  lmPointsVec;

type

  { TMainForm }

  TMainForm = class(TForm)
    CloseBtn: TBitBtn;
    ImExpressionEdit: TLabeledEdit;
    OpenDialog: TOpenDialog;
    StaticText2: TStaticText;
    TimeImCoords: TCoordSys;
    StaticText1: TStaticText;
    TimeReCoords: TCoordSys;
    PhaseCoords: TCoordSys;
    MagCoords: TCoordSys;
    Executebtn: TBitBtn;
    Label2: TLabel;
    InvMethodCombo: TComboBox;
    MethodCombo: TComboBox;
    ExpressionEdit: TLabeledEdit;
    MethodLabel: TLabel;
    MaxEdit: TFloatEdit;
    NLabel: TLabel;
    MaxLabel: TLabel;
    MinEdit: TFloatEdit;
    IntervalBox: TGroupBox;
    MinLabel: TLabel;
    NEdit: TSpinEdit;
    PageControl1: TPageControl;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    TimeSheet: TTabSheet;
    FreqSheet: TTabSheet;
    PolarToggle: TToggleBox;
    ToolPanel: TPanel;
    procedure ExecutebtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MagCoordsDrawData(Sender: TObject);
    procedure PhaseCoordsDrawData(Sender: TObject);
    procedure TimeImCoordsDrawData(Sender: TObject);
    procedure TimeReCoordsDrawData(Sender: TObject);
    procedure SetData;
    procedure SetNewCoords(Coords:TCoordSys;Points:TPoints);
  private
  public
    TimePoints:TPoints;
    InvTimePoints:TPoints;
    ImTimePoints:TPoints;
    InvImTimePoints:TPoints;
    TransRePoints:TPoints;
    TransImPoints:TPoints;
  end;

var
  MainForm: TMainForm;

implementation
var
  N: integer;
  TimeVector : TVector;
  FreqVector : TVector;
  InArray: TCompVector;
  TransArray : TCompVector;
  InvTransArray : TCompVector;
  Expression : string;
  ExpressionIm : string;
  // 0 FFT, 1 DFT, 2 FFTFort
  SelectedMethod, SelectedMethodInv : integer;

  Range: TInterval;

{$R *.lfm}

procedure TMainForm.SetData;
begin
  N := NEdit.Value;
  Expression := ExpressionEdit.text;
  ExpressionIm := ImExpressionEdit.text;
  Range.Lo := MinEdit.Value;
  Range.Hi := MaxEdit.Value;
  SelectedMethod := MethodCombo.ItemIndex;
  SelectedMethodInv := InvMethodCombo.ItemIndex;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  InitEval;
end;

procedure TMainForm.MagCoordsDrawData(Sender: TObject);
begin
  if Assigned(TransRePoints) then
    MagCoords.FastDraw(TransRePoints.Points,0,N-1);
end;

procedure TMainForm.PhaseCoordsDrawData(Sender: TObject);
begin
  if Assigned(TransImPoints) then
    PhaseCoords.FastDraw(TransImPoints.Points,0,N-1);

end;

procedure TMainForm.TimeImCoordsDrawData(Sender: TObject);
begin
  with TimeImCoords do
  begin
    Canvas.Pen.Color := clBlue;
    if Assigned(ImTimePoints) then
      FastDraw(ImTimePoints.Points,0,TimePoints.Count-1);
    Canvas.Pen.Color := clRed;
    if Assigned(InvImTimePoints) then
      FastDraw(InvImTimePoints.Points,0,TimePoints.Count-1);
  end;
end;

procedure TMainForm.TimeReCoordsDrawData(Sender: TObject);
begin
  with TimeReCoords do
  begin
    if Assigned(TimePoints) then
    begin
      Canvas.Pen.Color := clBlue;
      FastDraw(TimePoints.Points,0,TimePoints.Count-1);
    end;
    if Assigned(InvTimePoints) then
    begin
      Canvas.Pen.Color := clRed;
      FastDraw(InvTimePoints.Points,0,TimePoints.Count-1);
    end;
  end;
end;

procedure TMainForm.SetNewCoords(Coords:TCoordSys;Points:TPoints);
var
  MiY,MaY:float;
  MarginX, MarginY : float;
begin
  MiY := Points.MinY;
  MaY := Points.MaxY;
  MarginY := Points.RangeY / 10;
  if IsZero(MarginY) then
    MarginY := 0.5;
  MaY := MaY + MarginY;
  MiY := MiY - MarginY;
  MarginX := Points.Range / 10;
  Coords.NewLimits(Points.MinX-MarginX,MiY,Points.MaxX+MarginX,MaY);
  Coords.XGridDist := Points.Range / 5;
  Coords.YGridDist := Points.RangeY / 5;
  Coords.XGridNumbersDecimals := 2;
  Coords.YGridNumbersDecimals := 2;
end;

procedure TMainForm.ExecutebtnClick(Sender: TObject);
var
  K: integer;
  RT,IT:TVector;
begin
  SetData;
  GenerateData(Expression,ExpressionIm, N, Range, TimeVector, InArray);
  K := N-1;
  RT := ExtractReal(InArray);
  IT := ExtractImaginary(InArray);
  TimePoints := TPoints.Combine(TimeVector,RT,0,K);
  SetNewCoords(TimeReCoords,TimePoints);
  ImTimePoints := TPoints.Combine(TimeVector,IT,0,K);
  SetNewCoords(TimeImCoords,ImTimePoints);
  Execute(InArray,N,SelectedMethod,SelectedMethodInv,TransArray,InvTransArray);
  if PolarToggle.Checked then
    apply(TransArray,@CToPolar);
  FreqVector := Seq(0,K,-0.5,1.0/N);
  RT := ExtractReal(TransArray);
  IT := ExtractImaginary(TransArray);
  TransRePoints := TPoints.Combine(FreqVector,RT,0,K);
  TransImPoints := TPoints.Combine(FreqVector,IT,0,K);
  RT := ExtractReal(InvTransArray);
  IT := ExtractImaginary(InvTransArray);
  InvTimePoints := TPoints.Combine(TimeVector,RT,0,K);
  InvImTimePoints := TPoints.Combine(TimeVector,IT,0,K);
  SetNewCoords(MagCoords,TransRePoints);
  SetNewCoords(PhaseCoords,TransImPoints);
end;

end.

