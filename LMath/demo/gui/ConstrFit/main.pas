unit main;

{$mode objfpc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons, StrHolder,
  uTypes, uVectorHelper, uConstrNLFit, lmcoordsys, lmPointsVec, globals, ModelForms;

type

  { TMainForm }

  TMainForm = class(TForm)
    BitBtn1: TBitBtn;
    Button1: TButton;
    ShowParamsButton: TButton;
    CoordSys: TCoordSys;
    StrHolder: TStrHolder;
    procedure Button1Click(Sender: TObject);
    procedure CoordSysDrawData(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ShowParamsButtonClick(Sender: TObject);
  public
    DataColor, FittedColor : TColor;
    DataPoints:TPoints;
    FittedPoints:TPoints;
    procedure DrawDataPoints(Points:TPoints);
    procedure CalculateFit;
  end;

var
  MainForm: TMainForm;

implementation
{$R *.lfm}
uses FitLogExp;
var
  X,Y:TVector;

procedure TMainForm.CalculateFit;
var
  YC: TVector;
begin
  DataPoints.ExtractX(X,0,DataPoints.Count-1);
  DataPoints.ExtractY(Y,0,DataPoints.Count-1);
  ConstrNLFit(@RFunc,@Constr,X,Y,0,DataPoints.Count-1,OFCalls,Rho,Variables,VarNum,ConstrNum,MaxCV);
  YC := GetCFFittedData;
  FittedPoints := TPoints.Combine(X,YC,0,high(X));
  Finalize(X);
  Finalize(Y);
end;

procedure TMainForm.CoordSysDrawData(Sender: TObject);
begin
  DrawDataPoints(DataPoints);
  CoordSys.Canvas.Pen.Color := FittedColor;
  DrawDataPoints(FittedPoints);
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  CalculateFit;
  CoordSys.Invalidate;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  DataColor := clBlue;
  FittedColor := clMaroon;
  ReadDataPoints(DataPoints, StrHolder.Strings);
  CoordSys.NewLimits(DataPoints[0].X-0.2,DataPoints.MinY-0.2,DataPoints[DataPoints.Count-1].X+0.2,
                     DataPoints.MaxY + 0.2);
  CoordSys.YGridDist := (CoordSys.MaxY - CoordSys.MinY)/6;
  Variables.FillWithArr(1,[1.0,0.6,0.3,0.003,0.02,0.1]); //< fill with guess values
  RhoBeg := 0.5;       //< initial step of variable values
  Rho := 0.000001;     //< end step of variable vaules, defines accuracy of fit
  OFCalls := 150000;   //< maximal number of calls to objective function
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(DataPoints);
  FreeAndNil(FittedPoints);
  Finalize(Variables);
end;

procedure TMainForm.ShowParamsButtonClick(Sender: TObject);
var
  MR:integer;
begin
  MR := ModelForm.ShowModal;
  if MR = mrOK then
    ModelForm.ReturnVars;
end;

procedure TMainForm.DrawDataPoints(Points:TPoints);
var
  I:integer;
begin
  if not Assigned(Points) then Exit;
  CoordSys.PenPos := Points[0];
  for I := 1 to Points.Count - 1 do
    CoordSys.LineTo(Points[I]);
end;

end.

