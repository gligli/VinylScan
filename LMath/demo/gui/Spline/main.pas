{******************************************************************
 *Part of LMath. This program demonstrates use of uSpline unit    *
 *and DrawFunction method in TCoordSys                            *
 ******************************************************************}

unit main;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, lmcoordsys, Forms, Controls,
  Graphics, Dialogs, uTypes, uRealPoints, uSpline, UStrings;

type

  { TForm1 }

  PSplintData = ^TSplintData;

  TSplintData = record   // structure needed for SplineDrawing function
    XA, YA, DA : TVector;
    LB, UB : integer;
  end;

  TForm1 = class(TForm)
    CoordSys: TCoordSys;
    procedure CoordSysDrawData(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    SplintData: TSplintData;
    Points    : TRealPointVector;
    Derivs    : TVector;
    Minima    : TRealPointVector;
    Maxima    : TRealPointVector;
    NMin,NMax : integer;
  end;

var
  Form1: TForm1;
const
  Lbb = 3;
  Ubb = Lbb+6;

implementation
{$R *.lfm}

// Wrapper around Splint function, of type TParamFunc
//which may be passed to DrawFunc method of TCoordSys
function SplineDrawing(X:Float; Params:PSplintData):Float;
begin
  Result := Splint(X,Params^.XA,Params.YA,Params.DA,Params.LB,Params.UB);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  I:integer;
begin
  with SplintData do
  begin
    LB := Lbb;
    UB := Ubb;
    DimVector(XA,Ubb+3);
    DimVector(YA,Ubb+3);
    DimVector(DA,Ubb+3);
    DimVector(Points,Ubb+3);
    for I := 0 to Ubb+2 do  // here points for spline
    begin                                 // drawing are created
      XA[I] := I-3-Lbb;
      YA[I] := sin(XA[I]);
      Points[I] := rpPoint(XA[I],YA[I]);
    end;
    InitSpline(XA,YA,DA,Lbb,Ubb);
    FindSplineExtremums(XA,YA,DA,Lbb,Ubb,Minima,Maxima,NMin,NMax);
  end;
end;

// in TCoordSys, all user-defined drawing must occur in OnDrawData event
procedure TForm1.CoordSysDrawData(Sender: TObject);
var
  I:integer;
  OutCoord:TIntegerPoint;
  Txt : string;
begin
  CoordSys.DrawFunc(@SplineDrawing,@SplintData,CoordSys.MinX,CoordSys.MaxX);
  for I := Lbb to Ubb do
  begin
    CoordSys.Circle(Points[I],3);
  end;
  for I := 1 to NMin do
  begin
    Txt := 'Min: (' + Trim(FloatStr(Minima[I].X)) + ';' + Trim(FloatStr(Minima[I].Y)) + ')';
    OutCoord := CoordSys.UserToScreen(Minima[I]);
    Dec(OutCoord.X,Canvas.TextWidth(Txt) div 2);
    Inc(OutCoord.Y, 2);
    CoordSys.Canvas.TextOut(OutCoord.X,OutCoord.Y,Txt);
  end;
  for I := 1 to NMax do
  begin
    Txt := 'Max: (' + Trim(FloatStr(Maxima[I].X)) + ';' + Trim(FloatStr(Maxima[I].Y)) + ')';
    OutCoord := CoordSys.UserToScreen(Maxima[I]);
    Dec(OutCoord.X,Canvas.TextWidth(Txt) div 2);
    Dec(OutCoord.Y, Canvas.TextHeight(Txt) + 2);
    CoordSys.Canvas.TextOut(OutCoord.X,OutCoord.Y,Txt);
  end;
end;


end.

