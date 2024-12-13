unit main;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, lmcoordsys, Forms, Controls,
  Graphics, Dialogs, uTypes, uDistrib, uDistribs;

type

  { TForm1 }

  PDistribData = ^TDistribData;
  TDistribData = record
    beta1, beta2: float;
  end;

  TForm1 = class(TForm)
    CoordSys: TCoordSys;
    procedure CoordSysDrawData(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    MinVal,
    MaxVal      : float; //< Minimal and maximal values of a random variable in RawData
    HistoLength : integer; //< number of bins in the histogram
    HWidth      : float;  //< bin width
    DistribData : TDistribData; //< calculated distribution coefficients
    RawData     : TVector;  //< random variable
    Histo       : TStatClassVector; //< data of the primary histogram
    XV, DV      : TVector; //< vectors of X-values and prob.densities for the histogram
  end;

var
  Form1: TForm1;

implementation
{$R *.lfm}

// Wrapper around HypoExponentialDistribution2 function, of type TParamFunc
//which may be passed to DrawFunc method of TCoordSys
function HypoExpDrawing(X:float; Params:PDistribData):float;
begin
  Result := HypoExponentialDistribution2(Params^.beta1,Params^.beta2,X);
end;

// in TCoordSys, all user-defined drawing must occur in OnDrawData event
procedure TForm1.CoordSysDrawData(Sender: TObject);
var
  I:integer;
  SysColor:TColor;
begin
  SysColor := CoordSys.Canvas.Brush.Color;
  CoordSys.Canvas.Brush.Color := clGreen;
  for I := 1 to HistoLength do
    CoordSys.FillRect(Histo[I].Inf,0,Histo[I].Sup,Histo[I].D);
  CoordSys.DrawFunc(@HypoExpDrawing,@DistribData,0,CoordSys.MaxX);
  CoordSys.Canvas.Brush.Color := SysColor;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  T:TextFile;
  I:integer;
begin
  AssignFile(T,'hypo.txt');
  DimVector(RawData,1640);
  MinVal := MaxNum;
  MaxVal := 0;
  I := 1;
  try
    Reset(T);
    while not Eof(T) do
    begin
      readln(T,RawData[I]);
      if RawData[I] > MaxVal then
        MaxVal := RawData[I];
      if RawData[I] < MinVal then
        MinVal := RawData[I];
      Inc(I);
    end;
  except
    on Exception do
    begin
      ShowMessage('Missing or unreadable file "hypo.txt"');
      Halt;
    end;
  end;
  HWidth := (MaxVal-MinVal)/50;
  HistoLength := DimStatClassVector(Histo,MinVal,MaxVal,HWidth);
  Distrib(RawData,1,High(RawData),MinVal,HWidth,Histo);
  distExtractX(Histo, XV);
  distExtractDensity(Histo,DV);
  Fit2HypoExponents(XV,DV,HistoLength,DistribData.beta1,DistribData.beta2);
end;

end.

