{ ******************************************************************
  Procedures used by nonlinear regression programs
  ****************************************************************** }

unit regutils;

interface

uses
{$IFDEF USE_DLL}
  dmath, regutils;
{$ELSE}
  utypes, uScaling, ustrings, uplot;
{$ENDIF}

function ReadInputFile(InFName                 : String;
                       var Title, XName, YName : String;
                       var N                   : Integer;
                       var X, Y                : TVector) : Integer;
{ -------------------------------------------------------------------
  Reads an input file for linear or nonlinear regression.
  The input file is an ASCII file with the following structure :

    Line 1 : Title of study
    Line 2 : Number of variables (must be 2 here !)
    Line 3 : Name of variable x
    Line 4 : Name of variable y
    Line 5 : Number of points
             (must be > number of fitted parameters !)

    The next lines contain the coordinates (x, y) of the points
    (1 point by line). The values of x and y must be separated by
    spaces or tabs.
  ------------------------------------------------------------------ }

procedure WriteResults(FuncName          : String;
                       N                 : Integer;
                       X, Y, S, Ycalc    : TVector;
                       B                 : TVector;
                       V                 : TMatrix;
                       FirstPar, LastPar : Integer;
                       Test              : TRegTest;
                       Alpha, Tc, Fc     : Float);
{ ------------------------------------------------------------------
  Writes results to screen
  ------------------------------------------------------------------ }

procedure PlotGraph(PltFunc : TFunc;
                    Title   : String;
                    N       : Integer;
                    X, Y    : TVector);
{ ------------------------------------------------------------------
  Plots points and fitted curve
  ------------------------------------------------------------------ }

implementation

function ReadInputFile(InFName                 : String;
                       var Title, XName, YName : String;
                       var N                   : Integer;
                       var X, Y                : TVector) : Integer;
var
  InF  : Text;     { Input file }
  Nvar : Integer;  { Number of variables }
  K    : Integer;  { Loop variable }
begin
  Assign(InF, InFName);
  Reset(InF);

  ReadLn(InF, Title);
  ReadLn(InF, Nvar);

  if Nvar <> 2 then
    begin
      WriteLn('Data file must contain 2 variables !');
      ReadInputFile := - 1;
      Exit;
    end;

  ReadLn(InF, XName);
  ReadLn(InF, YName);
  ReadLn(InF, N);

  DimVector(X, N);
  DimVector(Y, N);

  for K := 1 to N do
    ReadLn(InF, X[K], Y[K]);

  Close(InF);
  ReadInputFile := 0;
end;

procedure WriteResults(FuncName          : String;
                       N                 : Integer;
                       X, Y, S, Ycalc    : TVector;
                       B                 : TVector;
                       V                 : TMatrix;
                       FirstPar, LastPar : Integer;
                       Test              : TRegTest;
                       Alpha, Tc, Fc     : Float);
var
  Line1,
  Line2 : String;   { Separating lines }
  Delta : Float;    { Residual }
  Sr    : Float;    { Residual standard deviation }
  SB    : Float;    { Standard deviations of parameters }
  I     : Integer;  { Loop variable }

begin
  Line1 := StrChar(73, '-');
  Line2 := StrChar(73, '=');

  WriteLn(Line2);
  WriteLn('Nonlinear regression: ', FuncName);
  WriteLn(Line1);

  WriteLn('Parameter   Est.value         Std.dev.        ',
          (100 * (1 - Alpha)):2:0, '% Confidence Interval');

  WriteLn(Line1);

  for I := FirstPar to LastPar do
    begin
      SB := Sqrt(V[I,I]);
      WriteLn('B(', I:1, ')', B[I]:17:8, SB:17:8,
              (B[I] - Tc * SB):17:8, ';', (B[I] + Tc * SB):17:8);
    end;

  WriteLn(Line1);

  WriteLn('Number of observations            : n           = ', N:5);

  with Test do
    begin
      Sr := Sqrt(Vr);
      WriteLn('Residual error                    : s           = ', Sr:10:4);

      if R2 <= 1.0 then
        begin
          WriteLn('Coefficient of correlation        : r           = ', (Sqrt(R2)):10:4);
          WriteLn('Coefficient of determination      : r2          = ', R2:10:4);
          WriteLn('Adjusted coeff. of determination  : r2a         = ', R2a:10:4);
        end;

      WriteLn('Variance ratio (explained/resid.) : F(', Nu1:3, ', ', Nu2:3, ') = ', F:10:4);
      WriteLn('Critical variance ratio           : F(p = ', (1 - Alpha):4:2, ') = ', Fc:10:4);
    end;

  WriteLn(Line1);
  WriteLn('  i        Y obs.       Y calc.      Residual      Std.dev.      Std.res.');
  WriteLn(Line1);

  for I := 1 to N do
    begin
      Delta := Y[I] - Ycalc[I];
      WriteLn(I:3, Y[I]:14:4, Ycalc[I]:14:4, Delta:14:4,
              S[I]:14:4, (Delta / S[I]):14:4);
    end;

  WriteLn(Line2);
end;

procedure PlotGraph(PltFunc : TFunc;
                    Title   : String;
                    N       : Integer;
                    X, Y    : TVector);
var
  Xmin, Xmax, Xstep : Float;  { Ox scale }
  Ymin, Ymax, Ystep : Float;  { Oy scale }

begin
  if not InitGraphics(9, 2, 'c:\tp\bgi') then  { 640x480 16 color }
    begin
      Writeln('Unable to set graphic mode');
      Exit;
    end;

  SetWindow(15, 85, 15, 85, True);

  AutoScale(X, 1, N, LinScale, Xmin, Xmax, Xstep);
  AutoScale(Y, 1, N, LinScale, Ymin, Ymax, Ystep);

  SetOxScale(LinScale, Xmin, Xmax, Xstep);
  SetOyScale(LinScale, Ymin, Ymax, Ystep);

  SetGraphTitle(Title);
  SetOxTitle('X');
  SetOyTitle('Y');

  PlotOxAxis;
  PlotOyAxis;

  WriteGraphTitle;

  SetClipping(True);

  SetLineParam(1, 0, 0, 0);  { Don't connect points }
  PlotCurve(X, Y, 1, N, 1);

  PlotFunc(PltFunc, Xmin, Xmax, 2);

  Readln;

  LeaveGraphics;
end;

end.
