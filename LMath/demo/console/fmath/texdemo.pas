{ ******************************************************************
  This program creates a TeX file which plots a Fourier series:

  F1 = 0.75 * (1 + Cos(Phi))
  F2 = 2.5 * [1 + Cos(2 * Phi - Pi)]
  F3 = F1 + F2

  Note: this function is used in chemistry to describe the torsional
        energy of an amide bond.

  The resulting file must be processed with LaTeX. The graphics may be
  converted to PostScript using the dvips utility.

  Example: latex figure
           dvips figure
  ****************************************************************** }

program texdemo;

uses
{$IFDEF USE_DLL}
  dmath;
{$ELSE}  
  utypes, utexplot;
{$ENDIF}  

const
  Npt      = 100;
  PiDiv180 = Pi / 180;

function Func1(Phi : Float) : Float;
begin
  Func1 := 0.75 * (1.0 + Cos(Phi * PiDiv180));
end;

function Func2(Phi : Float) : Float;
begin
  Func2 := 2.5 * (1.0 + Cos(2.0 * Phi * PiDiv180 - Pi));
end;

function Func3(Phi : Float) : Float;
begin
  Func3 := Func1(Phi) + Func2(Phi);
end;

begin
  if not TeX_InitGraphics('figure.tex', 15, 10, True) then
    begin
      Writeln('Unable to create LaTeX file!');
      Halt;
    end;

  TeX_SetWindow(10, 90, 10, 90, True);

  TeX_SetOxTitle('$\phi$ (deg)');
  TeX_SetOyTitle('E (kcal/mol)');

  TeX_SetOxScale(LinScale, -180, 180, 45);
  TeX_SetOyScale(LinScale, 0, 6, 1);

  TeX_PlotOxAxis;
  TeX_PlotOyAxis;

  TeX_SetLineParam(1, 2, 2, False);  { dotted, size 2, no smoothing }
  TeX_SetLineParam(2, 3, 2, False);  { dotted, size 2, no smoothing }
  TeX_SetLineParam(3, 1, 2, False);  { solid,  size 2, no smoothing }

  TeX_PlotFunc(@Func1, -180, 180, Npt, 1);
  TeX_PlotFunc(@Func2, -180, 180, Npt, 2);
  TeX_PlotFunc(@Func3, -180, 180, Npt, 3);

  TeX_LeaveGraphics(True);
end.
