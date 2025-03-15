{ ******************************************************************
  This program demonstrates the math parser and shows how to add
  variables and functions at run time.
  ****************************************************************** }

program eval2;

uses
{$IFDEF USE_DLL}
  dmath;
{$ELSE}  
  utypes, ueval;
{$ENDIF}  

var
  N : Integer;
  S : String;

function Average(N : TArgC; X : TVector) : Float;
var
  I : Integer;
  S : Float;
begin
  S := 0.0;
  for I := 1 to N do
    S := S + X[I];
  Average := S / N;
end;

begin
  { Initialize parser }
  N := InitEval;
  Writeln(N, ' functions initialized');
  Writeln;

  { Simple expression }
  S := '1 + 1';
  Writeln(S, ' = ', Eval(S));
  Writeln;

  { Function }
  S := 'Sin(0.5)^2 + Cos(0.5)^2';
  Writeln(S, ' = ', Eval(S));
  Writeln;

  { Adding a variable (26 variables from 'A' to 'Z') }
  SetVariable('X', Pi);
  S := 'X';      Writeln(S, '      = ', Eval(S));
  S := 'Sin(X)'; Writeln(S, ' = ', Eval(S));
  S := 'Cos(X)'; Writeln(S, ' = ', Eval(S));
  Writeln;

  { Adding a function (up to 20 functions) }
  SetFunction('Average', @Average);
  S := 'Average(1, 2, 3, 4, 5, 6)';  { up to 26 arguments }
  Writeln(S, ' = ', Eval(S));

  Readln;
end.
