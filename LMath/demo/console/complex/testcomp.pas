{ ******************************************************************
  Complex number demo
  ******************************************************************
  Based on ComplexMath Delphi library by E. F. Glynn
  http://www.efg2.com/Lab/Mathematics/Complex/index.html
  ****************************************************************** }

program testcomp;

uses
{$IFDEF USE_DLL}
  dmath;
{$ELSE}  
  utypes, ucomplex, ustrings;
{$ENDIF}  

var
  A : array[1..20] of Complex;
  I : Integer;

procedure Pause;
begin
  Writeln;
  Write('Press <Enter> to continue');
  ReadLn;
  Writeln;
end;

procedure TestConv;
var
  K : Integer;
begin
  WriteLn('Complex number definition/conversion/output:');
  WriteLn;
  WriteLn('       z rectangular                   z polar');
  WriteLn('   ----------------------     --------------------------');

  for K := 1 to 20 do
    WriteLn(CompStr(A[K]), ' ', FloatStr(CAbs(A[K]))
                 + ' * Exp(' + FloatStr(CArg(A[K])) + ' * i)');
  Pause;
end;

procedure TestArith;
var
  Z1, Z2 : Complex;
begin
  WriteLn('Complex arithmetic: +, -, *, /, ^');
  WriteLn;

  Z1 := Cmplx(1, 1); 
  Z2 := Cmplx(Sqrt(3), - 1);
  
  WriteLn('Let z1  = ':12, CompStr(Z1));
  WriteLn('    z2  = ':12, CompStr(Z2));
  WriteLn;
  WriteLn('z1 + z2 = ':12, CompStr(Z1+Z2));
  WriteLn('z1 - z2 = ':12, CompStr(Z1 - Z2));
  WriteLn('z1 * z2 = ':12, CompStr(Z1*Z2));
  WriteLn('z1 / z2 = ':12, CompStr(Z1/Z2));
  WriteLn('z1 ^ z2 = ':12, CompStr(CPower(Z1, Z2)));
  Pause;
end;

procedure TestFunc(Index : Integer);
var
  C, C1, Z, Z1 : Complex;
  K            : Integer;
begin
  C := Cmplx(0.5, 0.5);  { 0.5 + 0.5*i }
  C1 := CInv(C);
  
  Write('z':14);
  case Index of
    1 : WriteLn('     Ln(z)                  Exp(Ln(z))   ':62);
    2 : WriteLn('   ArcSin(z)              Sin(ArcSin(z)) ':62);
    3 : WriteLn('   ArcCos(z)              Cos(ArcCos(z)) ':62);
    4 : WriteLn('   ArcTan(z)              Tan(ArcTan(z)) ':62);
    5 : WriteLn('   ArcSinh(z)            Sinh(ArcSinh(z))':62);
    6 : WriteLn('   ArcCosh(z)            Cosh(ArcCosh(z))':62);
    7 : WriteLn('   ArcTanh(z)            Tanh(ArcTanh(z))':62);
    8 : WriteLn('z^c, c=0.5+0.5*i           (z^c)^(1/c)   ':62);
    9 : WriteLn('   Ln(Gamma(z))              Gamma(z)    ':62); 
  end;
  
  WriteLn('  -------------------------   -----------------------   ',
          '-----------------------');
  
  for K := 1 to 20 do
    begin
      Write(K:2, CompStr(A[K]));
      
      if ((Index = 1) and (K = 1))             { Log(0) }
      or ((Index = 4) and (K in [8, 12]))      { ArcTan(+/- i) }
      or ((Index = 7) and (K in [6, 10]))      { ArcTanh(+/- 1) }
      or ((Index = 9) and (K in [1, 10, 18]))  { LnGamma(integer <= 0) }
      then
        WriteLn('undefined':20)
      else
        begin
          case Index of
            1 : begin
                  Z := CLn(A[K]);
                  Z1 := CExp(Z);
                end; 
            2 : begin
                  Z := CArcSin(A[K]);
                  Z1 := CSin(Z);
                end;
            3 : begin
                  Z := CArcCos(A[K]);
                  Z1 := CCos(Z);
                end;
            4 : begin
                  Z := CArcTan(A[K]);
                  Z1 := CTan(Z);
                end;
            5 : begin
                  Z := CArcSinh(A[K]);
                  Z1 := CSinh(Z);
                end;
            6 : begin
                  Z := CArcCosh(A[K]);
                  Z1 := CCosh(Z);
                end;
            7 : begin
                  Z := CArcTanh(A[K]);
                  Z1 := CTanh(Z);
                end; 
            8 : begin
                  Z := CPower(A[K], C);  { z = a^c }
                  Z1 := CPower(Z, C1);   { z1 = z^(1/c) = a }
                end;
            9 : begin
                  Z := CLnGamma(A[K]);
                  Z1 := CExp(Z);
                end;  
            end;
          WriteLn(' ', CompStr(Z), ' ', CompStr(Z1))
        end;
     end;
  Pause;
end;

procedure TestRoot;
{ Computes the 3 cubic roots of -1+i }
var
  A, Z, Z1 : Complex;
  K : Integer;
begin
  A := Cmplx(-1.0, 1.0);
  WriteLn('The 3 cube roots of (-1+i)', #10);
  WriteLn('z':13, 'z^(1/3)                  [z^(1/3)]^3  ':63);
  WriteLn('     ----------------------    ---------------------- ',
          '   ----------------------');
  for K := 0 to 2 do
    begin
      Z := CRoot(A, K, 3);
      Z1 := CIntPower(Z, 3);
      WriteLn(K:2, CompStr(A), ' ', CompStr(Z), ' ', CompStr(Z1));
    end;
  Pause;
end;

begin
  A[ 1] := Cmplx( 0.0,  0.0);
  A[ 2] := Cmplx( 0.5,  0.5);
  A[ 3] := Cmplx(-0.5,  0.5);
  A[ 4] := Cmplx(-0.5, -0.5);
  A[ 5] := Cmplx( 0.5, -0.5);
  A[ 6] := Cmplx( 1.0,  0.0);
  A[ 7] := Cmplx( 1.0,  1.0);
  A[ 8] := Cmplx( 0.0,  1.0);
  A[ 9] := Cmplx(-1.0,  1.0);
  A[10] := Cmplx(-1.0,  0.0);
  A[11] := Cmplx(-1.0, -1.0);
  A[12] := Cmplx( 0.0, -1.0);
  A[13] := Cmplx( 1.0, -1.0);
  A[14] := Cmplx( 5.0,  0.0);
  A[15] := Cmplx( 5.0,  3.0);
  A[16] := Cmplx( 0.0,  3.0);
  A[17] := Cmplx(-5.0,  3.0);
  A[18] := Cmplx(-5.0,  0.0);
  A[19] := Cmplx( 5.0, -3.0);
  A[20] := Cmplx( 0.0, -3.0);
  
  SetFormat(9, 3, False, True);

  TestConv;
  TestArith; 
  for I := 1 to 9 do
    TestFunc(I); 
  TestRoot;
end.
