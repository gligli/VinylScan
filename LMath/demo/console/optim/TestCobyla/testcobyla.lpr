Program testcobyla;
{$mode delphi}
Uses uTypes, uMath, uErrors, SysUtils, TestFunc, uCobyla, uVecMatPrn, uVectorHelper, uStrings;
const
 OutputFmt = 'NFVALS = %6.4d F = %13.6g  MaxCV = %13.6g';

var
  X: TVector;
  XOPT: TVector;
  N, M, I : integer;
  ICase, MaxFun: integer;
  Temp, TempA, TempB, TempC, TempD : float;
  RhoBeg, RhoEnd : Float;
  F, MaxCV: Float; // value of function after minimization
  OutMatrix : TMatrix;
begin
  DimVector(X, 10);
  DimVector(XOpt,10);
  for NPROB := 1 to 10 do    // NProb is defined in TestFunc unit
  begin
    Case NProb of
    1: begin                // Minimization of a simple quadratic function of two variables.
          writeln('Output from test problem 1 (Simple quadratic)');
          N := 2;
          M := 0;
          XOPT[1] := -1.0;
          XOPT[2] := 0.0;
        end;
    2:  begin         // Easy two dimensional minimization in unit circle.
          writeln('Output from test problem 2 (2D unit circle calculation)');
          N := 2;
          M := 1;
          XOPT[1] := SQRT(0.5);
          XOPT[2] := -XOPT[1];
        end;
    3:  begin // Easy three dimensional minimization in ellipsoid.
          writeln('Output from test problem 3 (3D ellipsoid calculation)');
          N := 3;
          M := 1;
          XOPT[1] := 1.0/SQRT(3.0);
          XOPT[2] := 1.0/SQRT(6.0);
          XOPT[3] := -1.0/3.0;
        end;
    4:  begin // Weak version of Rosenbrocks problem.
          writeln('Output from test problem 4 (Weak Rosenbrock)');
          N := 2;
          M := 0;
          XOPT[1] := -1.0;
          XOPT[2] := 1.0;
        end;
    5:  begin // Intermediate version of Rosenbrock's problem.
          writeln('Output from test problem 5 (Intermediate Rosenbrock)');
          N := 2;
          M := 0;
          XOPT[1] := -1.0;
          XOPT[2] := 1.0;
        end;
    6:  begin
          writeln('Output from test problem 6 (Equation (9.1.15) in Fletcher)');
          N := 2;              // This problem is taken from Fletchers book Practical Methods of
          M := 2;              // Optimization and has the equation number (9.1.15).
          XOPT[1] := SQRT(0.5);
          XOPT[2] := XOPT[1];
        end;
    7:  begin
          writeln('Output from test problem 7 (Equation (14.4.2) in Fletcher)');
          N := 3;
          M := 3;             // This problem is taken from Fletchers book Practical Methods of
          XOPT[1] := 0.0;     // Optimization and has the equation number (14.4.2).
          XOPT[2] := -3.0;
          XOPT[3] := -3.0;
        end;
    8:  begin
          writeln('Output from test problem 8 (Rosen-Suzuki)');
          N := 4;
          M := 3;
          XOPT[1] := 0.0;   //  This problem is taken from page 66 of Hock and Schittkowski's book Test
          XOPT[2] := 1.0;   //  Examples for Nonlinear Programming Codes. It is their test problem Number
          XOPT[3] := 2.0;   //  43, and has the name Rosen-Suzuki.
          XOPT[4] := -1.0;
        end;
    9:  begin
          writeln( 'Output from test problem 9 (Hock and Schittkowski 100)');
          M := 4;
          N := 7;
          XOPT[1] := 2.330499;      //   This problem is taken from page 111 of Hock and Schittkowski's
          XOPT[2] := 1.951372;      //   book Test Examples for Nonlinear Programming Codes. It is their
          XOPT[3] := -0.4775414;    //   test problem Number 100.
          XOPT[4] := 4.365726;
          XOPT[5] := -0.624487;
          XOPT[6] := 1.038131;
          XOPT[7] := 1.594227;
        end;
    10: begin
          writeln( 'Output from test problem 10 (Hexagon area)');
          N := 9;         // This problem is taken from page 415 of Luenberger's book Applied
          M := 14;        // Nonlinear Programming. It is to maximize the area of a hexagon of
          TEMPA := X[1]+X[3]+X[5]+X[7];
          TEMPB := X[2]+X[4]+X[6]+X[8];
          TEMPC := 0.5/SQRT(TEMPA*TEMPA+TEMPB*TEMPB);
          TEMPD := TEMPC*SQRT(3.0);
          XOPT[1] := TEMPD*TEMPA+TEMPC*TEMPB;
          XOPT[2] := TEMPD*TEMPB-TEMPC*TEMPA;
          XOPT[3] := TEMPD*TEMPA-TEMPC*TEMPB;
          XOPT[4] := TEMPD*TEMPB+TEMPC*TEMPA;
          for I := 1 TO 4 do
            XOPT[I+4] := XOPT[I];
        end;            // unit diameter.
    END; //case
    for  ICASE := 1 to 2 do
    begin
      for I := 1 to N do
          X[I] := 1.0; // filling X with guess values
      RHOBEG := 0.5;
      case ICase of
        1: RHOEND := 0.0001;
        2: RHOEND := 0.00001;
      end;
      MAXFUN := 2000;
      COBYLA(N,M,X,F,MaxCV,RHOBEG,RHOEND,MAXFUN,@CalcFC);

      if MathErr = optOK then
        writeln('normal return from subroutine cobyla')
      else
        writeln(MathErrMessage);

      writeln(Format(OutputFmt,[MaxFun,F,MaxCV]));
      writeln('X calculated and X theoretical:');
      Finalize(outMatrix);
      DimMatrix(outMatrix,2,N);
      OutMatrix[1].InsertFrom(X,1,N,1);
      OutMatrix[2].InsertFrom(XOpt,1,N,1);
      PrintMatrix(OutMatrix);
      TEMP := 0.0;
      for I := 1 to N do
         TEMP := TEMP+(X[I]-XOPT[I])**2;
      writeln('Least squares error in variables  := ', FloatStr(SQRT(TEMP)));
    End; //Do
   writeln('----------------------------------------------');
  End; //Do
  writeln('Press [Enter] to terminate...');
  readln;
END. // program TestCOBYLA
