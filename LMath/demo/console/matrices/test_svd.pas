{ ******************************************************************
  This program solves a system of linear equations (A * X = B) with
  a single constant vector by singular value decomposition. The
  system is stored in a data file with the following structure :

    Line 1          : dimension of the matrix (N)
    Following lines : first N columns = matrix
                      last column = constant vector

  The file MATRIX3.DAT is an example data file with N = 4
  ****************************************************************** }

program test_svd;

uses
{$IFDEF USE_DLL}
  dmath;
{$ELSE}
  utypes, uErrors, usvd;
{$ENDIF}    

procedure WriteMatrix(Title : String;
                      A     : TMatrix;
                      N     : Integer);
var
  I, J : Integer;
begin
  WriteLn(Title, ' :');
  WriteLn;
  for I := 1 to N do
    begin
      for J := 1 to N do
        Write(A[I,J]:12:6);
      WriteLn;
    end;
  WriteLn;
end;

procedure WriteVector(Title : String;
                      V     : TVector;
                      N     : Integer);
var
  I : Integer;
begin
  WriteLn(Title, ' :');
  WriteLn;
  for I := 1 to N do
    WriteLn(V[I]:12:6);
  WriteLn;
end;

var
  N    : Integer;  { Matrix dimension }
  A    : TMatrix;  { System matrix }
  B    : TVector;  { Constant vector }
  S    : TVector;  { Singular values }
  U, V : TMatrix;  { Matrices from SVD }
  X    : TVector;  { Solution vector }
  F    : Text;     { Data file }
  I, J : Integer;  { Loop variables }

begin
  { Read matrix from file }
  Assign(F, 'matrix3.dat');
  Reset(F);
  Read(F, N);

  DimMatrix(A, N, N);
  DimMatrix(U, N, N);
  DimMatrix(V, N, N);
  DimVector(S, N);
  DimVector(B, N);
  DimVector(X, N);

  for I := 1 to N do
    begin
      for J := 1 to N do
        begin
          Read(F, A[I,J]);
          U[I,J] := A[I,J];
        end;
      Read(F, B[I]);
    end;

  Close(F);

  { Read and display data }
  WriteMatrix('System matrix', A, N);
  WriteVector('Constant vector', B, N);

  { Perform SV decomposition of A. If successful, solve system }

  SV_Decomp(U, 1, N, N, S, V);

  if MathErr = MatNonConv then
    begin
      WriteLn('Non-convergence!');
      Halt;
    end;

  SV_Solve(U, S, V, B, 1, N, N, X);

  WriteVector('Solution vector', X, N);

  { Compute U*S*V' which should be equal to A }
  SV_Approx(U, S, V, 1, N, N, A);
  WriteMatrix('Product U*S*V''', A, N);
  readln;
end.
