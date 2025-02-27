{$mode objfpc}
{
 Simplex method for linear programming.  
 Translated from Fortran 90, Numerical Recipes.
 
 Detailed description of principle and input and output parameters may be
 found in the "Numeric recipes in Fortran 77", 
 https://websites.pmc.ucsc.edu/~fnimmo/eart290c_17/NumericalRecipesinF77.pdf 
 pages 423-435
}
unit uLinSimplex;
interface
uses uTypes, uMinMax, uErrors, UVecUtils, uVectorHelper, uMatrix;
{
Input for LinSimplex:
N: number of variables x1..xN
m1: number of constraints in form: a1*x1+a2*x2*... <= bi, i in[1..m1]
m2: number of constraints in form: a1*x1+a2*x2*... >= bi, i in[m1+1..m1+m2]
m3: number of constraints in form: a1*x1+a2*x2*...  = bi, i in[m1+m2+1..m3]

A[M+2,N+1] contains Tableau. 
First line A[1] is occupied by coefficients for objective function
A[2]..A[M] coefficients for constraints multiplied by -1, in the order m1, m2, m3. That is,
"<=" constraints are in A[2] to A[m1+1]
">=" constraints     in A[m1+2] to A[m1+m2+1]
 "=" constraints     in A[m1+m2+2] to A[m1+m2+1]
Line A[m+2] is used internally for auxiliary function. 
First column A[*,1] is occupied by free members; A[1,1] is zero
Other columns are coefficients
Output: 
A is revized Tableu;  A[1,1] is objective function value.

Dimentions are M for iposv, N for izrov, with M1 + M2 + M3 = M.

iposv contains j in [1..M] is index i of x[i] which is represented by row j+1
First row is row of objective function. If value in iposv > N, the row represents 
a slack variable.

izrov[1..N] contains index i of a variable x[i] represented by column 1 to I.
All these X are "0" in the solution. i > N is a slack variable.

icase = 0: finite solution found; icase = 1: objective function is unbounded;
icase = -1: no solution exists.

For LinProgSolve, A and iCase is the same, but do not change sign of constrain coefficients!
FuncVal is function value and SolVector is vector of X1...XN
}

procedure LinProgSolve(var A : TMatrix; N, M1, M2, M3 : integer;
      out iCase: integer; out FuncVal : float; out SolVector :TVector);

procedure LinSimplex(
      var  A    : TMatrix; // 
       N,   // number of variables
       M1,  // number of constraints in form a1*x1+a2*x2*... <= bi, i in[1..m1]
       M2,  // number of constraints in form a1*x1+a2*x2*... >= bi, i in[1..m1]
       M3 : integer; // number of constraints in form a1*x1+a2*x2*... = bi, i in[1..m1]
      out icase : integer; 
      out izrov, iposv : TIntVector //iposv[1..M] izrov[1..N]
    );
 
implementation

procedure LinProgSolve(var A : TMatrix; N, M1, M2, M3 : integer;
      out iCase: integer; out FuncVal : float; out SolVector :TVector);
var
  I,M:integer;
  izrov,iposv: TIntVector;
begin
  M := M1+M2+M3;
  A := MatFloatMul(A,-1,2,M+1,N+1,A);
  LinSimplex(A,N,M1,M2,M3,icase,izrov,iposv);
  FuncVal := A[1,1];
  DimVector(SolVector,N);
  for I := 1 to N do
    if IzRov[I] in [1..N] then
      SolVector[IzRov[I]] := 0;
  for I := 1 to M do
    if IPosV[I] in [1..N] then
      SolVector[IPosV[I]] := A[I+1,1];
end;

var
  ip, kp:integer;
  M :integer;
      // Locate a pivot element, taking degeneracy into account.
procedure simp1(var A:TMatrix; M,N:integer);
var
  i,k:integer;
  q,q0,q1,qp:float;
begin
  ip := 0;
  i := FirstElement(A,2,M+1,kp+1,kp+1,@IsNegative).Row; // kp+1 is index of "pivot column"
  if I > M+1 then Exit;   // all elements positive; function is unbound
  q1 := -A[i,1]/A[i,kp+1];
  ip := i-1;
  for i := ip+1 to M do
    if IsNegative(A[i+1,kp+1]) then
    begin
      q := -A[i+1,1]/A[i+1,kp+1];
      if (q < q1) then  //this is more restrictive element
      begin
        ip := i;   // A[ip+1,kp+1] is pivot element
        q1 := q;
      end else if (q = q1) then
      begin// We have a degeneracy.
        for k := 1 to N do
        begin
          qp := -A[ip+1,k+1]/A[ip+1,kp+1];
          q0 := -A[i+1,k+1]/A[i+1,kp+1];
          if (q0 <> qp) then break;
        end; // for
        if (q0 < qp) then ip:=i;
      end; // if
    end; // if
end; // procedure simp1

procedure simp2(var A:TMatrix; i1,k1:integer);
var
  Piv:float;
  ii,kk:integer;
begin
  Piv := 1.0/A[ip+1,kp+1];
  for ii := 1 to i1+1 do
    if ii <> ip+1 then
    begin
      A[ii,kp+1] := a[ii,kp+1]*piv; // pivot column divided by pivot element
      for kk := 1 to k1+1 do
        if kk <> kp+1 then
          A[ii,kk] := A[ii,kk] - A[ip+1,kk]*A[ii,kp+1]; // everything but pivot column and row
    end;                                                // replaced
  for kk := 1 to k1+1 do
    if kk <> kp+1 then
      A[ip+1,kk] := A[ip+1,kk]*(-piv); // pivot row divided by negative pivot (which is positive number)
  A[ip+1,kp+1] := Piv; // pivot element replaced by its reciprocal
end;

procedure LinSimplex(
      var  A    : TMatrix;
       N, M1,M2,M3 : integer; 
      out icase : integer; 
      out izrov, iposv : TIntVector
    );

var
  k,kh,nl1 : integer;
  L1 : TIntVector;
  L3 : TIntVector; // < list of M2 constraints whose slack variables have never been exchanged
                   // out of the initial basis.
  bmax : float;

  procedure FindMax(MM:integer; iabf:boolean);
  // Determines the maximum of those elements whose index is contained in the
  // supplied list l1, either with or without taking the absolute value, as flagged by iabf.
  var
    K:integer;
    test:float;
  begin
    if (nl1 <= 0) then    // No eligible columns.
      bmax := 0.0
    else begin
      kp := l1[1];
      bmax := A[MM+1,kp+1];
      for K := 2 to nl1 do
      begin
        if iabf then
          test := abs(A[MM+1, l1[k]+1]) - abs(bmax)
        else
          test := A[MM+1, l1[k]+1] - bmax;
        if IsPositive(Test) then
        begin
          bmax := a[MM+1, l1[k]+1];
          kp := l1[k];
        end;
      end;
    end;
  end;

  procedure phase1;
  label
    phase1b;
  var
    I,J:integer;
  begin
    DimVector(L3,M2);
    L3.Fill(1,M2,1);
     // <-- Initialize list of M2 constraints whose slack variables have never been exchanged out
     // of the initial basis.
    A[M+2].Fill(1,N+1,0);            // coefficients of auxiliary function as
    for I := 1 to N+1 do             // negative sum of original coefficients found
      for J := M1+2 to M+1 do
        A[M+2,I] := A[M+2,I]-A[J,I];
    while true do
    begin
      FindMax(M+1,false); // kp is position of maximum coefficient of the auxiliary objective function, bmax its value
      // Phase 1a
      if (not IsPositive(bmax)) and IsNegative(A[M+2,1]) then
      begin // Auxiliary objective function is still negative and can’t
        icase:=-1; // be improved, hence no feasible solution exists.
        exit;
      end
      else if (not IsPositive(bmax)) and (not IsPositive(A[M+2,1])) then
      begin
        // Auxiliary objective function is zero and can’t be improved. This signals that we
       // have a feasible starting vector. Clean out the artificial variables corresponding
       // to any remaining equality constraints and then eventually exit phase one.
        for ip := M1+M2+1 to M do
        begin
          if iposv[ip] = ip + N then // Found an artificial variable for an equality constraint.
          begin
            FindMax(ip,true);
            if IsPositive(bmax) then // Exchange with column corresponding
              GoTo phase1b;          //to maximum pivot element
          end; // if
        end; // for
        for I := M1+1 to M1+M2 do //  Change sign of row for any M2 constraints
          if L3[I-M1] = 1 then    //  still present from the initial basis.
            for J := 1 to N+1 do
              A[I+1,J] := -A[I+1,J];
        exit; // Go to phase two.
      end; // if
      simp1(A,M,N);    // Locate a pivot element (phase one).
      if ip = 0 then   // Maximum of auxiliary objective function is
      begin            // unbounded, so no feasible solution exists.
        icase:=-1;
        exit;
      end;
phase1b:
      simp2(A,M+1,n);
      if iposv[ip] >= n+M1+M2+1 then  // Exchange a left- and a right-hand variable.
      begin                           // Exchanged out an artificial variable for an
        k := FirstElement(l1,1,nl1,kp,EQ); // equality constraint. Make sure it stays
        nl1:=nl1-1;                   // out by removing it from the l1 list.
        for I := k to nl1 do
          l1[I] := l1[I+1];
      end else
      begin
        kh := iposv[ip]-M1-n;         // Exchanged out an M2 type constraint.
        if (kh >= 1) and (l3[kh] <> 0) then  // If it’s the first time, correct the pivot column
          l3[kh] := 0;                   // for the minus sign and the implicit
        A[M+2,kp+1] := A[M+2,kp+1]+1.0;
        for I := 1 to M+2 do
          A[I,kp+1] := -A[I,kp+1];
      end; // if                   // artificial variable.
      swap(izrov[kp],iposv[ip]);   // Update lists of left- and right-hand variables.
    end; // phase1;                // If still in phase one, go back again.
  end;

begin
  icase := 0;
  SetErrCode(mathOK);
  M := M1+M2+M3;
  if high(A) < M + 2 then
  begin
    SetErrCode(lpBadConstraintCount);
    Exit;
  end;
  if high(A[0]) < N+1 then
  begin
    SetErrCode(lpBadVariablesCount);
    Exit;
  end;
  if Any(A, 2, M+1, 1, 1, @IsNegative) then
    SetErrCode(lpBadSimplexTableau); // Constants bi must be nonnegative.
  if MathErr <> mathOK then
    Exit;                                   
  nl1 := N;                                  
  L1 := iseq(1,N,1,1);     // Initially make all variables right-hand.
  izrov := copy(l1,0,n+1); // Initialize index list of columns admissible for exchange.
  iposv := iseq(1,M,N+1,1);// iposv is list of left-hand veriables, initially slack, beginning from N+1
   // Initial left-hand variables. M1 type constraints are represented by having their slack variable
   // initially left-hand, with no artificial variable. M2 type constraints have their slack variable
   // initially left-hand, with a minus sign, and their artificial variable handled implicitly during
   // their first exchange. M3 type constraints have their artificial variable initially left-hand.
  if M2+M3 <> 0 then // Origin may not be a feasible solution. phase1 needed
      phase1;
  if icase <> 0 then
    Exit;
  while true do // phase2
  begin
    // We have an initial feasible solution. Now optimize it.
    FindMax(0,false);
    if not IsPositive(BMax) then      //Done. Solution found. Return with the good news.
    begin
      icase := 0;
      exit;
    end; // if
    simp1(A,M,N);       // Locate a pivot element (phase two).
    if (ip = 0) then    // Objective function is unbounded. Report and return.
    begin
      icase:=1;
      exit;
    end;
    simp2(A,M,N);         // Exchange a left- and a right-hand variable,
    swap(izrov[kp],iposv[ip]); // update lists of left- and right-hand variables,
  end; // do phase2;           // and return for another iteration.
end; // procedure LinSimplex

end.
