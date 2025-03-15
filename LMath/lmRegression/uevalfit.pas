{ ******************************************************************
  Fitting of a user-defined function
  ****************************************************************** }

unit uevalfit;

interface

uses
  utypes, ueval, unlfit;

{ Defines a regression model from ExpressionList.
  The independent variable is denoted by 'x'.
  The regression parameters are denoted by single-character symbols,
  from 'a' to 'w'.
  Example: InitEvalFit('a * exp(-k * x)') }
procedure InitEvalFit(ExpressionString : String);

 { Returns the name of the regression function (= ExpressionString) }
function FuncName : String;

{ Returns the index of the last regression parameter }
function LastParam : Integer;

{ Returns the name of the I-th regression parameter }
function ParamName(I : Integer) : String;

{ Unweighted fit of the function defined by InitEvalFit
 
  Input parameters:  X, Y    = point coordinates
                     Lb, Ub  = array bounds
                     MaxIter = max. number of iterations
                     Tol     = tolerance on parameters
  Output parameters: B       = regression parameters
                     V       = inverse matrix }
procedure EvalFit(X, Y    : TVector;
                  Lb, Ub  : Integer;
                  MaxIter : Integer;
                  Tol     : Float;
                  B       : TVector;
                  V       : TMatrix);

{ Weighted fit of the function defined by InitEvalFit
  
  Additional input parameter:
  S = standard deviations of observations }
procedure WEvalFit(X, Y, S : TVector;
                   Lb, Ub  : Integer;
                   MaxIter : Integer;
                   Tol     : Float;
                   B       : TVector;
                   V       : TMatrix);

{ Returns the value of the regression function at point X }
function EvalFit_Func(X : Float; B : TVector) : Float;

implementation
  
var
  Formula, ParamList : string;
  NParam : Integer;
 
function ExtractParams(Formula : string) : string; 
{ Extracts parameter list from formula.
  Parameters are single-letter symbols from 'a' to 'w' }
const
  AWchars = ['A'..'W', 'a'..'w'];
  AZchars = ['A'..'Z', 'a'..'z'];
var
  I : Byte;
  S : String;
begin
  S := '';
  Formula := ' ' + Formula + ' ';

  for I := 2 to Pred(Length(Formula)) do
    if (Pos(Formula[I], S) = 0)  and 
       (Formula[I]   in AWchars) and
    not(Formula[I-1] in AZchars) and
    not(Formula[I+1] in AZchars) then
      S := S + Formula[I];
 
  ExtractParams := S;
end;

procedure InitEvalFit(ExpressionString : String);
begin
  InitEval;
  Formula := ExpressionString;
  ParamList := ExtractParams(Formula);
  NParam := Length(ParamList);      
end;

function FuncName : String;
begin
  FuncName := Formula;  
end;
  
function LastParam : Integer;
begin
  LastParam := NParam;
end;

function ParamName(I : Integer) : String;
begin
  ParamName := ParamList[I];
end;

function EvalFit_Func(X : Float; B : TVector) : Float;
var
  I : Integer;
begin
  SetVariable('X', X);
  for I := 1 to NParam do
    SetVariable(ParamList[I], B[I]);
  EvalFit_Func := Eval(Formula)
end;        

procedure EvalFit_Deriv(X, Y : Float; B, D : TVector);
{ ------------------------------------------------------------------
  Computes the derivatives of the regression function at point X
  with respect to the parameters B, by numerical derivation. The
  results are returned in D. D[I] contains the derivative with
  respect to the I-th parameter.
  ------------------------------------------------------------------ }
const
  Eps = 1.0E-4;
var 
  I      : Integer;
  dB, Y1 : Float;
begin
  SetVariable('X', X);
  for I := 1 to NParam do
    SetVariable(ParamList[I], B[I]);
    
  for I := 1 to NParam do
    begin
      dB := Eps * Abs(B[I]);
      SetVariable(ParamList[I], B[I] + dB);  
      Y1 := Eval(Formula);
      D[I] := (Y1 - Y) / dB;
      SetVariable(ParamList[I], B[I]);
    end;  
end;

procedure ApproxFit(B : TVector);
{ ------------------------------------------------------------------
  Computes initial estimates of the regression parameters
  ------------------------------------------------------------------ }
var
  I : Integer;
begin
  for I := 1 to NParam do
    B[I] := 1.0;
end;
  
procedure GenEvalFit(Mode    : TRegMode;
                     X, Y, S : TVector;
                     Lb, Ub  : Integer;
                     MaxIter : Integer;
                     Tol     : Float;
                     B       : TVector;
                     V       : TMatrix);
begin
  if (GetOptAlgo in [NL_MARQ, NL_BFGS, NL_SIMP])
     and NullParam(B, 1, NParam) then
       ApproxFit(B);

  if MaxIter = 0 then Exit;

  case Mode of
    OLS : NLFit(@EvalFit_Func, @EvalFit_Deriv, X, Y, Lb, Ub,
                       MaxIter, Tol, B, 1, NParam, V);
    WLS : WNLFit(@EvalFit_Func, @EvalFit_Deriv, X, Y, S, Lb, Ub,
                       MaxIter, Tol, B, 1, NParam, V);
  end;
end;

procedure EvalFit(X, Y    : TVector;
                  Lb, Ub  : Integer;
                  MaxIter : Integer;
                  Tol     : Float;
                  B       : TVector;
                  V       : TMatrix);
begin
  GenEvalFit(OLS, X, Y, nil, Lb, Ub, MaxIter, Tol, B, V);
end;

procedure WEvalFit(X, Y, S : TVector;
                   Lb, Ub  : Integer;
                   MaxIter : Integer;
                   Tol     : Float;
                   B       : TVector;
                   V       : TMatrix);
begin
  GenEvalFit(WLS, X, Y, S, Lb, Ub, MaxIter, Tol, B, V);
end;

end.
