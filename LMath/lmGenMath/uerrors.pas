unit uErrors;
{ ------------------------------------------------------------------
  Error handling
   ------------------------------------------------------------------ }
{$mode objfpc}{$H+}

interface
uses uTypes;
const
  MaxErrIndex = 29;

  MathOK = 0; {< No error }
  //  Error codes for mathematical functions
  FOk        = 0;  {< No error }
  FDomain    = 1;  {< Argument domain error }
  FSing      = 2;  {< Function singularity }
  FOverflow  = 3;  {< Overflow range error }
  FUnderflow = 4;  {< Underflow range error }
  FTLoss     = 5;  {< Total loss of precision }
  FPLoss     = 6;  {< Partial loss of precision }
//  Error codes for matrix computations
  MatOk      = 0;  {< No error }
  MatNonConv = 7;  {< Non-convergence }
  MatSing    = 8;  {< Quasi-singular matrix }
  MatErrDim  = 9;  {< Incompatible dimensions }
  MatNotPD   = 10; {< Matrix not positive definite }
//  Error codes for optimization and nonlinear equations
  OptOk        = 0;  {< No error }
  OptNonConv   = 11;  {< Non-convergence }
  OptSing      = 12;  {< Quasi-singular hessian matrix }
  OptBigLambda = 13;  {< Too high Marquardt parameter }
//  Error codes for nonlinear regression
  NLMaxPar  = 14;  {< Max. number of parameters exceeded }
  NLNullPar = 15;  {< Initial parameter equal to zero }
//  Error codes for Cobyla algorithm
  cobMaxFunc = 16;
  cobRoundErrors = 17;
  cobDegenerate = 18;
// Linear Programming Errors
  lpBadConstraintCount = 19;
  lpBadSimplexTableau = 20;
  lpBadVariablesCount = 21;
// File Operation Error
  lmFileError = 22;
// DFT errors
  lmDFTError = 23;
  lmDSPFilterWinError = 24;
  lmTooHighFreqError = 25;
  lmPolesNumError = 26;
  lmFFTError = 27;
  lmFFTBadRipple = 28;
// stats errors
  lmStatEmptyData = 29;
  ErrorMessage : array[0..MaxErrIndex] of String =
    ('No error',
     'Argument domain error',
     'Function singularity',
     'Overflow range error',
     'Underflow range error',
     'Total loss of precision',
     'Partial loss of precision',
     'Non-convergence',
     'Quasi-singular matrix',
     'Incompatible matrix dimensions',
     'Matrix not positive definite',
     'Non-convergence',
     'Quasi-singular hessian matrix',
     'Too high Marquardt parameter',
     'Max. number of parameters exceeded',
     'Initial parameter equal to zero',
     'Return from subroutine Cobyla because the maxfun limit has been reached',
     'Return from procedure Cobyla because rounding errors are becoming damaging.',
     'Degenerate gradient',
     'LinSimplex:bad input constraint counts',
     'Bad input tableau in LinSimplex',
     'LinSimplex: bad variables count',
     'File input/output error',
     'Internal DFT Error',
     'Filter window is longer than data',
     'Cutoff frequency must not exceed 0.5 of sampling rate',
     'Number of poles must be even',
     'FFT: number of samples must be power of two',
     'Ripple must be 0 to 29%',
     'Empty Dataset'
    );
  { Sets the error code. }
procedure SetErrCode(ErrCode : Integer; EMessage:string = '');

{ Sets error code and default function value }
function DefaultVal(ErrCode : Integer; DefVal : Float; EMessage:string = '') : Float;

{ Returns the error code. If user supplied ErrAddr for SetErrCode, it must be used here as well }
function MathErr : Integer;

// returns error message
function MathErrMessage:string;

implementation
var
  gErrCode  : Integer = 0;
  gEMessage  : string = '';

procedure SetErrCode(ErrCode : Integer; EMessage:string = '');
begin
   gErrCode := ErrCode;
   if (EMessage = '') and (ErrCode in [0..MaxErrIndex]) then
     gEMessage := ErrorMessage[ErrCode]
   else
     gEMessage := EMessage;
end;

function DefaultVal(ErrCode : Integer; DefVal : Float; EMessage:string = '') : Float;
begin
  SetErrCode(ErrCode,EMessage);
  DefaultVal := DefVal;
end;

function MathErr : Integer;
begin
    MathErr := gErrCode
end;

function MathErrMessage:string;
begin
  Result := gEMessage;
end;

end.

