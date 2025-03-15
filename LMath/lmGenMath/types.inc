{  Floating point type (Default = Double) }

type
PFloat = ^Float;
{$IFDEF SINGLEREAL}
  Float = Single;
{$ELSE}
{$IFDEF EXTENDEDREAL}
  Float = Extended;
{$ELSE}
  {$DEFINE DOUBLEREAL}
  Float = Double;
{$ENDIF}
{$ENDIF}


{  Mathematical constants } 
const
  Pi         = 3.14159265358979323846;  {< Pi }
  Euler      = 2.71828182845904523536;  {< Euler's number (logarithm base)}
  Ln2        = 0.69314718055994530942;  {< Ln(2) }
  Ln10       = 2.30258509299404568402;  {< Ln(10) }
  LnPi       = 1.14472988584940017414;  {< Ln(Pi) }
  InvLn2     = 1.44269504088896340736;  {< 1/Ln(2) }
  InvLn10    = 0.43429448190325182765;  {< 1/Ln(10) }
  TwoPi      = 6.28318530717958647693;  {< 2*Pi }
  PiDiv2     = 1.57079632679489661923;  {< Pi/2 }
  SqrtPi     = 1.77245385090551602730;  {< Sqrt(Pi) }
  Sqrt2Pi    = 2.50662827463100050242;  {< Sqrt(2*Pi) }
  InvSqrt2Pi = 0.39894228040143267794;  {< 1/Sqrt(2*Pi) }
  LnSqrt2Pi  = 0.91893853320467274178;  {< Ln(Sqrt(2*Pi)) }
  Ln2PiDiv2  = 0.91893853320467274178;  {< Ln(2*Pi)/2 }
  Sqrt2      = 1.41421356237309504880;  {< Sqrt(2) }
  Sqrt2Div2  = 0.70710678118654752440;  {< Sqrt(2)/2 }
  Gold       = 1.61803398874989484821;  {< Golden Mean = (1 + Sqrt(5))/2 }
  CGold      = 0.38196601125010515179;  {< 2 - GOLD }


{  Machine-dependent constants }
{$IFDEF SINGLEREAL}
const
  MachEp   = 1.192093E-7;   {< Floating point precision: 2^(-23) }
  MaxNum   = 3.402823E+38;  {< Max. floating point number: 2^128 }
  MinNum   = 1.175495E-38;  {< Min. floating point number: 2^(-126) }
  MaxLog   = 88.72283;      {< Max. argument for Exp = Ln(MaxNum) }
  MinLog   = -87.33655;     {< Min. argument for Exp = Ln(MinNum) }
  MaxFac   = 33;            {< Max. argument for Factorial }
  MaxGam   = 34.648;        {< Max. argument for Gamma }
  MaxLgm   = 1.0383E+36;    {< Max. argument for LnGamma }

  {$ENDIF}

{$IFDEF DOUBLEREAL}
const
  MachEp   = 2.220446049250313E-16;   {< 2^(-52) }
  MaxNum   = 1.797693134862315E+308;  {< 2^1024 }
  MinNum   = 2.225073858507202E-308;  {< 2^(-1022) }
  MaxLog   = 709.7827128933840;
  MinLog   = -708.3964185322641;
  MaxFac   = 170;
  MaxGam   = 171.624376956302;
  MaxLgm   = 2.556348E+305;

{$ENDIF}

{$IFDEF EXTENDEDREAL}
const
  MachEp   = 1.08420217248550444E-19;      {< 2^(-63) }
  MaxNum   = 5.9486574767861588254E+4931;  {< 2^16383 }
  MinNum   = 6.7242062862241870125E-4932;  {< 2^(-16381) }
  MaxLog   = 11355.830259113584004;
  MinLog   = -11354.443964752464114;

  MaxFac   = 1754;
  MaxGam   = 1755.455;
  MaxLgm   = 1.04848146839019521E+4928;
{$ENDIF}

{$push}
{$R-}
{$Q-}
       NaN = 0.0/0.0;
       Infinity = 1.0/0.0;
       NegInfinity = -1.0/0.0;
{$pop}

type Complex = record
  X, Y : Float;
end;

PRealPoint = ^TRealPoint;
TRealPoint = record
  X: Float;
  Y: Float;
end;

TRealPointVector =  array of TRealPoint;
PIntegerPoint = ^TIntegerPoint;
TIntegerPoint = record
  X, Y : integer;
end;

const
  C_infinity : Complex = (X : MaxNum; Y : 0.0);
  C_zero     : Complex = (X : 0.0;    Y : 0.0);
  C_one      : Complex = (X : 1.0;    Y : 0.0);
  C_i        : Complex = (X : 0.0;    Y : 1.0);
  C_pi       : Complex = (X : Pi;     Y : 0.0);
  C_pi_div_2 : Complex = (X : PiDiv2; Y : 0.0);

{  Vectors and matrices. }
const                    { Max. array size }

  {$IFDef CPU32}
  MaxSize = 2147483647;  {< 2^31 - 1 }
  {$ELSE IF CPU64}
  MaxSize = 9223372036854775807;
  {$ENDIF}
  MaxVecLength  = MaxSize div SizeOf(Float);
  MaxIntLength  = MaxSize div SizeOf(Integer);
  MaxCompLength = MaxSize div SizeOf(Complex);
  MaxBoolLength = MaxSize div SizeOf(Boolean);


type
  TVector     = array of Float;
  TIntVector  = array of Integer;
  TCompVector = array of Complex;
  TBoolVector = array of Boolean;
  TStrVector  = array of String;

  TMatrix     = array of TVector;
  TIntMatrix  = array of TIntVector;
  TCompMatrix = array of TCompVector;
  TBoolMatrix = array of TBoolVector;
  TStrMatrix  = array of TStrVector;
  
  // Types of comparisons, used in some comparators
  TCompOperator = (LT,LE,EQ,GE,GT,NE);

{ ------------------------------------------------------------------
  Functional types
  ------------------------------------------------------------------ }

{ Function of one variable }
TFunc         = function(X : Float) : Float;
TIntFunc      = function(X:integer) : integer;
TTestFunc     = function(X:Float):boolean;
TIntTestFunc  = function(X:Integer):boolean;
TIntFloatFunc = function(X:integer):float;

{ Function of several variables }
TFuncNVar = function(X : TVector) : Float;

{ Nonlinear equation system }
TEquations = procedure(X, F : TVector);

{ Differential equation system }
TDiffEqs = procedure(X : Float; Y, Yp : TVector);

{ Jacobian }
TJacobian = procedure(X : TVector; D : TMatrix);

{ Gradient }
TGradient = procedure(X, G : TVector);

{ Hessian and Gradient }
THessGrad = procedure(X, G : TVector; H : TMatrix);

TParamFunc = function(X:Float; Params:Pointer):Float;

// type for general test and comparison functions (Val, Ref):boolean;
TComparator = function(Val,Ref:float):boolean;
TIntComparator = function(Val,Ref:integer):boolean;

{ Random number generators }
RNG_Type =
  (RNG_MWC,      {< Multiply-With-Carry }
   RNG_MT,       {< Mersenne Twister }
   RNG_UVAG);    {< Universal Virtual Array Generator }

{ Statistics. Used by Distrib in uDistrib which essentially builds a histogram }
StatClass = record  {< Statistical class }
  Inf : Float;           {< Lower bound }
  Sup : Float;           {< Upper bound }
  N   : Integer;         {< Number of values }
  F   : Float;           {< Frequency }
  D   : Float;           {< Density }
end;

 TStatClassVector = array of StatClass;

{ Curve fit }
  TRegMode = (OLS, WLS);  {< Regression mode }

  TRegTest = record      {< Test of regression }
    Vr       : Float;    {< Residual variance }
    R2       : Float;    {< Coefficient of determination }
    R2a      : Float;    {< Adjusted coeff. of determination }
    F        : Float;    {< Variance ratio (explained/residual) }
    Nu1, Nu2 : Integer;  {< Degrees of freedom }
  end;

{ Optimization algorithms for nonlinear regression }
  TOptAlgo = (
    NL_MARQ,       {< Marquardt algorithm }
    NL_SIMP,       {< Simplex algorithm }
    NL_BFGS,       {< BFGS algorithm }
    NL_SA,         {< Simulated annealing }
    NL_GA);        {< Genetic algorithm }

{ Regression function }
  TRegFunc = function(X : Float; B : TVector) : Float;

{ Procedure to compute the derivatives of the regression function
  with respect to the regression parameters }
  TDerivProc = procedure(X, Y : Float; B, D : TVector);

{ Variable of the integrated Michaelis equation:
  Time, Substrate conc., Enzyme conc. }
  TMintVar = (Var_T, Var_S, Var_E);

  {User function supplied to COBYLA minimization algorythm.
  N is number of arguments to be adjusted;
  M is number of constraints;
  TVector X[N] is the current vector of variables.
  TVector Con[M+2] is vector of constraint values. The subroutine should return
  the objective and constraint functions at X in F and CON[1],CON[2],
  ...,CON[M]. Con[M+1] and Con[M+2] are used internally. Note that we are trying to
  adjust X so that F(X) is as small as possible subject to the constraint functions
  being nonnegative. Importantly, constraints can be violated during the calculation!}
  TCobylaObjectProc = procedure (N, M : integer; const X : TVector;
                                out F:Float; CON: TVector);
  { ------------------------------------------------------------------
  Graphics
  ------------------------------------------------------------------ }

  Str30  = String[30];
  TScale = (LinScale, LogScale);
  TGrid  = (NoGrid, HorizGrid, VertiGrid, BothGrid);
  
{ ------------------------------------------------------------------
  Math parser
  ------------------------------------------------------------------ }

const
  MaxArg = 26;  {< Max number of arguments for a function }

type
  TArgC = 1..MaxArg;

  TWrapper = function(ArgC : TArgC; ArgV : TVector) : Float;
