unit uConstrNLFit;
{$mode objfpc}
interface
uses uTypes, uCobyla;
type
  // Calculates constraint functions and puts results of calculation into Con
  // B is vector of model parameters as in ConstrNLFit
  // Constraint calculation results are placed from Con[1] and ending with Con[LastCon].
  // At the end they must be nonnegative.
  // Con[0] is not used. Fortran inheritance. Con is allocated by Cobyla.
  TConstrainsProc = procedure(MaxCon: integer; B, Con : TVector);

procedure ConstrNLFit(RegFunc : TRegFunc; // function to be fitted
                  ConstProc   : TConstrainsProc; // calculation of constrains
                  X, Y        : TVector; // data to be fitted
                  Lb, Ub      : Integer; // bounds of X and Y arrays
                  var MaxFun  : Integer; // maximal number of calls to objective function
                  var Tol     : Float;   // tolerance of fit (RhoEnd)
                  B           : TVector; // vector of parameters. Guesses in input, fitted values on output
                  LastPar     : Integer; // number of parameters in B. First parameter in b[1], last in B[LastParam]
                  LastCon     : Integer; // number of constraints
                  out MaxCV   : float // maximal constraint violation
                    );

function GetCFResiduals: TVector;

function GetCFFittedData: TVector;

var
  RhoBeg : float = 1.0;

implementation
const
{$IFDEF SINGLEREAL}
  MAX_FUNC  = 1.0E+30;
{$ELSE}
{$IFDEF EXTENDEDREAL}
  MAX_FUNC  = 1.0E+4000;
  {$ELSE}
    {$DEFINE DOUBLEREAL}
  MAX_FUNC  = 1.0E+280;
{$ENDIF}
{$ENDIF}

var
  XArr, YArr, YCArr, Resid : TVector;
  ArrLb, ArrUb : integer;
  gRegFunc : TRegFunc;
  gConProc : TConstrainsProc;

procedure CobylaObjectProc(N, M : integer; const B : TVector;
                           out F:Float; CON: TVector);
var
    I : integer;
begin
  F := 0.0;
  for I := ArrLb to ArrUb do
  begin
    YCarr[I] := gRegFunc(XArr[I], B);
    Resid[I] := YArr[I] - YCarr[I];
    F := F + Sqr(Resid[I]);
    if F > MAX_FUNC then
    begin
      F := MAX_FUNC;
      Break;
    end;
  end;
  gConProc(M, B, Con);
end;

procedure ConstrNLFit(RegFunc : TRegFunc; ConstProc : TConstrainsProc; X, Y : TVector;
                      Lb, Ub : Integer; var MaxFun : Integer; var Tol : Float; B : TVector;
                      LastPar : Integer; LastCon : Integer; out MaxCV : float);
var
  F:Float;
begin
  XArr := X; YArr := Y;
  DimVector(Resid, Ub);
  DimVector(YCarr, Ub);
  gRegFunc := RegFunc;
  gConProc := ConstProc;
  ArrLb := Lb; ArrUb := Ub;
  Cobyla(LastPar, LastCon, B, F, MaxCV, RhoBeg, Tol, MaxFun, @CobylaObjectProc);
end;

function GetCFResiduals: TVector;
begin
  Result := Resid;
end;

function GetCFFittedData: TVector;
begin
  Result := YCArr;
end;

end.

