unit ugoldman;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uTypes, unlFit;

var
  CinKnown : boolean;

{P is permeability constant. Classically, current density (Amp/m^2) is calculated and P is m/s. If we are interested
in absolute value of current and not density, P is m^3/s.
z is ion charge (-1 for Cl-; 2 for Ca++ etc). It is float since for non-selective channels apparent valence of
permeated ion may be non-integer.
Cin is intracellular concentration of ion, Mol/m^3 or mM/l; Cout is extracellular concentration.
Vm is transmembrane voltage, mVolts.
TC is temperature, grad Celsii}
function GHK(P, z, Cin, Cout, Vm, TC: float):float; // Goldman-Hodgkin-Katz equation
procedure FitGHK(CinFixed : boolean; az, aCout, aTC : float; var Cin, P : float; Voltages, Currents : TVector; Lb, Ub:integer);
function GOutMax(P,TC,Cin,Cout,z:float):float; // finds maximal (limiting) outward conductance with defined GHK
function GInMax(P,TC,Cin,Cout,z:float):float; // finds maximal (limiting) inward conductance with defined GHK
function ERev(CIn, COut, z, TC:float):float; // find reverse potential by Nernst equation, mV. TC : tempreature Celsii
function Intracellular(Cout, z, TC, ERev:float):float; // find intracellular concentration from Cout, valence, temperature (grad C), ERev (mV)
function GSlope(Cin,Cout,z,TC,Vm,P:float):float; // slope conductance at any point
function PfromSlope(dI,dV,z,C,TC:float):float; // permeability from linear IV

implementation
const
  F   = 96485.0; //Faraday constant, Q/mol
  R   = 8.3145;
  TK  = 273;
  FR  = F/R;
  F2R = FR*F;

var
  RTF, RTF2, T : float;

procedure SetConstants(const TC: float);
begin
  T := TK+TC;
  RTF := FR/T/1000; // 1000 because Vm in mV
  RTF2 := F2R/T/1000;
end;

function ERev(CIn,Cout,z,TC:float):float;
begin
  Result := -R*(TK+TC)*1000/F*z*ln(Cin/Cout);
end;

function GHK(P, z, Cin, Cout, Vm, TC: float):float;
var
  Ex : float;
begin
  if IsZero(z) then
    Result := 0 // neutral molecules do not carry current
  else begin
    SetConstants(TC);
    if IsZero(Vm) then
      Result := P*z*F*(Cin-Cout)
    else begin
      Ex := exp(-z*RTF*Vm);
      Result := P*z*z*RTF2*Vm*(Cin-Cout*Ex)/(1.0-Ex);
    end;
  end;
end;

function GOutMax(P,TC,Cin,Cout,z:float):float;
var
  C:float;
begin
  if z > 0 then
    C := Cin
  else
    C := Cout;
  SetConstants(TC);
  Result := RTF2*z*z*P*C*1000; // *1000 because here we need Volts and not mVolts
end;

function GInMax(P,TC,Cin,Cout,z:float):float;
var
  C:float;
begin
  if z < 0 then
    C := Cin
  else
    C := Cout;
  SetConstants(TC);
  Result := RTF2*z*z*P*C*1000; // *1000 because here we need Volts and not mVolts
end;

function PfromSlope(dI,dV,z,C,TC:float):float;
var
  RT:float;
begin
  RT := R*(TK+TC);
  Result := RT/F*dI/F/z/z/C/dV;
end;

var
  iz, iCin, iCout, iTC : float;

function Intracellular(Cout, z, TC, ERev: float): float;
begin
  Result := Cout/exp(z*FR/(TK+TC)*ERev/1000);
end;

function GSlope(Cin,Cout,z,TC,Vm,P:float):float;
var
 K1,ex,RT:float;
begin
  SetConstants(TC);
  RT := R*T;
  if IsZero(Vm) then
    Vm := 0.01;
  ex := exp(-z*Vm*RTF); // here RTF corrected for mV is used
  K1 := -F*F*z*z*P/(RT*RT*sqr(ex-1));
  Result := K1*(-(ex*ex)*RT*Cout + ((Cin + Cout)*RT + F*z*Vm*0.001*(Cin - Cout))*ex - Cin*RT);
end;

function fP_GHK(Vm:float; Params:TVector):float; // this function is used when Cin is fixed and only P is fitted
begin
  Result := GHK(Params[1],iz,iCin,iCout,Vm,iTC);
end;

function fPC_GHK(Vm:float; Params:TVector):float; // fitting both P and Cin
begin
  Result := GHK(Params[1],iz,Params[2],iCout,Vm,iTC);
end;

function GHKdP(z, Cin, Cout, Vm, TC: float):float;
begin
  if IsZero(z) then
    Result := 0 // neutral molecules do not carry current
  else begin
    SetConstants(TC);
    if IsZero(Vm) then Vm := 1E-6;
    Result := RTF2*z*Vm*(Cout-Cin*exp(-RTF*z*Vm))/(1.0-exp(-RTF*z*Vm));
  end;
end;

function GHKdCin(P, z, Vm, TC: float):float;
var
  Ex:float;
begin
  if IsZero(z) then
    Result := 0 // neutral molecules do not carry current
  else begin
    SetConstants(TC);
    if IsZero(Vm) then Vm := 1E-6;
    Ex := exp(-RTF*z*Vm);
    Result := -RTF2*P*z*Vm*ex/(1.0-ex);
  end;
end;

procedure GoldmanDerivProc_P(X,Y:float; Params, Derivs:TVector);
begin
  Derivs[1] := GHKdP(iz, iCin, iCout, X, iTC);
end;

procedure GoldmanDerivProc_PC(X,Y:float; Params, Derivs:TVector);
begin
  Derivs[1] := GHKdP(iz, Params[2], iCout, X, iTC);
  Derivs[2] := GHKdCin(Params[1],iz,X,iTC);
end;

procedure FitGHK(CinFixed : boolean; az, aCout, aTC : float; var Cin, P : float; Voltages, Currents : TVector; Lb, Ub:integer);
var
  MyTol:float;
  ExpParams:TVector;
  HESS:TMatrix;
begin
  SetOptAlgo(NL_SIMP);
  iz := az; iCout := aCout; iTC := aTC;
  P := PFromSlope(abs(Currents[Ub]-Currents[Lb]),abs(Voltages[Ub]-Voltages[Lb]),iz,Cin,iTC);
  MyTol := P/10000;
  if CinFixed then
  begin
    iCin := Cin;
    DimVector(ExpParams,1);
    DimMatrix(HESS, 1, 1);
  end else
  begin
    DimVector(ExpParams,2);
    DimMatrix(HESS, 2, 2);
  end;
  SetParamBounds(1,0,MaxNum/10);
  ExpParams[1] := 1E-6;
  if not CinFixed then
  begin
    SetParamBounds(2,0,55000);
    ExpParams[2] := Cin;
    NLFit(@fPC_GHK, @GoldmanDerivProc_PC, Voltages, Currents, Lb, Ub, 400, MyTol, ExpParams, 1, 2, HESS);
    Cin := ExpParams[2];
  end else
    NLFit(@fP_GHK, @GoldmanDerivProc_P, Voltages, Currents, Lb, Ub, 400, MyTol, ExpParams, 1, 1, HESS);
  P := ExpParams[1];
end;

end.
