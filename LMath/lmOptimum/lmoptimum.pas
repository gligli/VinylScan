{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lmoptimum;

{$warn 5023 off : no warning about unused units}
interface

uses
  ubfgs, ugenalg, ugoldsrc, ulinmin, ulinminq, umarq, umcmc, uminbrak, 
  unewton, usimann, usimplex, uCobyla, uTrsTlp, ueval, uLinSimplex, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('lmoptimum', @Register);
end.
