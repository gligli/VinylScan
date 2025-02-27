{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lmgenmath;

{$warn 5023 off : no warning about unused units}
interface

uses
  ubeta, ucomplex, udigamma, ufact, ugamma, uhyper, uibeta, uigamma, uScaling, 
  ulambert, umath, uminmax, upolev, uround, utrigo, utypes, uErrors, 
  uIntervals, uRealPoints, uIntPoints, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('lmgenmath', @Register);
end.
