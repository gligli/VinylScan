{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lmlinearalgebra;

{$warn 5023 off : no warning about unused units}
interface

uses
  ubalance, ubalbak, ucholesk, ueigsym, ueigval, ueigvec, uelmhes, ueltran, 
  ugausjor, uhqr, uhqr2, ujacobi, ulineq, ulu, uqr, usvd, uMatrix, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('lmlinearalgebra', @Register);
end.
