{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lmDSP;

{$warn 5023 off : no warning about unused units}
interface

uses
  uConvolutions, uFilters, uDFT, ufft, uFindFilterCoeffs, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('lmDSP', @Register);
end.
