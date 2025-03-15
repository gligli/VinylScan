{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lmpolynoms;

{$warn 5023 off : no warning about unused units}
interface

uses
  upolutil, upolynom, urootpol, urtpol1, urtpol2, urtpol3, urtpol4, ucrtptpol, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('lmpolynoms', @Register);
end.
