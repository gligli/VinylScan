{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lmnonlineq;

{$warn 5023 off : no warning about unused units}
interface

uses
  ubisect, ubroyden, unewteq, unewteqs, usecant, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('lmnonlineq', @Register);
end.
