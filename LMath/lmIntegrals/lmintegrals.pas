{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lmintegrals;

{$warn 5023 off : no warning about unused units}
interface

uses
  ugausleg, urkf, utrapint, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('lmintegrals', @Register);
end.
