{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lmrandoms;

{$warn 5023 off : no warning about unused units}
interface

uses
  urandom, urangaus, uranmt, uranmult, uranmwc, uranuvag, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('lmrandoms', @Register);
end.
