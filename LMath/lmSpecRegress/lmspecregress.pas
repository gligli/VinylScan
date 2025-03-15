{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lmspecregress;

{$warn 5023 off : no warning about unused units}
interface

uses
  ugoldman, umodels, udistribs, ugauss, ugaussf, usigmatable, uhillfit, umichfit, umintfit, upkfit, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('lmspecregress', @Register);
end.
