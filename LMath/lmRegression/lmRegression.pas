{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lmRegression;

{$warn 5023 off : no warning about unused units}
interface

uses
  uevalfit, uexlfit, uexpfit, ufracfit, ugamfit, uiexpfit, ulinfit, ulogifit, umulfit, unlfit, upolfit, upowfit, 
  usvdfit, uSpline, uregtest, uConstrNLFit, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('lmRegression', @Register);
end.
