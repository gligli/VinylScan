{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lmmathstat;

{$warn 5023 off : no warning about unused units}
interface

uses
  uanova1, uanova2, ubartlet, ubinom, ucorrel, udistrib, uexpdist, ugamdist, uibtdist, uigmdist, uinvbeta, uinvgam, 
  uinvnorm, ukhi2, umeansd, umeansd_md, umedian, unonpar, unormal, upca, upoidist, uskew, usnedeco, ustdpair, 
  ustudind, uwoolf, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('lmmathstat', @Register);
end.
