{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazFiltersPkg;

{$warn 5023 off : no warning about unused units}
interface

uses
  Filter, FilterIIR, FilterIIRHP, FilterIIRLP, FilterIIRBP, FilterIIRBS, 
  FilterIIRLPButterworth, UType, UOperator, UASConst, FilterIIRLPChebyshev, 
  FilterIIRLPBessel, FilterIIRHPButterworth, FilterIIRHPChebyshev, 
  FilterIIRHPBessel, FilterIIRBPButterworth, FilterIIRBPChebyshev, 
  FilterIIRBPBessel, FilterIIRBSButterworth, FilterIIRBSChebyshev, 
  FilterIIRBSBessel, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('FilterIIRLPButterworth', @FilterIIRLPButterworth.Register);
  RegisterUnit('FilterIIRLPChebyshev', @FilterIIRLPChebyshev.Register);
  RegisterUnit('FilterIIRLPBessel', @FilterIIRLPBessel.Register);
  RegisterUnit('FilterIIRHPButterworth', @FilterIIRHPButterworth.Register);
  RegisterUnit('FilterIIRHPChebyshev', @FilterIIRHPChebyshev.Register);
  RegisterUnit('FilterIIRHPBessel', @FilterIIRHPBessel.Register);
  RegisterUnit('FilterIIRBPButterworth', @FilterIIRBPButterworth.Register);
  RegisterUnit('FilterIIRBPChebyshev', @FilterIIRBPChebyshev.Register);
  RegisterUnit('FilterIIRBPBessel', @FilterIIRBPBessel.Register);
  RegisterUnit('FilterIIRBSButterworth', @FilterIIRBSButterworth.Register);
  RegisterUnit('FilterIIRBSChebyshev', @FilterIIRBSChebyshev.Register);
  RegisterUnit('FilterIIRBSBessel', @FilterIIRBSBessel.Register);
end;

initialization
  RegisterPackage('LazFiltersPkg', @Register);
end.
