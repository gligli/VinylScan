{*********************************************************}
{                                                         }
{         AntillaSoft Signal Processing Component         }
{                Digital Filter Component                 }
{                                                         }
{*********************************************************}

{*********************************************************}
{    Copyright (c) 2005-2012 AntillaSoft                  }
{                                                         }
{ License Agreement:                                      }
{                                                         }
{ This library is distributed AS IS, with the hope that   }
{ it will be useful, but WITHOUT ANY WARRANTY; without    }
{ even the implied warranty of MERCHANTABILITY or FITNESS }
{ FOR A PARTICULAR PURPOSE.                               }
{                                                         }
{ Please read the file License.txt before use this        }
{ Component.                                              }
{                                                         }
{ The project web site is located on:                     }
{   http://www.antillasoft.com                            }
{                                                         }
{                          AntillaSoft Development Group. }
{*********************************************************}
unit UOperator;

{$ifdef FPC}
 {$MODE delphi}{$H+}
{$endif}

interface

uses
  Math, UType;

  function TanDelphi(const X: Extended): Extended;
  function  cmpxInit  (Re: TAS_Sample; Im: TAS_Sample = 0.0): TComplex; overload;
  procedure cmpxInit  (var Z: TComplex; Re: TAS_Sample; Im: TAS_Sample = 0.0); overload;

  function cmpxNeg   (Z: TComplex)   : TComplex;
  function cmpxSqr   (Z: TComplex)   : TComplex;
  function cmpxSqrt  (Z: TComplex)   : TComplex;
  function cmpxSqrtx (V: TAS_Sample)     : TAS_Sample;
  function cmpxExp   (Z: TComplex)   : TComplex;
  function cmpxExpj  (V: TAS_Sample)     : TComplex;
  function cmpxHypot (Z: TComplex)   : TAS_Sample;
  function cmpxArcTan2 (Z: TComplex) : TAS_Sample;
  function cmpxConj  (Z: TComplex)   : TComplex; // Conjugate the complex number

  function cmpxMul   (Z: TComplex; V: TAS_Sample) : TComplex; overload;
  function cmpxMul   (Z1, Z2: TComplex)          : TComplex; overload;
  function cmpxDiv   (Z: TComplex; V: TAS_Sample) : TComplex; overload;
  function cmpxDiv   (Z1, Z2: TComplex)          : TComplex; overload;
  function cmpxAdd   (Z1, Z2: TComplex)          : TComplex;
  function cmpxSub   (Z1, Z2: TComplex)          : TComplex;

  function cmpxEval(C: PComplexArray; Count: Integer; Z: TComplex): TComplex;
  function cmpxEvaluate(C1, C2: PComplexArray; Count1, Count2: Integer; Z: TComplex): TComplex;

implementation

//******************************************************************************
function cmpxInit (Re: TAS_Sample; Im: TAS_Sample = 0.0): TComplex;
begin
  Result.Re:= Re;
  Result.Im:= Im;
end;

//******************************************************************************
procedure cmpxInit(var Z: TComplex; Re: TAS_Sample; Im: TAS_Sample = 0.0);
begin
  Z.Re:= Re;
  Z.Im:= Im;
end;

//******************************************************************************
function cmpxNeg (Z: TComplex): TComplex;
begin
  Result.Re:= -Z.Re;
  Result.Im:= -Z.Im;
end;

//******************************************************************************
function cmpxSqr (Z: TComplex): TComplex;
begin
  Result:= cmpxMul(Z, Z);
end;

//******************************************************************************
function cmpxSqrt (Z: TComplex): TComplex;
var V: TAS_Sample;
begin
  V:= cmpxHypot(Z);
  Result.Re:= cmpxSqrtx(0.5 * (V + Z.Re));
  Result.Im:= cmpxSqrtx(0.5 * (V - Z.Re));
  if (Z.Im < 0.0) then Result.Im:= -Result.Im;
end;

//******************************************************************************
function cmpxSqrtx (V: TAS_Sample): TAS_Sample;
begin
  Result:= Sqrt(Max(V, 0.0));
end;

//******************************************************************************
function cmpxExp (Z: TComplex): TComplex;
begin
  Result:= cmpxMul(cmpxExpj(Z.Im), Exp(Z.Re));
end;

//******************************************************************************
function cmpxExpj (V: TAS_Sample): TComplex;
begin
  Result.Im:= Sin(V);
  Result.Re:= Cos(V);
end;

//******************************************************************************
function cmpxHypot (Z: TComplex): TAS_Sample;
begin
  Result:= Hypot(Z.Re, Z.Im);
end;

//******************************************************************************
function cmpxArcTan2 (Z: TComplex): TAS_Sample;
begin
  Result:= ArcTan2(Z.Im, Z.Re);
end;

//******************************************************************************
function TanDelphi(const X: Extended): Extended; //la de Delphi difiere de la de lazarus por eso puse esta aqui.
Begin
 Result := Sin(X) / Cos(X) ;
//asm
//        FLD    X
//        FPTAN
//        FSTP   ST(0)      { FPTAN pushes 1.0 after result }
//        FWAIT
end;

//******************************************************************************
function cmpxConj (Z: TComplex): TComplex;
begin
  Result.Re:= Z.Re;
  Result.Im:= -Z.Im;
end;

//******************************************************************************
function cmpxMul (Z: TComplex; V: TAS_Sample): TComplex;
begin
  Result.Re:= Z.Re * V;
  Result.Im:= Z.Im * V;
end;

//******************************************************************************
function cmpxMul (Z1, Z2: TComplex): TComplex;
begin
  Result.Re:= (Z1.Re * Z2.Re) - (Z1.Im * Z2.Im);
  Result.Im:= (Z1.Re * Z2.Im) + (Z1.Im * Z2.Re);
end;

//******************************************************************************
function cmpxDiv (Z: TComplex; V: TAS_Sample): TComplex;
begin
  Result.Re:= Z.Re / V;
  Result.Im:= Z.Im / V;
end;

//******************************************************************************
function cmpxDiv (Z1, Z2: TComplex): TComplex;
var
  L: TAS_Sample;
begin

  L:= Z2.Re * Z2.Re + Z2.Im * Z2.Im;
  if L <> 0 then Begin
                  Result.Re:= ((Z1.Re * Z2.Re )+ (Z1.Im * Z2.Im)) / L;
                  Result.Im:= ((Z1.Im * Z2.Re) - (Z1.Re * Z2.Im)) / L;
                 end
            else Begin
                  Result.Re:=0;
                  Result.Im:=0;
                 end;

end;

//******************************************************************************
function cmpxAdd (Z1, Z2: TComplex): TComplex;
begin
  Result.Re:= Z1.Re + Z2.Re;
  Result.Im:= Z1.Im + Z2.Im;
end;

//******************************************************************************
function cmpxSub (Z1, Z2: TComplex): TComplex;
begin
  Result.Re:= Z1.Re - Z2.Re;
  Result.Im:= Z1.Im - Z2.Im;
end;

//******************************************************************************
function cmpxEval(C: PComplexArray; Count: Integer; Z: TComplex): TComplex;
var
 I: Integer;
 cmpxTmp1 : TComplex;
begin
  cmpxInit(Result, 0);
  for I:= Count downto 0 do
    Begin
     cmpxTmp1 := cmpxMul(Result, Z);
     Result:= cmpxAdd(cmpxTmp1, C^[I]);
    end;
end;

//******************************************************************************
function cmpxEvaluate(C1, C2: PComplexArray; Count1, Count2: Integer; Z: TComplex): TComplex;
var
 Tmp1, Tmp2 : TComplex;
begin
  Tmp1 := cmpxEval(C1, Count1, Z);
  Tmp2 := cmpxEval(C2, Count2, Z);
  Result:= cmpxDiv(Tmp1, Tmp2);

end;
end.

