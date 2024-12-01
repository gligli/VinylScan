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
unit FilterIIRBPChebyshev;

{$ifdef FPC}
   {$MODE delphi}{$H+}
{$endif}


interface

uses
  Classes, SysUtils,
  {$ifdef FPC}
   LResources,
  {$endif}
  //Forms, Controls, Graphics, Dialogs,
  Math,
  Filter, FilterIIR, FilterIIRBP, UOperator, UType;

 resourcestring
   cChebyshevError = 'Chebyshev filter error y=%g; must be .gt. 0.0';

type

  TFilterIIRBPChebyshev = class(TFilterIIRBP)
  private
    FRipple: TAS_Sample;

  protected
    procedure SetRipple(ARipple: TAS_Sample);
    procedure SetupForChebyshev();

  public
    constructor Create(AOwner: TComponent);override;
    Destructor  Destroy();override;

    procedure SetupFilter(); override;

  published
    Property FreqCut1;
    Property FreqCut2;
    property Order;
    property SampleRate;
    property ManualReset;
    property Gain;
    property Ripple: TAS_Sample read FRipple write SetRipple;
  end;

procedure Register;

implementation
{$ifNdef FPC}
  {$R *.dcr}
{$endif}

 uses
   UASConst;
 //*****************************************************************************

constructor TFilterIIRBPChebyshev.Create(AOwner: TComponent);
 begin
   inherited Create (AOwner);
   Ripple:= 0.1;
   Reset;
 end;

//*****************************************************************************

Destructor  TFilterIIRBPChebyshev.Destroy();
Begin
  inherited Destroy();
end;

//******************************************************************************

procedure TFilterIIRBPChebyshev.SetRipple(ARipple: TAS_Sample);
begin
 ARipple:= Max(0.1, Abs(ARipple));
 if ARipple <> FRipple then
 begin
   FRipple:= ARipple;
   SetupFilter();
 end;
end;

//******************************************************************************

procedure TFilterIIRBPChebyshev.SetupForChebyshev();
 var
   I: Integer;
   Rip, Eps, Y, tmp: TAS_Sample;
begin
   Rip:= Power(10.0, Ripple / 10.0);
   Eps:= Max(Sqrt(Rip - 1.0), 1e-10);
   Y:= ArcSinh(1.0 / Eps) / Order;

   if Y <= 0.0 then FLastError:=  Format(cChebyshevError, [Y]) else
     for I:= 0 to SPlane.NumPoles - 1 do
     begin
       Tmp := SPlane.Poles[I].Re;
       SPlane.Poles[I].Re := Tmp * Sinh(Y);

       Tmp := SPlane.Poles[I].Im;
       SPlane.Poles[I].Im := Tmp * Cosh(Y);
     end;

end;

//******************************************************************************

procedure TFilterIIRBPChebyshev.SetupFilter();
  var
   PoleMask: Longword;
  HBA, Temp, DCGain, FCGain, HFGain, cmpxGain: TComplex;
  Alpha1, Alpha2: TAS_Sample;

  I, TMP, TMP2: Integer;
  W1, W2, W0, BW, Theta: TAS_Sample;

Begin

  if csLoading in ComponentState then Exit;

  Alpha1:= Max(0.0001 * Order, FreqCut1 / SampleRate);
  Alpha2:= Max(0.0001 * Order, FreqCut2 / SampleRate);

  {$I FilterIRRAuxSetup1.inc}

  {$I FilterIIRAuxSetupChebyshev.inc}

  SetupForChebyshev();

  {$I FilterIIRSetupAuxPasaBanda.inc}

  // Check Alpha1 = Alpha2 for bandstop and bandpass filters

  if Alpha1 = Alpha2 then

  Filter:= FilterPassNothing;

  if not FManualReset then Reset;

 end;

//******************************************************************************
procedure Register;
begin
  RegisterComponents('LazFilters',[TFilterIIRBPChebyshev]);
end;

initialization
{$ifdef FPC}
 {$I BPChebyshev.lrs}
{$endif}

end.
