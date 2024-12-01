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
unit FilterIIRLPBessel;

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
  Filter, FilterIIR, FilterIIRLP, UOperator, UType;


type

  TFilterIIRLPBessel = class(TFilterIIRLP)
  private

  protected

  public
    constructor Create(AOwner: TComponent);override;
    Destructor  Destroy();override;

    procedure SetupFilter(); override;

  published
    Property FreqCut1;
    property Order;
    property SampleRate;
    property ManualReset;
    property Gain;
  end;

procedure Register;

implementation
{$ifNdef FPC}
  {$R *.dcr}
{$endif}

  uses
   UASConst;

 //*****************************************************************************
 constructor TFilterIIRLPBessel.Create(AOwner: TComponent);
begin
   inherited Create (AOwner);
   Filter:= FilterPassAll;
   FreqCut1:= 300;
   SampleRate:= 44100;
   SetupFilter();
   Reset;
 end;

 //*****************************************************************************

Destructor  TFilterIIRLPBessel.Destroy();
Begin
  inherited Destroy ();
end;
//******************************************************************************
procedure TFilterIIRLPBessel.SetupFilter();
 var
  PoleMask: Longword;
  DCGain, FCGain, HFGain, cmpxGain: TComplex;
  Alpha1, Alpha2: TAS_Sample;

  I, TMP, TMP2, P: Integer;
  W1, Theta: TAS_Sample;

Begin

  if csLoading in ComponentState then Exit;

  Alpha1:= Max(0.0001 * Order, FreqCut1 / SampleRate);

  Alpha2:= Alpha1;


  {$I FilterIRRAuxSetup1.inc}

  {$I FilterIIRAuxSetupBessel.inc}

  {$I FilterIIRSetupAuxPasaBajo.inc}

  if not FManualReset then Reset;

end;

//******************************************************************************
procedure Register;
begin
  RegisterComponents('LazFilters',[TFilterIIRLPBessel]);
end;

initialization
{$ifdef FPC}
 {$I LPBessel.lrs}
{$endif}

end.
