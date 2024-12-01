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
unit FilterIIRBSButterworth;

{$ifdef FPC}
   {$MODE delphi}{$H+}
{$endif}

interface

uses
  Classes, SysUtils,
  {$ifdef FPC}
   LResources,
  {$endif}
 // Forms, Controls, Graphics, Dialogs,
  Math,
  Filter, FilterIIR, FilterIIRBS, UOperator, UType;

type

  TFilterIIRBSButterworth = class(TFilterIIRBS)
  private

  protected

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
  end;

procedure Register;

implementation
{$ifNdef FPC}
  {$R *.dcr}
{$endif}

 uses
   UASConst;
 //*****************************************************************************
 constructor TFilterIIRBSButterworth.Create(AOwner: TComponent);
 begin
   inherited Create (AOwner);
   SetupFilter();
   Reset;
 end;

//*****************************************************************************

Destructor  TFilterIIRBSButterworth.Destroy();
Begin
  inherited Destroy();
end;

//******************************************************************************

procedure TFilterIIRBSButterworth.SetupFilter();
  var
  PoleMask: Longword;
  HBA, Temp, DCGain, FCGain, HFGain, cmpxGain, TMP3, TMP4: TComplex;
  Alpha1, Alpha2, tmp5: TAS_Sample;

  I, TMP, TMP2: Integer;
  W1, W2, W0, BW, Theta: TAS_Sample;

Begin

  if csLoading in ComponentState then Exit;

  Alpha1:= Max(0.0001 * Order, FreqCut1 / SampleRate);
  Alpha2:= Max(0.0001 * Order, FreqCut2 / SampleRate);

  {$I FilterIRRAuxSetup1.inc}

  {$I FilterIIRAuxSetupButterworth.inc}

  {$I FilterIRRAuxSetupBandStop.inc}

  // Check Alpha1 = Alpha2 for bandstop and bandpass filters

  if Alpha1 = Alpha2 then

  Filter:= FilterPassAll;

  if not FManualReset then Reset;

end;

//******************************************************************************
procedure Register;
begin
  RegisterComponents('LazFilters',[TFilterIIRBSButterworth]);
end;

initialization
{$ifdef FPC}
  {$I BSButterworth.lrs}
{$endif}

end.
