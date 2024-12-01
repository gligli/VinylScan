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
unit FilterIIRBS;

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
   FilterIIR, UOperator, UType, UASConst;

type
  TFilterIIRBS = class(TFilterIIR)
  private

  protected

  public
   constructor Create(AOwner: TComponent);override;
   Destructor  Destroy();override;

   procedure ResponseBandStop(var parW1, parW2, parW0, parBW : TAS_Sample;
                              var   parHBA, parTemp: TComplex);
   procedure SetFreqCut1(const InputValue: TFreqType); virtual;
   procedure SetFreqCut2(const InputValue: TFreqType); virtual;

   function  GetFreqCut1() : TFreqType;
   function  GetFreqCut2() : TFreqType;

   property FreqCut1 : TFreqType read GetFreqCut1 write SetFreqCut1;
   property FreqCut2 : TFreqType read GetFreqCut2 write SetFreqCut2;

  published

  end;

//procedure Register;

implementation

uses Filter;

//******************************************************************************
constructor TFilterIIRBS.Create(AOwner: TComponent);
 begin
   inherited Create (AOwner);
   FreqCutCount := 2;
   Filter:= FilterPassAll;
   SetFreqCutWithOutCallSetupFilter(1, 300);
   SetFreqCutWithOutCallSetupFilter(2, 3000);
   SetSampledRateWithOutCallSetupFilter(44100);
 end;

//******************************************************************************

Destructor  TFilterIIRBS.Destroy();
Begin
  inherited Destroy ();
end;

 //******************************************************************************

function TFilterIIRBS.GetFreqCut1(): TFreqType;
begin
  result := FreqCut[1];
end;

//******************************************************************************
function TFilterIIRBS.GetFreqCut2(): TFreqType;
begin
  result := FreqCut[2];
end;

//******************************************************************************
procedure  TFilterIIRBS.SetFreqCut1(const InputValue: TFreqType);
begin
  FreqCut[1]:=InputValue;
  SetupFilter();
end;

//******************************************************************************
 procedure  TFilterIIRBS.SetFreqCut2(const InputValue: TFreqType);
begin
  FreqCut[2]:=InputValue;
  SetupFilter();
end;

//******************************************************************************

procedure  TFilterIIRBS.ResponseBandStop(var parW1, parW2, parW0, parBW : TAS_Sample;
                                            var   parHBA, parTemp: TComplex);
var
   I: Integer;
begin
  if parW1 > parW2 then
      begin
        parBW:= parW1;
        parW1:= parW2;
        parW2:= parBW;
      end;

      parW0:= sqrt(parW1 * parW2);
      parBW:= Max(parW2 - parW1, 0.0001 * Order);
      for I:= 0 to SPlane.NumPoles - 1 do
      begin
        parHBA:= cmpxMul(SPlane.Poles[I], 0.5 * parBW);
        parTemp:= cmpxSqrt(cmpxSub(cmpxInit(1.0), cmpxSqr(cmpxDiv(cmpxInit(parW0), parHBA))));
        SPlane.Poles[I]:= cmpxMul(parHBA, cmpxAdd(cmpxinit(1.0), parTemp));
        SPlane.Poles[SPlane.NumPoles + I]:= cmpxMul(parHBA, cmpxSub(cmpxInit(1.0), parTemp));
      end;

      for I:= 0 to SPlane.NumPoles - 1 do
      begin
        SPlane.Zeros[I]:= cmpxInit(0.0, parW0);
        SPlane.Zeros[SPlane.NumPoles + I]:= cmpxInit(0.0, - parW0);
      end;

      SPlane.NumPoles:= SPlane.NumPoles * FreqCutCount;
      SPlane.NumZeros:= SPlane.NumPoles;

end;

//******************************************************************************
{procedure Register;
begin
  RegisterComponents('LazFilters',[TFilterIIRBS]);
end;  }

end.
