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

unit FilterIIRLP;

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
  FilterIIR, UOperator, UType, UASConst;

type

  TFilterIIRLP = class(TFilterIIR)
  private

  protected

  public

   constructor Create(AOwner: TComponent);override;
   Destructor  Destroy();override;

   procedure SetFreqCut1(const InputValue: TFreqType); virtual;
   procedure ResponseLowPass(var parW1: TAS_Sample);

   function  GetFreqCut1() : TFreqType;

   property FreqCut1 : TFreqType read GetFreqCut1 write SetFreqCut1;

  published


  end;

//procedure Register;

implementation

//******************************************************************************
constructor TFilterIIRLP.Create(AOwner: TComponent);
 begin
   inherited Create (AOwner);
   FreqCutCount := 1;
 end;

//******************************************************************************

Destructor  TFilterIIRLP.Destroy();
Begin
  inherited Destroy ();
end;

//******************************************************************************
function TFilterIIRLP.GetFreqCut1():TFreqType;
Begin
 result := FreqCut[1];
end;


//******************************************************************************
procedure TFilterIIRLP.SetFreqCut1(const InputValue: TFreqType);
Begin
  FreqCut[1]:=InputValue;
  SetupFilter();
End;


//******************************************************************************

  procedure TFilterIIRLP.ResponseLowPass(var parW1: TAS_Sample);
   var
     I   : Integer;
   begin
      SPlane.NumZeros:= 0;
      for I:= 0 to SPlane.NumPoles - 1 do SPlane.Poles[I]:= cmpxMul(SPlane.Poles[I], parW1);
   end;

//******************************************************************************

{procedure Register;
begin
  RegisterComponents('LazFilters',[TFilterIIRLP]);
end; }

end.
