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

unit FilterIIRHP;

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
  FilterIIR,UOperator, UType;

type
  TFilterIIRHP = class(TFilterIIR)
  private

  protected

  public
   constructor Create(AOwner: TComponent);override;
   Destructor  Destroy();override;

   procedure SetFreqCut1(const InputValue: TFreqType);
   procedure ResponseHighPass(var parW1: TAS_Sample);

   function  GetFreqCut1() : TFreqType;

   property FreqCut1 : TFreqType read GetFreqCut1 write SetFreqCut1;

  published

  end;

//procedure Register;

implementation
uses UASConst;

//******************************************************************************
constructor TFilterIIRHP.Create(AOwner: TComponent);
 begin
   inherited Create (AOwner);
   FreqCutCount := 1;
 end;

//******************************************************************************

Destructor  TFilterIIRHP.Destroy();
Begin
  inherited Destroy ();
end;

//******************************************************************************
function TFilterIIRHP.GetFreqCut1():TFreqType;
Begin
 result := FreqCut[1];
end;


//******************************************************************************
procedure TFilterIIRHP.SetFreqCut1(const InputValue: TFreqType);
Begin
  FreqCut[1]:=InputValue;
  SetupFilter();
End;


//******************************************************************************

procedure TFilterIIRHP.ResponseHighPass(var parW1: TAS_Sample);
 var
     I   : Integer;

begin

 for I:= 0 to SPlane.NumPoles - 1 do
      begin
        SPlane.Poles[I]:= cmpxDiv(cmpxInit(parW1), SPlane.Poles[I]);
        cmpxInit(SPlane.Zeros[I], 0.0);
      end;
      SPlane.NumZeros:= SPlane.NumPoles;

end;

//******************************************************************************
{procedure Register;
begin
  RegisterComponents('LazFilters',[TFilterIIRHP]);
end; }

end.
