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
unit UType;

{$ifdef FPC}
 {$MODE delphi}{$H+}
{$endif}

interface


type
  TFreqType = Extended;
  
  TFreqCutArray = Array [0..0]of TFreqType;
  
  PFreqCutArray = ^TFreqCutArray;

  FilterOrder = Integer;

  TAS_Sample = Extended;
  
  TAS_SampleArray = Array[0..High(Integer) div Sizeof(TAS_Sample) - 1] of TAS_Sample;
  
  PTAS_SampleArray = ^TAS_SampleArray;

  TAS_Float = Extended;
  
  TComplex = record
    Re, Im: TAS_Sample; // Z = Re + i*Im
	end;
 
  PComplex = ^TComplex;
  
  TComplexArray = Array[0..High(Integer) div Sizeof(TComplex) - 1] of TComplex;
  
  PComplexArray = ^TComplexArray;
  
  FilterProc = function(V: TAS_Sample): TAS_Sample of object;


implementation



end.









