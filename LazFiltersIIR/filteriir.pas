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

unit FilterIIR;

{$ifdef FPC}
   {$MODE delphi}{$H+}
{$endif}
interface

uses
  Classes, SysUtils,
  {$ifdef FPC}
     LResources,
  {$endif}
//   Forms,
//   Controls, Graphics, Dialogs,
   Math,
  Filter, UOperator, UType, UASConst;

const
  cWrongCoeffError = 'Coeff of z^%d is not real; poles/zeros are not complex conjugates'#13#10;

 type
   TPoles = record
    Poles, Zeros: Array[0..cMaxPoles] of TComplex;
    NumPoles, NumZeros: Integer;
  end;

  TFilterIIR = class(TFilter)
  private
    FInternalGain: TAS_Sample;
  protected
    FLastError: String;
    FManualReset: Boolean;

    InputValues: Array[0..cMaxPoles] of TAS_Sample;
    OutputValues: Array[0..cMaxPoles] of TAS_Sample;
    InputCoefficients: Array[0..cMaxPoles] of TAS_Sample;
    OutputCoefficients: Array[0..cMaxPoles] of TAS_Sample;
    NumberOfCoefficients: Integer;

    SPlane, ZPlane: TPoles;
    procedure ChoosePole(Z: TComplex; var parPoleMask: Longword);
    procedure ComputeZPole;
    procedure Expand(ZPoles, C: PComplexArray; Count: Integer);
    procedure ExpandPoly(var parDCGain: TComplex;
                         var parTheta : TAS_Sample;
                         var parFCGain: TComplex;
                         var parHFGain: TComplex;
                         parAlpha1       : TAS_Sample;
                         parAlpha2       : TAS_Sample);

    property ManualReset: Boolean read FManualReset write FManualReset default false;

  public
    constructor Create(AOwner: TComponent);override;
    Destructor  Destroy();override;

    property LastError: String read FLastError;
    property InternalGain : TAS_Sample read FInternalGain write FInternalGain;
    function FilterFilter(InputValue: TAS_Sample): TAS_Sample; override;
    function FGetInfo:String; Override;
    function GetFrequencyResponse(FreqCut1: TFreqType): TFreqType;Override;
    procedure Reset;Override;

  published

  end;

// procedure Register;

implementation

//******************************************************************************
constructor TFilterIIR.Create(AOwner: TComponent);
 begin
   inherited Create (AOwner);
   FInternalGain  := 1;
   FMaxOrderFilter:= 7; //Estos algoritmos estan diseñados y probados para filtros de Orden=7
   FMinOrderFilter:= 1;
 end;
//******************************************************************************

Destructor  TFilterIIR.Destroy();
Begin
  inherited Destroy ();
end;

//******************************************************************************
function  TFilterIIR.FilterFilter(InputValue: TAS_Sample): TAS_Sample;
var
  I: Integer;

begin

  for I:= 0 to NumberOfCoefficients - 1 do
  begin
    InputValues[I]:= InputValues[I + 1];
    OutputValues[I]:= OutputValues[I + 1];
  end;
  Result:= (InputValue) * FInternalGain;
  InputValues[NumberOfCoefficients]:= Result;

  for I:= 0 to NumberOfCoefficients - 1 do
  begin
    Result:= Result + InputValues[I] * InputCoefficients[I] +
            OutputValues[I] * OutputCoefficients[I] ;

  end;

  OutputValues[NumberOfCoefficients]:= Result;
  result := result * Gain;
end;

//*******************************************************************************
function TFilterIIR.FGetInfo:String;
 var I: Integer;
 begin
  Result:= 'Gain: ' + Format('%e', [FInternalGain]) + #13#10'Input coefficients:'#13#10#13#10;
  for I:= 0 to NumberOfCoefficients - 1 do
    Result:= Result + '    ' + FloatToStr(InputCoefficients[I]) + #13#10;

  Result:= Result + #13#10 + 'Output coefficients:'#13#10#13#10;
  for I:= 0 to NumberOfCoefficients - 1 do
    Result:= Result + '    ' + FloatToStr(OutputCoefficients[I]) + #13#10;
 end;


 //*****************************************************************************
 function TFilterIIR.GetFrequencyResponse(FreqCut1: TFreqType): TFreqType;
  var
    I: Integer;
    Z, Z2: TComplex;
    TopCos, BotCos: Array[0..cMaxPoles] of TComplex;
  begin
   cmpxInit(TopCos[0], 0.0);
   cmpxInit(BotCos[0], 0.0);

   for I:= 1 to NumberOfCoefficients do
   begin
    cmpxInit(TopCos[I], InputCoefficients[NumberOfCoefficients - I]);
    cmpxInit(BotCos[I], -OutputCoefficients[NumberOfCoefficients - I]);
   end;

    Z:= cmpxExpj(2 * PI * FreqCut1 / SampleRate);
    Z2:= cmpxDiv(cmpxAdd(cmpxInit(1.0), cmpxEval(@TopCos, NumberOfCoefficients, Z)), (cmpxAdd(cmpxInit(1.0), cmpxEval(@BotCos, NumberOfCoefficients, Z))));
    Result:= (cmpxHypot(Z2) * InternalGain) * gain;
end;

//******************************************************************************
 procedure TFilterIIR.ChoosePole(Z: TComplex; Var parPoleMask: Longword);
  begin
    if Z.Re < 0.0 then
    begin
      if parPoleMask and 1 = 1 then
      begin
        SPlane.Poles[SPlane.NumPoles]:= Z;
        Inc(SPlane.NumPoles);
      end;

      parPoleMask:= parPoleMask shr 1;
    end;
  end;
//******************************************************************************

  // given S-plane poles & zeros, compute Z-plane poles & zeros, by bilinear transform

  procedure TFilterIIR.ComputeZPole;
  var I: Integer;
    function Transform(ZPole: TComplex): TComplex;
     var
        Tmp1, Tmp2 : TComplex;
    begin
     Result:= cmpxDiv(cmpxAdd(cmpxInit(2.0), ZPole), cmpxSub(cmpxInit(2.0), ZPole));
    end;
  begin
    ZPlane.NumPoles:= SPlane.NumPoles;
    ZPlane.NumZeros:= SPlane.NumZeros;
    for I:= 0 to ZPlane.NumPoles - 1 do
         ZPlane.Poles[I]:= Transform(SPlane.Poles[I]);

    for I:= 0 to ZPlane.NumZeros - 1 do
         ZPlane.Zeros[I]:= Transform(SPlane.Zeros[I]);

    while ZPlane.NumZeros < ZPlane.NumPoles do
    begin
      cmpxInit(ZPlane.Zeros[ZPlane.NumZeros], -1.0);
      Inc(ZPlane.NumZeros);
    end;
  end;

//******************************************************************************

  // compute product of poles or zeros as a polynomial of z
  procedure TFilterIIR.Expand(ZPoles, C: PComplexArray; Count: Integer);
  var
    I, J: Integer;
    W: TComplex;

    WarningMessage: String;

  begin
    C^[0]:= cmpxInit(1.0);
    for I:= 1 to Count do cmpxInit(C^[I], 0.0);
    for I:= 0 to Count - 1 do
    begin
      W:= cmpxNeg(ZPoles[I]);
      for J:= Count downto 1 do
       Begin
          C^[J]:= cmpxAdd(cmpxMul(W, C^[J]), C^[J-1]);
       End;
      C^[0]:= cmpxMul(W, C^[0]);
    end;

    // check computed coeffs of z^k are all real
    WarningMessage:= '';
    for I:= 0 to Count do
      if abs(C^[I].Im) > cEpsilon then
        WarningMessage:= WarningMessage + Format(cWrongCoeffError, [I]);
    if WarningMessage <> '' then FLastError:= WarningMessage;

  end;

//******************************************************************************

  // given Z-plane poles & zeros, compute top & bot polynomials in Z, and then recurrence relation
  procedure TFilterIIR.ExpandPoly(var parDCGain: TComplex;
                                  var parTheta : TAS_Sample;
                                  var parFCGain: TComplex;
                                  var parHFGain: TComplex;
                                  parAlpha1    : TAS_Sample;
                                  parAlpha2    : TAS_Sample);
   var
    I: Integer;
    C1, C2: Array[0..cMaxPoles] of TComplex;

  begin
    Expand(@ZPlane.Zeros, @C1, ZPlane.NumZeros);
    Expand(@ZPlane.Poles, @C2, ZPlane.NumPoles);

    parDCGain:= cmpxEvaluate(@C1, @C2, ZPlane.NumZeros, ZPlane.NumPoles, cmpxInit(1.0));
    parTheta:= PI * (parAlpha1 + parAlpha2);
    parFCGain:= cmpxEvaluate(@C1, @C2, ZPlane.NumZeros, ZPlane.NumPoles, cmpxExpj(partheta));
    parHFGain:= cmpxEvaluate(@C1, @C2, ZPlane.NumZeros, ZPlane.NumPoles, cmpxInit(-1.0));

    for I:= 0 to zplane.numzeros - 1 do
        InputCoefficients[i]:= C1[i].re / C2[ZPlane.NumPoles].Re;

    for I:= 0 to zplane.numpoles - 1 do
        OutputCoefficients[i]:= -C2[i].re / C2[zplane.NumPoles].Re;

  end;
 //*******************************************************************************
procedure TFilterIIR.Reset;
 var I: Integer;
 begin
  for I:= 0 to cMaxPoles do
  begin
    InputValues[I]:= 0;
    OutputValues[I]:= 0;
  end;
 end;
//******************************************************************************

{procedure Register;
begin
  RegisterComponents('LazFilters',[TFilterIIR]);
end; }

end.
