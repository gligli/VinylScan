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

unit Filter;

{$ifdef FPC}
 {$MODE delphi}{$H+}
{$endif}

interface

uses
  Classes, SysUtils,
  {$ifdef FPC}
   LResources,
  {$endif}
 // Controls, Dialogs,
  Math,
   UASConst, UType, UOperator;

resourcestring
   IndexOverflowErrorMsg ='Overflow index Error.';


type
  IndexOverflowError = class(Exception);

  TFilter = class(TComponent)
  private
    FOrder: FilterOrder;            //Orden del filtro                 //Filter Orden
    FSampleRate: TAS_Float;         //Frecuencia de Muestreo           //Sample Freq.
    FGain: TAS_Float;               //Ganancia final del filtro        //Filter Gain
    FFreqCutCount : Integer;        //Número de frecuencias de corte   ////Numbers of Cut Freq.
    FFreqCutArray : PFreqCutArray;  //Arreglo con todas las frecuencias de corte //Array with all Cut Freq. Values


  protected
    FMaxOrderFilter: Integer;
    FMinOrderFilter: Integer;

    procedure SetFreqCutCount(InputValue: Integer); Virtual;
    procedure SetFreqCut(Index: Integer; const InputValue: TFreqType); Virtual;

    procedure SetFreqCutWithOutCallSetupFilter(Index: Integer; const InputValue: TFreqType);
    procedure SetSampledRateWithOutCallSetupFilter(ASampleRate: TAS_Float);

    Function  GetFreqCut(Index: Integer) : TFreqType; Virtual;


  public

    Filter: FilterProc;

    constructor Create(AOwner: TComponent);override;
    destructor Destroy; override;

    procedure Reset; virtual;abstract;
    procedure SetupFilter(); virtual;
    procedure Loaded; virtual;

    procedure SetOrder(AOrder: FilterOrder); virtual;
    procedure SetSampleRate(ASampleRate: TAS_Float);


    function FilterPassAll(InputValue: TAS_Sample): TAS_Sample; virtual ;
    function FilterPassNothing(InputValue: TAS_Sample): TAS_Sample; virtual;
    function FilterFilter(InputValue: TAS_Sample): TAS_Sample; virtual; abstract;
    function FGetInfo: String; virtual; abstract;
    function GetFrequencyResponse(FreqCut1: TFreqType): TFreqType; virtual; abstract;
    property FreqCutCount: Integer read FFreqCutCount write SetFreqCutCount;
    property FreqCut[Index: Integer]: TFreqType read GetFreqCut write SetFreqCut;
    property Order: FilterOrder read FOrder write SetOrder ;
    property SampleRate: TAS_Float read  FSampleRate write SetSampleRate;
    property Gain : TAS_Float read FGain write FGain;
    property Info: String read FGetInfo;
  published

  end;

//procedure Register;

implementation

//******************************************************************************
constructor TFilter.Create(AOwner: TComponent);
 begin
   inherited Create (AOwner);
   FSampleRate:=1;
   FGain := 1;
   FOrder := 1;
   FMaxOrderFilter:= 9999999;
   FMinOrderFilter:= 1;
 end;

//******************************************************************************
procedure TFilter.Loaded;
begin
 inherited;
 SetupFilter();
end;

//******************************************************************************
procedure TFilter.SetOrder(AOrder: FilterOrder);
begin
 if (AOrder <> FOrder) and
    (AOrder >= FMinOrderFilter) and
    (AOrder <= FMaxOrderFilter) then
 begin
   FOrder:= AOrder;
   Reset; // Reset buffers in any case
   SetupFilter();
 end;
end;

//******************************************************************************
procedure TFilter.SetSampledRateWithOutCallSetupFilter(ASampleRate: TAS_Sample);
Var
 Tmp :TAS_Sample;
 i : Integer;
begin
 if (ASampleRate <> FSampleRate) and (ASampleRate > 0) then
 begin

   FSampleRate:= Abs(ASampleRate);

   for i:= 0 to FreqCutCount -1 do
    Begin
     Tmp := Min(FreqCut[i], ((FSampleRate / 2) - 0.0001));
     FreqCut[i]:= Tmp;
    end;
 end;
end;

//******************************************************************************
procedure TFilter.SetSampleRate(ASampleRate: TAS_Sample);
Var
 Tmp :TAS_Sample;
 i : Integer;

begin
 if (ASampleRate <> FSampleRate) and (ASampleRate > 0) then
 begin
   FSampleRate:= Abs(ASampleRate);

   for i:= 0 to FreqCutCount -1 do
    Begin

     Tmp := Min(FreqCut[i],((FSampleRate / 2) - 0.0001));
     FreqCut[i]:= Tmp;
    end;
   SetupFilter();
 end;
end;
//******************************************************************************
Function  TFilter.GetFreqCut(Index: Integer) : TFreqType;
 Begin
  if (Index > 0)  and
     (Index <= FreqCutCount)
     then result := FFreqCutArray^[Index-1] //Index-1 ya que el arreglo empieza en 0
     else result := ErrorInFreqCutValue; //Si hay problemas se devuelve -1 para indicar situacion
                        //Erronea
 end;

//*******************************************************************************
procedure  TFilter.SetFreqCut(Index: Integer; const InputValue: TFreqType);
Begin
   if (Index > 0) and
      (Index <= FreqCutCount)
      then FFreqCutArray[Index-1] := InputValue;

end;
//*******************************************************************************
procedure  TFilter.SetFreqCutWithOutCallSetupFilter(Index: Integer; const InputValue: TFreqType);
Begin
   if (Index > 0) and
      (Index <= FreqCutCount)
      then FFreqCutArray[Index-1] := InputValue;

end;

//*******************************************************************************
procedure  TFilter.SetFreqCutCount(InputValue: Integer);
var
 i : Integer;
Begin
  try
   Freemem(FFreqCutArray);
 except  //Si no puede liberar no hay problemas

 end;

 try
   {$ifdef FPC}
    FFreqCutArray := GetMem(InputValue * sizeOf(TFreqType)); //Usando Lazarus
   {$ELSE}
     GetMem(FFreqCutArray, InputValue * sizeOf(TFreqType)); //Usando Delphi
   {$endif}

 except Begin   //Si no puedo obtener la memoria pongo FFreqCutCount:=0 y salgo
         FFreqCutCount:=ErrorInFreqCutCountValue;
         exit;
        End
 end;
  FFreqCutCount := InputValue;

  //Garantizando que el arreglo de frecuencia no contenga ningun valor invalido
  for i:=1 to FFreqCutCount do
   FreqCut[i]:=i;

end;



//*******************************************************************************
//Descripcion: Permite pasar todas componentes de la señal. Lo mismo que llega
//             es lo que sale.
function  TFilter.FilterPassAll(InputValue: TAS_Sample): TAS_Sample;
 begin
  Result:= InputValue;
 end;

//*******************************************************************************
//Descripcion: No permite pasar ninguna componente de la señal. La salida siempre
//             es 0, independientemente de la entrada.
function  TFilter.FilterPassNothing(InputValue: TAS_Sample): TAS_Sample;
 begin
  Result:= 0;
 end;



//******************************************************************************
 procedure TFilter.SetupFilter();
  begin

  end;


//*******************************************************************************
Destructor  TFilter.Destroy();
Begin
 try
   Freemem(FFreqCutArray);
 except  //Si no puede liberar no hay problemas

 end;

 inherited Destroy ();

end;

 //*******************************************************************************
{procedure Register;
begin
  RegisterComponents('LazFilters',[TFilter]);
end;
   }

end.
