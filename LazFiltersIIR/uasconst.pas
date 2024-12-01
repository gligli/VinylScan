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
unit UASConst;

{$include defines.inc}

{$ifdef FPC}
 {$MODE delphi}{$H+}
{$endif}

interface
 Uses
   {$ifdef shareware}
     Windows,
   {$endif}
   UType;
 Const
  cMaxPoles = 32;
  cEpsilon: TAS_Sample = 1e-10;
  cMaxValue: TAS_Sample = 1e10;

  ErrorInFreqCutValue      = -1; //Error en el valor entrado como FreqCut
  ErrorInFreqCutCountValue = -1; //Error en el valor entrado como Cantidad de FreqCut

   //Arreglo de polos para filtros Bessel

 cBesselPoles: Array[0..29] of TComplex = (
  (Re:-1.00000000000e+00; Im:0.00000000000e+00), (Re:-1.10160133059e+00; Im:6.36009824757e-01),
  (Re:-1.32267579991e+00; Im:0.00000000000e+00), (Re:-1.04740916101e+00; Im:9.99264436281e-01),
  (Re:-1.37006783055e+00; Im:4.10249717494e-01), (Re:-9.95208764350e-01; Im:1.25710573945e+00),
  (Re:-1.50231627145e+00; Im:0.00000000000e+00), (Re:-1.38087732586e+00; Im:7.17909587627e-01),
  (Re:-9.57676548563e-01; Im:1.47112432073e+00), (Re:-1.57149040362e+00; Im:3.20896374221e-01),
  (Re:-1.38185809760e+00; Im:9.71471890712e-01), (Re:-9.30656522947e-01; Im:1.66186326894e+00),
  (Re:-1.68436817927e+00; Im:0.00000000000e+00), (Re:-1.61203876622e+00; Im:5.89244506931e-01),
  (Re:-1.37890321680e+00; Im:1.19156677780e+00), (Re:-9.09867780623e-01; Im:1.83645135304e+00),
  (Re:-1.75740840040e+00; Im:2.72867575103e-01), (Re:-1.63693941813e+00; Im:8.22795625139e-01),
  (Re:-1.37384121764e+00; Im:1.38835657588e+00), (Re:-8.92869718847e-01; Im:1.99832584364e+00),
  (Re:-1.85660050123e+00; Im:0.00000000000e+00), (Re:-1.80717053496e+00; Im:5.12383730575e-01),
  (Re:-1.65239648458e+00; Im:1.03138956698e+00), (Re:-1.36758830979e+00; Im:1.56773371224e+00),
  (Re:-8.78399276161e-01; Im:2.14980052431e+00), (Re:-1.92761969145e+00; Im:2.41623471082e-01),
  (Re:-1.84219624443e+00; Im:7.27257597722e-01), (Re:-1.66181024140e+00; Im:1.22110021857e+00),
  (Re:-1.36069227838e+00; Im:1.73350574267e+00), (Re:-8.65756901707e-01; Im:2.29260483098e+00)
  );

implementation
{$ifdef shareware}
Initialization
 {$ifNdef FPC}
  // MessageBox(0,'Using Digital filters Components developed by www.antillasoft.com' + #13#10 +'Se estan usando Componentes de filtrado digital desarrollados por www.antillasoft.com', 'Information visit: www.AntillaSOFT.com', MB_OK);
  {$endif}
{$endif}

end.

