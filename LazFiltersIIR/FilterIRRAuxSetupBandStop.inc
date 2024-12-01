  //FilterIRRAuxSetupBandStop.inc

  W1:= 2 * TanDelphi(PI * Alpha1);
  W2:= 2 * TanDelphi(PI * Alpha2);

  ResponseBandStop(W1, W2, W0, BW, HBA, Temp);

  ComputeZPole;

  NumberOfCoefficients:= SPlane.NumPoles;

  ExpandPoly(DCGain,
             Theta,
             FCGain,
             HFGain,
             Alpha1,
             Alpha2);

  try
   TMP3:=cmpxMul(DCGain, HFGain);
  except Begin
           TMP3.Re := 0;
           TMP3.Im := 0;
         end;
  end;

  try
   TMP4:= cmpxSqrt(TMP3);
  except Begin
           TMP4.Re := 0;
           TMP4.Im := 0;
         end;
  end;

  cmpxGain:= TMP4 ;

  try
  tmp5 := cmpxHypot(cmpxGain)
  except Begin
           tmp5 := 0;
          end;
  end;

  try
     InternalGain:= 1 / tmp5;
  except Begin
           InternalGain := 0;
         end;
  end;

  Filter:= FilterFilter;


