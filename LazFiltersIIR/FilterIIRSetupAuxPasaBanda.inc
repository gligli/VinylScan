// FilterIIRSetupAuxPasaBanda.inc

  W1:= 2 * TanDelphi(PI * Alpha1);
  W2:= 2 * TanDelphi(PI * Alpha2);

  ResponseBandPass(W1, W2, W0, BW, HBA, Temp);

  ComputeZPole;

  NumberOfCoefficients:= SPlane.NumPoles;

  ExpandPoly(DCGain,
             Theta,
             FCGain,
             HFGain,
             Alpha1,
             Alpha2);

  cmpxGain:= FCGain;

  InternalGain:= 1 / cmpxHypot(cmpxGain);

  Filter:= FilterFilter;


