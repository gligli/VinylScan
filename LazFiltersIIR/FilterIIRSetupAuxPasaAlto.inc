// FilterIIRSetupAuxPasaAlto.inc

  W1:= 2 * tan(PI * Alpha1);

  ResponseHighPass(W1);

  ComputeZPole;

  NumberOfCoefficients:= SPlane.NumPoles;

  ExpandPoly(DCGain,
             Theta,
             FCGain,
             HFGain,
             Alpha1,
             Alpha2);

  cmpxGain:= HFGain;

  InternalGain:= 1 / cmpxHypot(cmpxGain);

  Filter:= FilterFilter;
