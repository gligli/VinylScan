// FilterIIRSetupAuxPasaBajo.inc

  W1:= 2 * tan(PI * Alpha1);

  ResponseLowPass(W1);

  ComputeZPole;

  NumberOfCoefficients:= SPlane.NumPoles;

  ExpandPoly(DCGain,
             Theta,
             FCGain,
             HFGain,
             Alpha1,
             Alpha2);

  cmpxGain:= DCGain;

  InternalGain:= 1 / cmpxHypot(cmpxGain);

  Filter:= FilterFilter;
