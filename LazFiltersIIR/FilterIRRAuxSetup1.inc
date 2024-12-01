  //FilterIRRAuxSetup1.inc

  for I:= 0 to CMaxPoles do
  begin
    SPlane.Poles[I]:= cmpxInit(0); SPlane.Zeros[I]:= cmpxInit(0);
    ZPlane.Poles[I]:= cmpxInit(0); ZPlane.Zeros[I]:= cmpxInit(0);
  end;

  SPlane.NumPoles:= 0; SPlane.NumZeros:= 0;
  ZPlane.NumPoles:= 0; ZPlane.NumZeros:= 0;
  PoleMask:= not Longword(0);
