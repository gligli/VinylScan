 //FilterIIRAuxSetupBessel.inc

P:= Round((Order * Order) / 4); // ptr into table
  Tmp:= (Order and 1);
  if Tmp = 1 then
    begin
      ChoosePole(cBesselPoles[p],PoleMask);
      Inc(P);
    end;

    I:= 0;
    while I < Order div 2 do
    begin
      ChoosePole(cBesselPoles[P],PoleMask);
      ChoosePole(cmpxConj(cBesselPoles[P]),PoleMask);
      Inc(P); Inc(I);
    end;
