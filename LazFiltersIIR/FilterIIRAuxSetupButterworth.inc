 //FilterIIRAuxSetupButterworth.inc

 TMP := 2 * Order - 1;
  for I:= 0 to TMP do
    begin
      Theta:= I;
      Tmp2 := (Order and 1);
      if Tmp2 = 0
         then Begin
                Theta:= Theta + 0.5;
              end;
      Theta:= Theta * PI / Order;
      ChoosePole(cmpxExpj(Theta), PoleMask);
    end;
