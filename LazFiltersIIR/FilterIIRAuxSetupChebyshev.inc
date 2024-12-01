 //FilterIIRAuxSetupChebyshev.inc

  TMP := (2 * Order) - 1;
  for I:= 0 to TMP do
    begin
      Theta:= I;
      Tmp2 := (Order and 1);
      if Tmp2 = 0
         then Begin
                  case Order of
                   2 : Theta:= Theta + 0.2437;
                   4 : Theta:= Theta + 0.3163;
                   6 : Theta:= Theta + 0.343;
                  // 8 : Theta:= Theta + 0.3574;
                  // 10 : Theta:= Theta + 0.3612;
                  end;

              end;
      Theta:= Theta * PI / Order;
      ChoosePole(cmpxExpj(Theta), PoleMask);
    end;
