{ ******************************************************************
  Brackets a minimum of a function
  ****************************************************************** }
unit uminbrak;

interface

uses utypes, uminmax, uIntervals;

{ Given two points (A, B) this procedure finds a triplet (A, B, C) such that:
  1) A < B < C
  2) A, B, C are within the golden ratio
  3) Func(B) < Func(A) and Func(B) < Func(C).
  The corresponding function values are returned in Fa, Fb, Fc }
procedure MinBrack(Func : TFunc; var A, B: Float; out C, Fa, Fb, Fc : Float);

procedure SetBrakConstrain(L, R: Float);

implementation

var
  Constrain: TInterval = (Lo:-MaxNum/10; Hi:MaxNum/10);

procedure SetBrakConstrain(L, R: Float);
begin
  if L > R then
    Swap(R,L);
  Constrain := DefineInterval(L, R);
end;

procedure MinBrack(Func : TFunc; var A, B :float; out C, Fa, Fb, Fc : Float);
begin
  if A > B then
    Swap(A, B);
  Fa := Func(A);
  Fb := Func(B);
  if Fb > Fa then
  begin
    Swap(A, B); // now A > B
    Swap(Fa, Fb);  //Fa > Fb
  end;
  C := B + GOLD * (B - A); // if so, C < B, so we have C < B < A (or A > B > C)
  Fc := Func(C);
  while Fc < Fb do
  begin
    A := B;
    B := C;
    Fa := Fb;
    Fb := Fc;
    C := B + GOLD * (B - A); // and here it is increasing interval
    Fc := Func(C);
    if not Inside(C,Constrain) then // now we have brakes here
    begin
      if C > Constrain.Hi then
        C := Constrain.Hi
      else
        C := Constrain.Lo;
      Break;
    end;
  end;
  if A > C then   // but now we swap them and A < B < C
  begin
    Swap(A, C);
    Swap(Fa, Fc);
  end;
end;

end.
