{Quicksort, InsertSort and HeapSort algorithms are implemented for sorting of arrays of float, of TRealPoint for X
and for TRealPoint for Y
}
unit usorting;
{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, uTypes, uMinMax;

procedure QuickSort(Vector : TVector; Lb: integer = 0; Ub:integer = -1; desc:boolean = false);
procedure QuickSortX(Points : TRealPointVector; Lb: integer = 0; Ub:integer = -1; desc:boolean = false);
procedure QuickSortY(Points : TRealPointVector; Lb: integer = 0; Ub:integer = -1; desc:boolean = false);

procedure InsertSort(Vector : TVector; Lb: integer = 0; Ub:integer = -1; desc:boolean = false);
procedure InsertSortX(Points : TRealPointVector; Lb: integer = 0; Ub:integer = -1; desc:boolean = false);
procedure InsertSortY(Points : TRealPointVector; Lb: integer = 0; Ub:integer = -1; desc:boolean = false);

procedure Heapsort(Vector:TVector; Lb: integer = 0; Ub:integer = -1; desc:boolean = false);
procedure HeapSortX(Points:TRealPointVector; Lb: integer = 0; Ub:integer = -1; desc:boolean = false);
procedure HeapSortY(Points:TRealPointVector; Lb: integer = 0; Ub:integer = -1; desc:boolean = false);

implementation

procedure QuickSort(Vector : TVector; Lb: integer = 0; Ub:integer = -1; desc:boolean = false);
var
  I, J: Integer;
  P : Float;
begin
  if not InRange(Lb,0,High(Vector)) then
    Lb := 0;
  if not InRange(Ub,Lb,High(Vector)) then
    Ub := High(Vector);
  I := Lb; // leftmost index
  J := Ub; // rightmost index
  P := Vector[(Lb + Ub) div 2];  //find middle
  repeat
    if Desc then
    begin
      while Vector[I] > P do Inc(I);
      while Vector[J] < P do Dec(J);
    end else
    begin
      while Vector[I] < P do Inc(I);
      while Vector[J] > P do Dec(J);
    end;
    if I <= J then
    begin
      swap(Vector[I], Vector[J]);
      Inc(I);
      Dec(J);
    end;
  until I > J;
  if Lb < J then QuickSort(Vector, Lb,J,desc);
  if I < Ub then QuickSort(Vector, I,Ub,desc);
end;

procedure QuickSortX(Points : TRealPointVector; Lb: integer = 0; Ub:integer = -1; desc:boolean = false);
var
  I, J: Integer;
  P, T: TRealPoint;
begin
  if not InRange(Lb,0,High(Points)) then
    Lb := 0;
  if not InRange(Ub,Lb,High(Points)) then
    Ub := High(Points);
  I := Lb; // leftmost index
  J := Ub; // rightmost index
  P := Points[(Lb + Ub) div 2];  //find middle
  repeat
    if Desc then
    begin
      while Points[I].X > P.X do Inc(I);
      while Points[J].X < P.X do Dec(J);
    end else
    begin
      while Points[I].X < P.X do Inc(I);
      while Points[J].X > P.X do Dec(J);
    end;
    if I <= J then
    begin
      T := Points[I];
      Points[I] := Points[J];
      Points[J] := T;
      Inc(I);
      Dec(J);
    end;
  until I > J;
  if Lb < J then QuickSortX(Points, Lb,J, desc);
  if I < Ub then QuickSortX(Points, I, Ub, desc);
end;

procedure QuickSortY(Points : TRealPointVector; Lb: integer = 0; Ub:integer = -1; desc:boolean = false);
var
  I, J: Integer;
  P, T: TRealPoint;
begin
  if not InRange(Lb,0,High(Points)) then
    Lb := 0;
  if not InRange(Ub,Lb,High(Points)) then
    Ub := High(Points);
  I := Lb; // leftmost index
  J := Ub; // rightmost index
  P := Points[(Lb + Ub) div 2];  //find middle
  repeat
    if Desc then
    begin
      while Points[I].Y > P.Y do Inc(I);
      while Points[J].Y < P.Y do Dec(J);
    end else
    begin
      while Points[I].Y < P.Y do Inc(I);
      while Points[J].Y > P.Y do Dec(J);
    end;
    if I <= J then
    begin
      T := Points[I];
      Points[I] := Points[J];
      Points[J] := T;
      Inc(I);
      Dec(J);
    end;
  until I > J;
  if Lb < J then QuickSortY(Points, Lb,J, desc);
  if I < Ub then QuickSortY(Points, I, Ub, desc);
end;

procedure InsertSort(Vector : TVector; Lb: integer = 0; Ub:integer = -1; desc:boolean = false);
var
  I,J: integer;
  Buf:Float;
begin
  if not InRange(Lb,0,High(Vector)) then
    Lb := 0;
  if not InRange(Ub,Lb,High(Vector)) then
    Ub := High(Vector);
  if Ub - Lb < 1 then Exit;
  for I := Lb to Ub do
  begin
    Buf := Vector[I];
    J := I;
    while (J > Lb) and (desc and (Vector[J-1] < Buf) or
          (not desc and (Vector[J-1] > Buf))) do
    begin
      Vector[J] := Vector[J-1];
      J := J-1;
    end;
    Vector[J] := Buf;
  end;
end;

procedure InsertSortX(Points : TRealPointVector; Lb: integer = 0; Ub:integer = -1; desc:boolean = false);
var
  I,J: integer;
  Buf:TRealPoint;
begin
  if not InRange(Lb,0,High(Points)) then
    Lb := 0;
  if not InRange(Ub,Lb,High(Points)) then
    Ub := High(Points);
  if Ub - Lb < 1 then Exit;
  for I := Lb to Ub do
  begin
    Buf := Points[I];
    J := I;
    while (J > Lb) and (desc and (Points[J-1].X < Buf.X) or
          (not desc and (Points[J-1].X > Buf.X))) do
    begin
      Points[J] := Points[J-1];
      J := J-1;
    end;
    Points[J] := Buf;
  end;
end;

procedure InsertSortY(Points : TRealPointVector; Lb: integer = 0; Ub:integer = -1; desc:boolean = false);
var
  I,J: integer;
  Buf:TRealPoint;
begin
  if not InRange(Lb,0,High(Points)) then
    Lb := 0;
  if not InRange(Ub,Lb,High(Points)) then
    Ub := High(Points);
  if Ub - Lb < 1 then Exit;
  for I := Lb to Ub do
  begin
    Buf := Points[I];
    J := I;
    while (J > Lb) and (desc and (Points[J-1].Y < Buf.Y) or
          (not desc and (Points[J-1].Y > Buf.Y))) do
    begin
      Points[J] := Points[J-1];
      J := J-1;
    end;
    Points[J] := Buf;
  end;
end;

procedure HeapSort(Vector:TVector; Lb: integer = 0; Ub:integer = -1; desc:boolean = false);
var
  I,J,L,IR, Corr :Integer;
  Buf : float;
// Sorts an array Vector[Lb..Ub] into ascending numerical order using the Heapsort algorithm.
begin
  if not InRange(Lb,0,High(Vector)) then
    Lb := 0;
  if not InRange(Ub,Lb,High(Vector)) then
    Ub := High(Vector);
  if Ub - Lb < 1 then Exit;
// The index L will be decremented from its initial value down to 1 during the heap
// building phase. Once it reaches 1, the index IR will be decremented from its initial value
// down to 1 during the "retirement-and-promotion" (heap selection) phase.
  Corr := Lb - 1; // correction which maps Lb to 1
  IR := Ub - Corr;  // length of array; mapped last element 
  L := IR div 2 + 1; //Middle of array; border of upper half of heap.
  while true do      //Lower part will be filled automatically as children of upper.
  begin
    if L > 1 then
    begin        // Still in heap-building phase.
      Dec(L);
      Buf := Vector[L+Corr]
    end else
    begin                // In retirement-and-promotion phase.
      Buf := Vector[IR+Corr];         // Clear a space at end of array.
      Vector[IR+Corr] := Vector[Lb];  // Retire the top of the heap into it.
      Dec(IR);                        // Decrease the size of the corporation.
      if IR = 1 then
      begin                           // Done with the last promotion.
        Vector[Lb] := Buf;            // The least competent worker of all!
        Exit;
      end;
    end;
    I := L;             //Whether in the hiring phase or promotion phase, we here
    J := L+L;           //set up to sift down element Buf to its proper level.
    while J <= IR do    //initial J is IR 
    begin               //in Heap building phase IR was constant and pointed to the end of array
      if J < IR then                             // if 2 siblings exist then
        if (desc and (Vector[J+Corr] > Vector[J+Corr+1])) or
          (not desc and (Vector[J+Corr] < Vector[J+Corr+1])) then
          J := J+1;                              // found bigger sibling.   
      if (desc and (Buf > Vector[J+Corr])) or
         (not desc and (Buf < Vector[J+Corr]))then
      begin                                // Demote Buf (= Vector[I+Corr]) 
        Vector[I+Corr] := Vector[J+Corr];  // So former bigger child is now boss (in place of Buf or his child)
        I := J;                            // I points to former Child place
        J := J+J;                          // and J to its child
      end else          //  This is Buf's level: children are smaller him.                         
        J := IR + 1;    //  Set J to terminate the sift-down.
    end;
    Vector[I+Corr] := Buf;  //Put Buf into its slot.
  end;
end;

procedure HeapSortX(Points:TRealPointVector; Lb: integer = 0; Ub:integer = -1; desc:boolean = false);
var
  I,J,L,IR, Corr :Integer;
  Buf : TRealPoint;
begin
  if not InRange(Lb,0,High(Points)) then
    Lb := 0;
  if not InRange(Ub,Lb,High(Points)) then
    Ub := High(Points);
  if Ub - Lb < 1 then Exit;
  Corr := Lb - 1;
  IR := Ub - Corr;
  L := IR div 2 + 1;
  while true do
    begin
    if L > 1 then
    begin
      Dec(L);
      Buf := Points[L+Corr]
    end else
    begin
      Buf := Points[IR+Corr];
      Points[IR+Corr] := Points[Lb];
      Dec(IR);
      if IR = 1 then
      begin
        Points[Lb] := Buf;
        Exit;
      end;
    end;
    I := L;
    J := L+L;
    while J <= IR do
    begin
      if J < IR then
        if (desc and (Points[J+Corr].X > Points[J+Corr+1].X)) or
           (not desc and (Points[J+Corr].X < Points[J+Corr+1].X)) then
          J := J+1;
      if (desc and (Buf.X > Points[J+Corr].X)) or
         (not Desc and (Buf.X < Points[J+Corr].X)) then
      begin
        Points[I+Corr] := Points[J+Corr];
        I := J;
        J := J+J;
      end else
        J := IR + 1;
    end;
    Points[I+Corr] := Buf;
  end;
end;

procedure HeapSortY(Points:TRealPointVector; Lb: integer = 0; Ub:integer = -1; desc:boolean = false);
var
  I,J,L,IR, Corr :Integer;
  Buf : TRealPoint;
begin
  if not InRange(Lb,0,High(Points)) then
    Lb := 0;
  if not InRange(Ub,Lb,High(Points)) then
    Ub := High(Points);
  if Ub - Lb < 1 then Exit;
  Corr := Lb - 1;
  IR := Ub - Corr;
  L := IR div 2 + 1;
  while true do
    begin
    if L > 1 then
    begin
      Dec(L);
      Buf := Points[L+Corr]
    end else
    begin
      Buf := Points[IR+Corr];
      Points[IR+Corr] := Points[Lb];
      Dec(IR);
      if IR = 1 then
      begin
        Points[Lb] := Buf;
        Exit;
      end;
    end;
    I := L;
    J := L+L;
    while J <= IR do
    begin
      if J < IR then
        if (desc and (Points[J+Corr].Y > Points[J+Corr+1].Y)) or
           (not desc and (Points[J+Corr].Y < Points[J+Corr+1].Y)) then
          J := J+1;
      if (desc and (Buf.Y > Points[J+Corr].Y)) or
         (not Desc and (Buf.Y < Points[J+Corr].Y)) then
      begin
        Points[I+Corr] := Points[J+Corr];
        I := J;
        J := J+J;
      end else
        J := IR + 1;
    end;
    Points[I+Corr] := Buf;
  end;
end;

end.

