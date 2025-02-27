program mediana;
uses uTypes, uMeanSD_MD, uVecUtils, uVecMatPrn, uStrings, uMedian, uSorting,
  uMinMax;
var
  Vec1,Vec2,Vec3, Vec4 : TVector;
  Mask : TIntVector;
  Q: TQuantiles;

function Rand(F:integer):float;
begin
  Result := 12*random;
end;

var
  I,J,K,C:integer;
  Med, MedSort: float;
begin
  SetMd(-9999);
  vprnLB := 0;
  vprnFmtStr := '%9.2f';
  C := 0;
  Randomize;
  for J := 1 to 333 do
  begin
    K := (J + 1) div 2;
    for I := 0 to 200 do
    begin
      Vec4 := InitWithFunc(0,J,@Rand);
      Med := median(Vec4,0,J);
      HeapSort(Vec4);
      if J mod 2 = 0 then
        MedSort := Vec4[length(Vec4) div 2]
      else
        MedSort := (Vec4[K-1]+Vec4[K])/2;
      if not SameValue(Med,MedSort) then
        Inc(C);
    end;
  end;
  writeln(C);
  readln;
end.

