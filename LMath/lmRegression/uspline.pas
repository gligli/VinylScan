unit uSpline;
interface
uses uTypes, uIntervals, ulineq;

// Given vectors X, Y with data points and lower and upper bounds of data used 
// returns cubic spline values used in calls to Splint 
// This procedure must be called before actual drawing with Splint function
procedure InitSpline(Xv, Yv:TVector; out Ydv:TVector; Lb,Ub:integer);

// After preparing drawing by InitSpline, this function returns Y value given X
function SplInt(X:Float; Xv, Yv, Ydv: TVector; Lb,Ub:integer):Float;

// Returns first derivative to spline function at any given point
function SplDeriv(X:Float; Xv, Yv, Ydv: TVector; Lb, Ub:integer):float;

// returns all local minima and maxima of a spline between points Lb and Ub.
// Number of found minima is returned in NMin, of maxima - in NMax
procedure FindSplineExtremums(Xv,Yv,Ydv:TVector; Lb,Ub:integer;
          out Minima, Maxima:TRealPointVector; out NMin, NMax: integer; ResLb: integer = 1);

implementation

type
  TExtremum = record
    Pt:TRealPoint;
    minimum:boolean;
  end;
  TExtremums = array[1..2] of TExtremum;
  
procedure InitSpline(Xv, Yv:TVector; out Ydv:TVector; Lb,Ub:integer);
var
  I, J, N : integer;
  M   : TMatrix;
  V   : TVector;
  Det : Float;
begin
  N := Ub - Lb;
  DimMatrix(M,N,N);
  DimVector(V,N);
  DimVector(Ydv,Ub);
  for I := 1 to N-1 do
  begin
    J := I + Lb;
    M[I,I-1] := 1/(Xv[J] - Xv[J-1]);  // 1/L
    M[I,I]   := 2*(1/(Xv[J] - Xv[J-1]) + 1/(Xv[J+1] - Xv[J]));
    M[I,I+1] := 1/(Xv[J+1] - Xv[J]);
    V[I]     := 3 * ((Yv[J] - Yv[J-1])/Sqr(Xv[J] - Xv[J-1]) + (Yv[J+1]-Yv[J])/Sqr(Xv[J+1] - Xv[J]));
  end;
  M[0,0]  := 2/(Xv[Lb+1] - Xv[Lb]);
  M[0,1]  := 1/(Xv[Lb+1] - Xv[Lb]);
  V[0] := 3*(Yv[Lb+1] - Yv[Lb])/Sqr(Xv[Lb+1] - Xv[Lb]);

  M[N,N-1]:= 1/(Xv[Ub] - Xv[Ub-1]);
  M[N,N]  := 2/(Xv[Ub] - Xv[Ub-1]);
  V[N]:= 3*(Yv[Ub] - Yv[Ub-1])/Sqr(Xv[Ub] - Xv[Ub-1]);

  LinEq(M,V,0,N,Det);
  for I := 0 to N do
  begin
    Ydv[Lb + I] := V[I];
  end;
  Finalize(M);
  Finalize(V);
end;

function SplInt(X:Float; Xv, Yv, Ydv: TVector; Lb,Ub:integer):Float;
var
  K, Hi, Lo : integer;
  A, t, B, L, H: float;
begin
  Lo := Lb;
  Hi := Ub;
  if X < Xv[Lb] then
     Result := Yv[Lb] + Ydv[Lb]*(X - Xv[Lb])
  else if X > Xv[Ub] then
     Result := Yv[Ub] + Ydv[Ub]*(X - Xv[Ub])
  else begin
    while (Hi - Lo > 1) do
    begin
      K := (Hi + Lo) div 2;
      if X < Xv[K] then  // Interval in which X resides is found
        Hi := K         //by bisection
      else
        Lo := K;
    end;
    L := Xv[Hi] - Xv[Lo];
    H := Yv[Hi] - Yv[Lo];
    A := Ydv[Lo]*L - H;
    B := -Ydv[Hi]*L + H;
    t := (X - Xv[Lo])/L;
    Result := (1-t)*Yv[Lo] + t*Yv[Hi] + t*(1-t)*(A*(1-t)+B*t);
  end;
end;

function SplDeriv(X:Float; Xv, Yv, Ydv: TVector; Lb, Ub:integer):float;
var
  K, Hi, Lo : integer;
  A, t, B, L, H: float;
  E:float;
begin
  Lo := Lb;
  Hi := Ub;
  if X < Xv[Lb] then
     Result := Ydv[Lb] // spline is linear outside points array
  else if X > Xv[Ub] then
     Result := Ydv[Ub]
  else begin
    while (Hi - Lo > 1) do
    begin
      K := (Hi + Lo) div 2;
      if X < Xv[K] then  // Interval in which X resides is found
        Hi := K         //by bisection
      else
        Lo := K;
    end;
    L := Xv[Hi] - Xv[Lo];
    H := Yv[Hi] - Yv[Lo];
    A := Ydv[Lo]*L - H;
    B := -Ydv[Hi]*L + H;
    t := (X - Xv[Lo])/L;
    E := (A*(1-t) + B*t)/L;
    Result := H/L + (1-2*t)*E + t*(1-t)*(B-A)/L;
  end;
end;

procedure FindLocalExtremums(Xv,Yv,Ydv:TVector; Lb,Ub:integer; I : integer;
  out E:TExtremums; out N:integer);
var
  Den:float;
  Discr:float;
  J:integer;
  T1,T2:float;
  L,H,A,B:float;
  Buf:TExtremum;
begin
  N := 0;
  if not (I in [Lb..Ub-1]) then Exit;
  L := Xv[I+1] - Xv[I];
  H := Yv[I+1] - Yv[I];
  A := Ydv[I]*L - H;
  B := -Ydv[I+1]*L + H;
  Den := (A-B)*3;
  if IsZero(Den) then Exit;
  Discr := 3*H*(B-A) + A*A - A*B + B*B;
  if Discr < 0 then Exit;
  if IsZero(Discr) then
  begin
    T1 := (2*A - B) / Den;
    E[1].Pt.X := T1*L+Xv[I];
    if Inside(E[1].Pt.X,Xv[I],Xv[I+1]) then
      N := 1;
  end else
  begin
    T1 := (2*A - B + Sqrt(Discr)) / Den;
    T2 := -(B - 2*A + Sqrt(Discr)) / Den;
    E[1].Pt.X := T1*L+Xv[I];
    E[2].Pt.X := T2*L+Xv[I];
    if Inside(E[1].Pt.X,Xv[I],Xv[I+1]) then
      inc(N)
    else
      E[1] := E[2];
    if Inside(E[2].Pt.X,Xv[I],Xv[I+1]) then Inc(N);
 end;
  for J := 1 to N do
    E[J].Pt.Y := Splint(E[J].Pt.X, Xv,Yv,Ydv,Lb,Ub);
  if (N > 1) and (E[1].Pt.X > E[2].Pt.X) then
  begin
    Buf := E[1];
    E[1] := E[2];
    E[2] := Buf;
  end;
  E[1].Minimum := Ydv[I] < 0;
  E[2].Minimum := Ydv[I] > 0; 
end;

procedure FindSplineExtremums(Xv,Yv,Ydv:TVector; Lb,Ub:integer;
          out Minima, Maxima:TRealPointVector; out NMin, NMax: integer; ResLb: integer = 1);
var
  I,J, N : integer;
  LE:TExtremums;
begin
  SetLength(Minima,Ub-Lb+ResLb);
  SetLength(Maxima,Ub-Lb+ResLb);
  NMin := 0;
  NMax := 0;
  for I := Lb to Ub-1 do
  begin
    FindLocalExtremums(Xv,Yv,Ydv,Lb,Ub,I,LE,N);
    for J := 1 to N do
      if LE[J].minimum then
      begin
        Minima[ResLb+NMin] := LE[J].Pt;
        inc(NMin);
     end else
      begin
        Maxima[ResLb+NMax] := LE[J].Pt;
        inc(NMax);
      end;
  end;
  SetLength(Minima,Nmin+ResLb);
  SetLength(Maxima,NMax+ResLb);
end;

end.
