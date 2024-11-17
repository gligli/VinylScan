// Powell's method for multidimensional optimization.  The implementation
// is taken from scipy, which requires the following notice:
// ******NOTICE***************
// optimize.py module by Travis E. Oliphant
//
// You may copy and use this module as you see fit with no
// guarantee implied provided you keep this notice in all copies.
// *****END NOTICE************


unit powell;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math;


type
  TScalar = Double;
  TVector = array of TScalar;
  TFunction = function(x: TScalar): TScalar;
  TFunctionN = function(const x: TVector; data: Pointer): TScalar;

function PowellMinimize(f: TFunctionN; var x: TVector; scale, xtol, ftol: TScalar; maxiter: Integer; data: Pointer = nil): TVector;


implementation

function Vec(a, b, c: TScalar): TVector; overload;
begin
  SetLength(Result, 3);
  Result[0] := a;
  Result[1] := b;
  Result[2] := c;
end;

function Vec(a, b: TScalar): TVector; overload;
begin
  SetLength(Result, 2);
  Result[0] := a;
  Result[1] := b;
end;

procedure Swap(var a, b: TScalar);
var
  temp: TScalar;
begin
  temp := a;
  a := b;
  b := temp;
end;

function Bracket(f: TFunction; xa, xb: TScalar): TVector;
const
  MaxIter = 1000;
  GrowLimit = 110;
  Gold = (1 + Sqrt(5)) / 2;
  Small = 1e-21;
var
  fa, fb, fc, xc, w, fw, tmp1, tmp2, val, denom, wlim: TScalar;
  iter: Integer;
begin
  fa := f(xa);
  fb := f(xb);
  if fa < fb then
  begin
    Swap(xa, xb);
    Swap(fa, fb);
  end;
  xc := xb + Gold * (xb - xa);
  fc := f(xc);
  iter := 0;
  while fc < fb do
  begin
    tmp1 := (xb - xa) * (fb - fc);
    tmp2 := (xb - xc) * (fb - fa);
    val := tmp2 - tmp1;
    if Abs(val) < Small then
      denom := 2 * Small
    else
      denom := 2 * val;
    w := xb - ((xb - xc) * tmp2 - (xb - xa) * tmp1) / denom;
    wlim := xb + GrowLimit * (xc - xb);
    if iter > MaxIter then
      raise Exception.Create('bracket: Too many iterations');
    Inc(iter);
    fw := 0;
    if (w - xc) * (xb - w) > 0 then
    begin
      fw := f(w);
      if fw < fc then
      begin
        xa := xb;
        xb := w;
        fa := fb;
        fb := fw;
        Break;
      end
      else if fw > fb then
      begin
        xc := w;
        fc := fw;
        Break;
      end;
      w := xc + Gold * (xc - xb);
      fw := f(w);
    end
    else if (w - wlim) * (wlim - xc) >= 0 then
    begin
      w := wlim;
      fw := f(w);
    end
    else if (w - wlim) * (xc - w) > 0 then
    begin
      fw := f(w);
      if fw < fc then
      begin
        xb := xc;
        xc := w;
        w := xc + Gold * (xc - xb);
        fb := fc;
        fc := fw;
        fw := f(w);
      end;
    end
    else
    begin
      w := xc + Gold * (xc - xb);
      fw := f(w);
    end;
    xa := xb;
    xb := xc;
    xc := w;
    fa := fb;
    fb := fc;
    fc := fw;
  end;
  if xa > xc then
  begin
    Swap(xa, xc);
    Swap(fa, fc);
  end;
  Result := Vec(xa, xb, xc);
end;

function BrentHelper(f: TFunction; a, x, b, fx, xtol: TScalar; maxiter: Integer): TVector;
const
  CG = (3 - Sqrt(5)) / 2;
var
  w, v, fw, fv, deltax, xmid, rat, tmp1, tmp2, p, dx_temp, u, fu: TScalar;
  iter: Integer;
begin
  if a > b then
    Swap(a, b);
  Assert((a < x) and (x < b), 'Invalid input range');

  w := x;
  v := x;
  fw := fx;
  fv := fx;
  deltax := 0;
  iter := 0;
  rat := 0;

  while iter < maxiter do
  begin
    xmid := 0.5 * (a + b);
    if Abs(x - xmid) <= 2 * xtol - 0.5 * (b - a) then
      Break;

    if Abs(deltax) <= xtol then
    begin
      if x >= xmid then
        deltax := a - x
      else
        deltax := b - x;
      rat := CG * deltax;
    end
    else
    begin
      tmp1 := (x - w) * (fx - fv);
      tmp2 := (x - v) * (fx - fw);
      p := (x - v) * tmp2 - (x - w) * tmp1;
      tmp2 := 2 * (tmp2 - tmp1);
      if tmp2 > 0 then
        p := -p;
      tmp2 := Abs(tmp2);
      dx_temp := deltax;
      deltax := rat;

      if (p > tmp2 * (a - x)) and (p < tmp2 * (b - x)) and (Abs(p) < Abs(0.5 * tmp2 * dx_temp)) then
      begin
        rat := p / tmp2;
        u := x + rat;
        if (u - a < xtol) or (b - u < xtol) then
          rat := Sign(xmid - x) * xtol;
      end
      else
      begin
        if x >= xmid then
          deltax := a - x
        else
          deltax := b - x;
        rat := CG * deltax;
      end;
    end;

    if Abs(rat) > xtol then
      u := x + rat
    else
      u := x + Sign(rat) * xtol;

    fu := f(u);

    if fu > fx then
    begin
      if u < x then
        a := u
      else
        b := u;
      if (fu <= fw) or (w = x) then
      begin
        v := w;
        w := u;
        fv := fw;
        fw := fu;
      end
      else if (fu <= fv) or (v = x) or (v = w) then
      begin
        v := u;
        fv := fu;
      end;
    end
    else
    begin
      if u >= x then
        a := x
      else
        b := x;
      v := w;
      w := x;
      x := u;
      fv := fw;
      fw := fx;
      fx := fu;
    end;

    Inc(iter);
  end;

  Result := Vec(x, fx, iter);
end;

function Brent(f: TFunction; brack: TVector; xtol: TScalar; maxiter: Integer): TVector;
var
  a, b, c, fb: TScalar;
  br: TVector;
begin
  br := Bracket(f, brack[0], brack[1]);
  a := br[0];
  b := br[1];
  c := br[2];
  fb := f(b);
  Result := BrentHelper(f, a, b, c, fb, xtol, maxiter);
end;

threadvar
  ar_data: Pointer;
  ar_f: TFunctionN;
  ar_p, ar_xi: TVector;

function AlongRay1(t: TScalar): TScalar;
var
  i: Integer;
  tmp: TVector;
begin
  SetLength(tmp, Length(ar_p));
  for i := 0 to High(tmp) do
    tmp[i] := ar_p[i] + t * ar_xi[i];
  Result := ar_f(tmp, ar_data);
end;


function LinesearchPowell(f: TFunctionN; var p, xi: TVector; xtol: TScalar; tmp: TVector; data: Pointer): TScalar;
var
  n, i: Integer;
  atol, alpha, fret, sqsos: TScalar;
  alpha_fret_iter: TVector;
begin
  ar_data := data;
  ar_p := p;
  ar_xi := xi;
  ar_f := f;

  n := Length(p);

  sqsos := Sqrt(SumOfSquares(xi));
  atol := 1.0;
  if sqsos <> 0 then
    atol := 5 * xtol / sqsos;
  atol := Min(0.1, atol);

  alpha_fret_iter := Brent(@AlongRay1, Vec(0, 1), atol, 100);
  alpha := alpha_fret_iter[0];
  fret := alpha_fret_iter[1];
  for i := 0 to n - 1 do
  begin
    xi[i] := xi[i] * alpha;
    p[i] := p[i] + xi[i];
  end;
  Result := fret;
end;

function PowellMinimize(f: TFunctionN; var x: TVector; scale, xtol, ftol: TScalar; maxiter: Integer; data: Pointer): TVector;
var
  n, i, iter, bigind: Integer;
  direc1, tmp, x1: TVector;
  direc: array of TVector;
  fval, fx, delta, fx2, t, temp: TScalar;
begin
  Assert((scale > 0) and (xtol >= 0) and (ftol >= 0), 'Invalid input parameters');
  n := Length(x);
  SetLength(direc1, n);
  SetLength(tmp, n);

  SetLength(direc, n);
  for i := 0 to n - 1 do
  begin
    SetLength(direc[i], n);
    direc[i, i] := scale;
  end;

  fval := f(x, data);
  x1 := Copy(x);
  iter := 0;

  while True do
  begin
    fx := fval;
    bigind := 0;
    delta := 0;
    for i := 0 to n - 1 do
    begin
      fx2 := fval;
      fval := LinesearchPowell(f, x, direc[i], xtol, tmp, data);
      if fx2 - fval > delta then
      begin
        delta := fx2 - fval;
        bigind := i;
      end;
    end;
    Inc(iter);
    if (fx - fval <= ftol) or (iter >= maxiter) then
      Break;

    for i := 0 to n - 1 do
    begin
      direc1[i] := x[i] - x1[i];
      tmp[i] := x[i] + direc1[i];
      x1[i] := x[i];
    end;
    fx2 := f(tmp, data);

    if fx > fx2 then
    begin
      t := 2 * (fx + fx2 - 2 * fval);
      temp := fx - fval - delta;
      t := t * temp * temp;
      temp := fx - fx2;
      t := t - delta * temp * temp;
      if t < 0 then
      begin
        fval := LinesearchPowell(f, x, direc1, xtol, tmp, data);
        direc[bigind] := direc[n - 1];
        direc[n - 1] := direc1;
      end;
    end;
  end;

  Result := Vec(fval, iter);
end;

end.

