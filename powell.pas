// Powell's method for multidimensional optimization.  The implementation
// is taken from scipy, which requires the following notice:
// ******NOTICE***************
// optimize.py module by Travis E. Oliphant
//
// You may copy and use this module as you see fit with no
// guarantee implied provided you keep this notice in all copies.
// *****END NOTICE************


unit powell;

{$include 'compileroptions.inc'}

interface

uses
  Classes, SysUtils, Math;


type
  TScalar = Double;
  TVector = array of TScalar;
  TFunctionN = function(const x: TVector; data: Pointer): TScalar of object;
  TFunction = function(x: TScalar; const p, xi :TVector; f: TFunctionN; data: Pointer): TScalar;

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

function Bracket(f1: TFunction; xa, xb: TScalar; const p, xi :TVector; f: TFunctionN; data: Pointer): TVector;
const
  MaxIter = 1000;
  GrowLimit = 110;
  Gold = (1 + Sqrt(5)) / 2;
  Small = 1e-21;
var
  fa, fb, fc, xc, w, fw, tmp1, tmp2, val, denom, wlim: TScalar;
  iter: Integer;
begin
  fa := f1(xa, p, xi, f, data);
  fb := f1(xb, p, xi, f, data);
  if fa < fb then
  begin
    Swap(xa, xb);
    Swap(fa, fb);
  end;
  xc := xb + Gold * (xb - xa);
  fc := f1(xc, p, xi, f, data);
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
      fw := f1(w, p, xi, f, data);
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
      fw := f1(w, p, xi, f, data);
    end
    else if (w - wlim) * (wlim - xc) >= 0 then
    begin
      w := wlim;
      fw := f1(w, p, xi, f, data);
    end
    else if (w - wlim) * (xc - w) > 0 then
    begin
      fw := f1(w, p, xi, f, data);
      if fw < fc then
      begin
        xb := xc;
        xc := w;
        w := xc + Gold * (xc - xb);
        fb := fc;
        fc := fw;
        fw := f1(w, p, xi, f, data);
      end;
    end
    else
    begin
      w := xc + Gold * (xc - xb);
      fw := f1(w, p, xi, f, data);
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

function BrentHelper(f1: TFunction; a, x, b, fx, xtol: TScalar; maxiter: Integer; const p, xi :TVector; f: TFunctionN; data: Pointer): TVector;
const
  CG = (3 - Sqrt(5)) / 2;
var
  w, v, fw, fv, deltax, xmid, rat, tmp1, tmp2, pp, dx_temp, u, fu: TScalar;
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
      pp := (x - v) * tmp2 - (x - w) * tmp1;
      tmp2 := 2 * (tmp2 - tmp1);
      if tmp2 > 0 then
        pp := -pp;
      tmp2 := Abs(tmp2);
      dx_temp := deltax;
      deltax := rat;

      if (pp > tmp2 * (a - x)) and (pp < tmp2 * (b - x)) and (Abs(pp) < Abs(0.5 * tmp2 * dx_temp)) then
      begin
        rat := pp / tmp2;
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

    fu := f1(u, p, xi, f, data);

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

function Brent(f1: TFunction; const brack: TVector; xtol: TScalar; maxiter: Integer; const p, xi :TVector; f: TFunctionN; data: Pointer): TVector;
var
  a, b, c, fb: TScalar;
  br: TVector;
begin
  br := Bracket(f1, brack[0], brack[1], p, xi, f, data);
  a := br[0];
  b := br[1];
  c := br[2];
  fb := f1(b, p, xi, f, data);
  Result := BrentHelper(f1, a, b, c, fb, xtol, maxiter, p, xi, f, data);
end;

function AlongRay1(t: TScalar; const p, xi :TVector; f: TFunctionN; data: Pointer): TScalar;
var
  i: Integer;
  tmp: TVector;
begin
  SetLength(tmp, Length(p));
  for i := 0 to High(tmp) do
    tmp[i] := p[i] + t * xi[i];
  Result := f(tmp, data);
end;


function LinesearchPowell(f: TFunctionN; var p, xi: TVector; xtol: TScalar; data: Pointer): TScalar;
var
  n, i: Integer;
  atol, alpha, fret, sqsos: TScalar;
  alpha_fret_iter: TVector;
begin
  n := Length(p);

  sqsos := Sqrt(SumOfSquares(xi));
  atol := 1.0;
  if sqsos <> 0 then
    atol := 5 * xtol / sqsos;
  atol := Min(0.1, atol);

  alpha_fret_iter := Brent(@AlongRay1, Vec(0, 1), atol, 500, p, xi, f, data);
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
      fval := LinesearchPowell(f, x, direc[i], xtol, data);
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
        fval := LinesearchPowell(f, x, direc1, xtol, data);
        direc[bigind] := direc[n - 1];
        direc[n - 1] := direc1;
      end;
    end;
  end;

  Result := Vec(fval, iter);
end;

end.

