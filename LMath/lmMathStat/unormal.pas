{ ******************************************************************
  Density of standard normal distribution
  ****************************************************************** }

unit unormal;

interface

uses
  utypes, uErrors;

{ Density of standard normal distribution }
function DNorm(X : Float) : Float;

{Density of gaussian distribution with orbitrary math. expectation and sigma}
function DGaussian(X, Mean, Sigma: float) : float;

implementation

function DNorm(X : Float) : Float;
var
  Y : Float;
begin
  Y := - 0.5 * X * X;
  if Y < MinLog then
    DNorm := DefaultVal(FUnderflow, 0.0)
  else
    begin
      SetErrCode(FOk);
      DNorm := InvSqrt2Pi * Exp(Y);
    end;
end;

function DGaussian(X, Mean, Sigma: float) : float;
var
  Y : Float;
  Diff: Float;
begin
  Diff := X - Mean;
  if IsZero(Sigma) then
  begin
    if IsZero(Diff) then
      Result := DefaultVal(FSing,1.0)
    else
      Result := DefaultVal(FSing,0.0);
    Exit;
  end;
  Y := -(Diff * Diff) / (2*Sigma*sigma);
  if Y < MinLog then
    Result := DefaultVal(FUnderflow, 0.0)
  else
    begin
      SetErrCode(FOk);
      if Sigma = 0 then
      begin
        if X = Mean then
          Result := Mean
        else
          Result := 0;
      end else
        Result := InvSqrt2Pi / Sigma * Exp(Y);
    end;
end;

end.
