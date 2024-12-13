{ ******************************************************************
  Student t-test for paired samples
  ****************************************************************** }

unit ustdpair;

interface

uses
  utypes, uErrors, uminmax, umeansd;

{ Student t-test for paired samples
 
  Input parameters : X, Y   = samples
                     Lb, Ub = lower and upper bounds
  Output parameters: T      = Student's t
                     DoF    = degrees of freedom }
procedure StudPaired(X, Y    : TVector;
                     Lb, Ub  : Integer;
                     out T   : Float;
                     out DoF : Integer);

implementation

procedure StudPaired(X, Y    : TVector;
                     Lb, Ub  : Integer;
                     out T   : Float;
                     out DoF : Integer);

var
  D      : TVector;  { Differences between samples  }
  MD, SD : Float;    { Mean & std.dev. of differences }
  N      : Integer;  { Sample size }
  I      : Integer;  { Loop variable }

begin
  DimVector(D, Ub);

  for I := Lb to Ub do
    D[I] := X[I] - Y[I];

  MD := Mean(D, Lb, Ub);
  SD := StDev(D, Lb, Ub, MD);

  if SD = 0.0 then
    begin
      T := Sgn(MD) * MaxNum;
      SetErrCode(FSing);
    end;

  DoF := Ub - Lb;  { N - 1 }
  N := DoF + 1;
  T := MD * Sqrt(N) / SD;
end;

end.
