{ ******************************************************************
  Gaussian random numbers
  ****************************************************************** }

unit urangaus;

interface

uses
  utypes, urandom;

{ Computes 2 random numbers from the standard normal distribution,
  returns one and saves the other for the next call }
function RanGaussStd : Float;

{ Returns a random number from a Gaussian distribution
  with mean Mu and standard deviation Sigma }
function RanGauss(Mu, Sigma : Float) : Float;

implementation

var
  GaussSave : Float   = 0.0;   { Saves a Gaussian number }
  GaussNew  : Boolean = True;  { Flags a new calculation }

function RanGaussStd : Float;
var
  R, Theta : Float;
begin
  if GaussNew then
    begin
      R := Sqrt(-2.0 * Ln(RanGen3));
      Theta := TwoPi * RanGen3;
      RanGaussStd := R * Cos(Theta);  { Return 1st number }
      GaussSave := R * Sin(Theta);    { Save 2nd number }
    end
  else
    RanGaussStd := GaussSave;         { Return saved number }
  GaussNew := not GaussNew;
end;

function RanGauss(Mu, Sigma : Float) : Float;
begin
  RanGauss := Mu + Sigma * RanGaussStd;
end;

end.
