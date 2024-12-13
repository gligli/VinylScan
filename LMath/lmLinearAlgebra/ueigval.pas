{ ******************************************************************
  Eigenvalues of a general square matrix
  ****************************************************************** }

unit ueigval;

interface

uses
  utypes, ubalance, uelmhes, uhqr;

procedure EigenVals(A      : TMatrix;
                    Lb, Ub : Integer;
                    Lambda : TCompVector);

implementation

{procedure EigenVals(A, Lb, Ub, Lambda), defined in unit ueigval,
computes the eigenvalues of the real square matrix A[Lb..Ub, Lb..Ub].
Eigenvalues are stored in the complex vector Lambda. The real and
imaginary parts of the ith eigenvalue are stored in Lambda[i].X and
Lambda[i].Y, respectively. The eigenvalues are unordered, except that
complex conjugate pairs appear consecutively with the value having
the positive imaginary part first.}
procedure EigenVals(A      : TMatrix;
                    Lb, Ub : Integer;
                    Lambda : TCompVector);
  var
    I_low, I_igh : Integer;
    Scale        : TVector;
    I_int        : TIntVector;
  begin
    DimVector(Scale, Ub);
    DimVector(I_Int, Ub);

    Balance(A, Lb, Ub, I_low, I_igh, Scale);
    ElmHes(A, Lb, Ub, I_low, I_igh, I_int);
    Hqr(A, Lb, Ub, I_low, I_igh, Lambda);
  end;

end.
