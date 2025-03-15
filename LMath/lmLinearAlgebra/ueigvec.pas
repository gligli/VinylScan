{ ******************************************************************
  Eigenvalues and eigenvectors of a general square matrix
  ****************************************************************** }

unit ueigvec;

interface

uses
  utypes, uErrors, ubalance, uelmhes, ueltran, uhqr2, ubalbak;

procedure EigenVect(A      : TMatrix;
                    Lb, Ub : Integer;
                    Lambda : TCompVector;
                    V      : TMatrix);

implementation

procedure EigenVect(A      : TMatrix;
                    Lb, Ub : Integer;
                    Lambda : TCompVector;
                    V      : TMatrix);
  var
    I_low, I_igh : Integer;
    Scale        : TVector;
    I_Int        : TIntVector;
  begin
    DimVector(Scale, Ub);
    DimVector(I_Int, Ub);

    Balance(A, Lb, Ub, I_low, I_igh, Scale);
    ElmHes(A, Lb, Ub, I_low, I_igh, I_int);
    Eltran(A, Lb, Ub, I_low, I_igh, I_int, V);
    Hqr2(A, Lb, Ub, I_low, I_igh, Lambda, V);

    if MathErr = 0 then
      BalBak(V, Lb, Ub, I_low, I_igh, Scale, Ub);
  end;

end.
