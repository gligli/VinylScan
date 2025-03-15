unit main;

{$MODE Delphi}

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin, Grids;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    SpinEdit1: TSpinEdit;
    StringGrid1: TStringGrid;
    StringGrid2: TStringGrid;
    StringGrid3: TStringGrid;
    StringGrid4: TStringGrid;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    SaveDialog2: TSaveDialog;
    RadioGroup1: TRadioGroup;

    procedure FormActivate(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);

  private
    procedure SetGridSizes;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  { DMath units }
  utypes, uErrors, ulineq, ulu, uqr, usvd, ustrings,
  { Dialogs }
  Format;

{ ------------------------------------------------------------------
  Functions for solving linear system
  ------------------------------------------------------------------ }

function Gauss_Solver(A : TMatrix; B : TVector; N : Integer; X : TVector) : Integer;
{ Solves system AX = B by Gauss-Jordan method - A and B are destroyed }
var
  Det : Float;
  I   : Integer;
begin
  LinEq(A, B, 0, N, Det);
  for I := 0 to N do
    X[I] := B[I];
  Result := MathErr;
end;

function LU_Solver(A : TMatrix; B : TVector; N : Integer; X : TVector) : Integer;
{ Solves system AX = B by LU decomposition - A is destroyed }
begin
  LU_Decomp(A, 0, N);
  if MathErr = 0 then LU_Solve(A, B, 0, N, X);
  Result := MathErr;
end;

function QR_Solver(A : TMatrix; B : TVector; N : Integer; X : TVector) : Integer;
{ Solves system AX = B by QR decomposition - A is destroyed }
var
  R : TMatrix;
begin
  DimMatrix(R, N, N);
  QR_Decomp(A, 0, N, N, R);
  if MathErr = 0 then QR_Solve(A, R, B, 0, N, N, X);
  Result := MathErr;
end;

function SVD_Solver(A : TMatrix; B : TVector; N : Integer; X : TVector) : Integer;
{ Solves system AX = B by singular value decomposition - A is destroyed }
var
  V : TMatrix;
  S : TVector;
begin
  DimMatrix(V, N, N);
  DimVector(S, N);
  SV_Decomp(A, 0, N, N, S, V);
  if MathErr = 0 then SV_Solve(A, S, V, B, 0, N, N, X);
  Result := MathErr;
end;

{ ------------------------------------------------------------------
  Main procedures
  ------------------------------------------------------------------ }

procedure TForm1.SetGridSizes;
var
  N : Integer;
begin
  N := SpinEdit1.Value;
  StringGrid1.RowCount := N;
  StringGrid1.ColCount := N;
  StringGrid2.RowCount := N;
  StringGrid3.RowCount := N;
  StringGrid4.RowCount := N;
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  SpinEdit1.Value := 3;
  SetGridSizes;
end;

procedure TForm1.SpinEdit1Change(Sender: TObject);
begin
  SetGridSizes;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  F       : TextFile;
  N, I, J : Integer;
  X       : Float;
begin
  if not OpenDialog1.Execute then Exit;

  AssignFile(F, OpenDialog1.FileName);
  Reset(F);

  Readln(F, N);
  with SpinEdit1 do
    if not (N in [MinValue..MaxValue]) then
      begin
        MessageDlg('System order must be between ' + IntToStr(MinValue) +
                   ' and'  + IntToStr(MaxValue), mtError, [mbOK], 0);
        Exit;
      end;

  SpinEdit1.Value := N;
  SetGridSizes;

  for I := 0 to Pred(N) do
    begin
      for J := 0 to Pred(N) do
        begin
          Read(F, X);
          StringGrid1.Cells[J, I] := FloatToStr(X);
        end;
      Readln(F, X);
      StringGrid2.Cells[0, I] := FloatToStr(X);
    end;

  CloseFile(F);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  F       : TextFile;
  N, I, J : Integer;
begin
  if not SaveDialog1.Execute then Exit;

  AssignFile(F, SaveDialog1.FileName);
  Rewrite(F);

  N := SpinEdit1.Value;
  Writeln(F, N);

  for I := 0 to Pred(N) do
    begin
      for J := 0 to Pred(N) do
        Write(F, StringGrid1.Cells[J, I], #9);
      Writeln(F, StringGrid2.Cells[0, I]);
    end;

  CloseFile(F);
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  N, I, J  : Integer;
  B, X, AX : TVector;
  A, AA    : TMatrix;
  ErrCode  : Integer;
begin
  N := Pred(SpinEdit1.Value);  { Arrays will be [0..N] }

  DimMatrix(A, N, N);
  DimMatrix(AA, N, N);

  DimVector(B, N);
  DimVector(X, N);
  DimVector(AX, N);

  { Read system }
  for I := 0 to N do
    begin
      for J := 0 to N do
        A[I, J] := StrToFloat(StringGrid1.Cells[J, I]);
      B[I] := StrToFloat(StringGrid2.Cells[0, I]);
    end;

  { Save a copy of the system matrix }
  for I := 0 to N do
    for J := 0 to N do
      AA[I, J] := A[I, J];

  { Solve system - The original matrix A is destroyed }
  case RadioGroup1.ItemIndex of
    0 : ErrCode := Gauss_Solver(A, B, N, X);
    1 : ErrCode := LU_Solver(A, B, N, X);
    2 : ErrCode := QR_Solver(A, B, N, X);
    3 : ErrCode := SVD_Solver(A, B, N, X);
  end;

  case ErrCode of
    MatSing    : MessageDlg('Singular matrix', mtError, [mbOK], 0);
    MatNonConv : MessageDlg('Non-convergence', mtError, [mbOK], 0);
  end;

  if ErrCode <> 0 then Exit;

  { Compute vector AX }
  for I := 0 to N do
    begin
      AX[I] := 0.0;
      for J := 0 to N do
        AX[I] := AX[I] + AA[I, J] * X[J];
    end;

  { Display results }
  for I := 0 to N do
    begin
      StringGrid3.Cells[0, I] := FloatStr(X[I]);
      StringGrid4.Cells[0, I] := FloatStr(AX[I]);
    end;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  F    : TextFile;
  N, I : Integer;
begin
  if not SaveDialog2.Execute then Exit;

  AssignFile(F, SaveDialog2.FileName);
  Rewrite(F);

  N := SpinEdit1.Value;
  Writeln(F, N);

  for I := 0 to Pred(N) do
    Writeln(F, StringGrid3.Cells[0, I]);

  CloseFile(F);
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  FormatDlg.ShowModal;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  Form1.Close;
end;

end.
