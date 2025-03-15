unit Param;

{$MODE Delphi}

interface

uses
  { Controls }
  LCLIntf, LCLType, LMessages, Classes, Graphics, Forms, Controls, Buttons,
  StdCtrls, Grids,
  { DMath unit }
  utypes, umodels;

type
  TParamDlg = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    StringGrid1: TStringGrid;
    procedure HelpBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure SetModel(Model : TModel);
    procedure SetParam(B : TVector);
  end;

var
  ParamDlg: TParamDlg;

implementation

uses
  ustrings, uwinstr, unlfit, Help;

{$R param.lfm}

const
  HelpLines = 7;

  HelpText : array[1..HelpLines] of String[68] = (
  '1. If necessary, modify the initial parameter values estimated',
  '   by the program (column "Value").',
  '',
  '2. If necessary, modify the parameter bounds (columns "Minimum"',
  '   and "Maximum").',
  '',
  'To modify a value, click on its cell and enter the new value.');

var
  gModel : TModel;
  gB     : TVector;

procedure TParamDlg.FormCreate(Sender: TObject);
begin
  StringGrid1.Cells[0,0] := 'Parameter';
  StringGrid1.Cells[1,0] := 'Value';
  StringGrid1.Cells[2,0] := 'Minimum';
  StringGrid1.Cells[3,0] := 'Maximum';
end;

procedure TParamDlg.FormActivate(Sender: TObject);
var
  FirstPar,
  LastPar    : Integer;
  I, Row     : Integer;
  Bmin, Bmax : Float;
begin
  FirstPar := FirstParam(gModel);
  LastPar := LastParam(gModel);

  StringGrid1.RowCount := LastPar - FirstPar + 2;

  for Row := 1 to Pred(StringGrid1.RowCount) do
    begin
      I := Row + FirstPar - 1;
      GetParamBounds(I, Bmin, Bmax);
      SetFormat(12, 2, True, False);
      StringGrid1.Cells[0, Row] := ParamName(gModel, I);
      StringGrid1.Cells[1, Row] := FloatStr(gB[I]);
      StringGrid1.Cells[2, Row] := FloatStr(Bmin);
      StringGrid1.Cells[3, Row] := FloatStr(Bmax);
      SetFormat(10, 4, False, False);
    end;
end;

procedure TParamDlg.OKBtnClick(Sender: TObject);
var
  FirstPar   : Integer;
  Row, I     : Integer;
  S1, S2, S3 : String;
  X1, X2, X3 : Float;
begin
  FirstPar := FirstParam(gModel);
  for Row := 1 to Pred(StringGrid1.RowCount) do
    begin
      I := Row + FirstPar - 1;

      S1 := StringGrid1.Cells[1, Row];
      S2 := StringGrid1.Cells[2, Row];
      S3 := StringGrid1.Cells[3, Row];

      if IsNumeric(S1, X1) then gB[I] := X1;
      if IsNumeric(S2, X2) and IsNumeric(S3, X3) then
        SetParamBounds(I, X2, X3);
    end;
end;

procedure TParamDlg.HelpBtnClick(Sender: TObject);
var
  I : Integer;
begin
  HelpDlg.Caption := 'Initial parameters Help';
  HelpDlg.Memo1.Lines.Clear;
  for I := 1 to HelpLines do
    HelpDlg.Memo1.Lines.Add(HelpText[I]);
  HelpDlg.ShowModal;
end;

procedure TParamDlg.SetModel(Model : TModel);
begin
  gModel := Model;
end;

procedure TParamDlg.SetParam(B : TVector);
begin
  gB := B;
end;

end.
