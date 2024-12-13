unit ModelForms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Spin, StdCtrls, Buttons,
  uTypes, lmnumericedits, uConstrNLFit, uVectorHelper, Globals;

type

  { TModelForm }

  TModelForm = class(TForm)
    Label5: TLabel;
    RhoEndEdit: TFloatEdit;
    RhoBeginEdit: TFloatEdit;
    Label3: TLabel;
    Label4: TLabel;
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    Label2: TLabel;
    MCV_text: TStaticText;
    OFCEdit: TSpinEdit;
    VariableEdit: TFloatEdit;
    Label1: TLabel;
    ConstrNumberText: TStaticText;
    VariablesList: TListBox;
    VarsNumberText: TStaticText;
    procedure FormShow(Sender: TObject);
    procedure VariableEditEditingDone(Sender: TObject);
    procedure VariablesListSelectionChange(Sender: TObject; User: boolean);
    procedure ReturnVars;
  end;

var
  ModelForm: TModelForm;
  OldItem:integer;
implementation

var
  Vars:TVector;

{$R *.lfm}

{ TModelForm }

procedure TModelForm.FormShow(Sender: TObject);
begin
  Vars := copy(Variables,0,length(Variables));
  VariablesList.Items.Clear;
  Vars.ToStrings(VariablesList.Items,1,High(Vars),false,';');
  OldItem := 1;
  VariableEdit.Value := Vars[OldItem];
  RhoEndEdit.Value   := Rho;
  RhoBeginEdit.Value := RhoBeg;
  OFCEdit.Value      := OFCalls;
  MCV_Text.Caption   := 'Maximal constraint violation: '+FloatToStr(MaxCV);
  VarsNumberText.Caption := 'Number of variables: ' + IntToStr(VarNum);
  ConstrNumberText.Caption := 'Number of constraints: ' + IntToStr(ConstrNum);
end;

procedure TModelForm.VariableEditEditingDone(Sender: TObject);
begin
  Vars[OldItem] := VariableEdit.Value;
end;

procedure TModelForm.VariablesListSelectionChange(Sender: TObject; User: boolean);
begin
  Vars[OldItem] := VariableEdit.Value;
  VariablesList.Items[OldItem-1] := Vars.ToString(OldItem);
  OldItem := VariablesList.ItemIndex + 1;
  VariableEdit.Value := Vars[OldItem];
  VariablesList.Invalidate;
end;

procedure TModelForm.ReturnVars;
begin
  Variables := copy(Vars,0,length(Vars));
  Rho := RhoEndEdit.Value;
  RhoBeg := RhoBeginEdit.Value;
  OFCalls := OFCEdit.Value;
end;

end.

