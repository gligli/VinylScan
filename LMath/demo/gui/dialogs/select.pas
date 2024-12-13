unit Select;

interface

uses WinTypes, WinProcs, Classes, Graphics, Forms, Controls, Buttons,
  StdCtrls, ExtCtrls;

type
  TSelectDlg = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    Notebook1: TNotebook;
    CalcListBox: TListBox;
    GraphListBox: TListBox;
    procedure FormActivate(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;

var
  SelectDlg: TSelectDlg;

implementation

{$R *.DFM}

uses
  Help;

const
  HelpLines = 14;

  HelpText : array[1..HelpLines] of String[66] = (
  '1. Click on the item to select.',
  '   It will appear on a blue background.',
  '',
  '   To select several items: click on each of them',
  '   while pressing the <Ctrl> key.',
  '',
  '   To select consecutive items, you may extend the selection:',
  '',
  '   - by dragging the mouse while pressing the left button',
  '',
  '   - by pressing the <Up arrow> or <Down arrow> key',
  '     together with the UpperCase key.',
  '',
  '2. Click on OK to validate your selection, or on Cancel to quit.');

procedure TSelectDlg.FormCreate(Sender: TObject);
var
  I : Integer;
begin
  { Select all variables }
  for I := 0 to Pred(CalcListBox.Items.Count) do
    CalcListBox.Selected[I] := True;
  for I := 0 to Pred(GraphListBox.Items.Count) do
    GraphListBox.Selected[I] := True;
end;

procedure TSelectDlg.FormActivate(Sender: TObject);
begin
  Caption := NoteBook1.ActivePage;
  case NoteBook1.PageIndex of
    0 : CalcListBox.SetFocus;
    1 : GraphListBox.SetFocus;
  end;
end;

procedure TSelectDlg.HelpBtnClick(Sender: TObject);
var
  I : Integer;
begin
  HelpDlg.Caption := 'Selection Help';
  HelpDlg.Memo1.Lines.Clear;
  for I := 1 to HelpLines do
    HelpDlg.Memo1.Lines.Add(HelpText[I]);
  HelpDlg.ShowModal;
end;

procedure TSelectDlg.OKBtnClick(Sender: TObject);
var
  I : Integer;
begin
  { Select the same curves for fitting and plotting }
  if NoteBook1.PageIndex = 0 then
    begin
      GraphListBox.Items := CalcListBox.Items;
      for I := 0 to Pred(CalcListBox.Items.Count) do
        GraphListBox.Selected[I] := CalcListBox.Selected[I];
    end;
end;

end.
