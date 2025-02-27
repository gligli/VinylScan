unit Help;

{$MODE Delphi}

interface

uses LCLIntf, LCLType, LMessages, Classes, Graphics, Forms, Controls, Buttons,
  StdCtrls, ExtCtrls;

type
  THelpDlg = class(TForm)
    OKBtn: TBitBtn;
    Memo1: TMemo;
  end;

var
  HelpDlg: THelpDlg;

implementation

{$R help.lfm}

end.
