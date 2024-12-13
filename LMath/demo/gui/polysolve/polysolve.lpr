program polysolve;

{$MODE Delphi}

uses
  Forms, Interfaces,
  main in 'main.pas' {Form1},
  format in '..\dialogs\format.pas' {FormatDlg};

{.$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TFormatDlg, FormatDlg);
  Application.Run;
end.
