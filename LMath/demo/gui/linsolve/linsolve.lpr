program linsolve;

{$MODE Delphi}

uses
  Forms, Interfaces,
  main in 'main.pas' {Form1},
  format in '..\dialogs\format.pas' {FormatDlg};

{.$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TFormatDlg, FormatDlg);
  Application.Run;
end.
