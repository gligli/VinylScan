program ProbCalc;

{$MODE Delphi}

uses
  Forms, Interfaces,
  Unit1 in 'UNIT1.PAS' {Form1};

{.$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
