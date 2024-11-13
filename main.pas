unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, scan2track;

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  s2t: TScan2Track;
begin
  s2t := TScan2Track.Create;
  try
    s2t.PNGFileName := 'data\think0.png';
    s2t.Run;
  finally
    s2t.Free;
  end;
end;

end.

