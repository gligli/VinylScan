unit main;

{$MODE Delphi}

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;

    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    SpinEdit4: TSpinEdit;
    SpinEdit5: TSpinEdit;
    SpinEdit6: TSpinEdit;

    Button1: TButton;
    Button2: TButton;

    Shape1: TShape;

    Edit1: TEdit;

    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);

  private
    procedure DisplayColor;
  public
  end;

var
  Form1: TForm1;

implementation

uses
  utypes, uhsvrgb;

{$R *.lfm}

var
  R, G, B : Byte;
  H, S, V : Float;

procedure TForm1.DisplayColor;
begin
  Shape1.Brush.Color := RGB(R, G, B);
  Edit1.Text := '#' + IntToHex(R, 2) + IntToHex(G, 2) + IntToHex(B, 2);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  H := SpinEdit1.Value;
  S := SpinEdit2.Value * 0.01;
  V := SpinEdit3.Value * 0.01;

  HSVtoRGB(H, S, V, R, G, B);

  SpinEdit4.Value := R;
  SpinEdit5.Value := G;
  SpinEdit6.Value := B;

  DisplayColor;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  R := SpinEdit4.Value;
  G := SpinEdit5.Value;
  B := SpinEdit6.Value;

  RGBtoHSV(R, G, B, H, S, V);

  SpinEdit1.Value := Round(H);
  SpinEdit2.Value := Round(S * 100);
  SpinEdit3.Value := Round(V * 100);

  DisplayColor;
end;


end.
