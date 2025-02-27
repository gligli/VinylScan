unit main;

{$MODE Delphi}

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    Edit2: TEdit;
    Label3: TLabel;
    Edit3: TEdit;
    Label4: TLabel;
    Edit4: TEdit;
    Label5: TLabel;
    Edit5: TEdit;
    Label6: TLabel;
    Edit6: TEdit;
    Label7: TLabel;
    Edit7: TEdit;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Image1: TImage;
    GroupBox1: TGroupBox;
    Label8: TLabel;
    Edit8: TEdit;
    Label9: TLabel;
    Edit9: TEdit;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;

    procedure FormActivate(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    procedure SetCaption;
  public
    { Dйclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  utypes, ucomplex, uhsvrgb, ustrings;

{ ---------------------------------------------------------
  Constants and global variables
 ---------------------------------------------------------- }

const
  PicWidth      = 640;
  PicHeight     = 480;
  HalfPicWidth  = PicWidth div 2;
  HalfPicHeight = PicHeight div 2;
  Scale         = PicHeight;
  Esc           = 1.0E+10;       { Escape radius }

var
  p         : Float   = 2.0;     { Exponent }
  x0        : Float   = -0.75;
  y0        : Float   = 0.0;     { Coord. of picture center }
  MaxIter   : Integer = 200;     { Max. number of iterations }
  ZoomFact  : Float   = 1.75;    { Zoom factor }
  ScaleFact : Float   = 0.005;   { Distance between 2 pixels }
  DistFact  : Float   = -1.0;    { Factor for distance est. }
  ColorFact : Float   = -2.0;    { Color factor }
  AbsColor  : Float   = 2.0;     { Abs(ColorFact) }
  Julia     : Boolean = False;   { Julia set ? }
  p1        : Float   = 1.0;     { p - 1 }
  Lnp       : Float   = 0.693;   { ln(p) }
  LLE       : Float   = 7.847;   { ln(ln(Esc)) / ln(p) }
  c_X       : Float   = 0.0;
  c_Y       : Float   = 0.0;     { c param. for Julia set }

var
  Iter  : Integer;  { Iteration count }
  Dist  : Float;    { Distance estimator }
  Dwell : Float;    { Continuous dwell }

{ ---------------------------------------------------------
  Internal subroutines
  --------------------------------------------------------- }

function MdbCol : Integer;
var
  DScale, Angle, Radius, P, H, S, V : Float;
  R, G, B : Byte;
begin
  DScale := Ln(Dist / ScaleFact) / Lnp + DistFact;

  if DScale > 0 then
    V := 1
  else if DScale > -8 then
    V := (8 + DScale) / 8
  else
    V := 0;
  
  P := Ln(Dwell) * AbsColor;
  
  if P < 0.5 then
    begin
      P := 1.0 - 1.5 * P;
      Angle := 1.0 - P;
    end  
  else
    begin
      P := 1.5 * P - 0.5;
      Angle := P;
    end;  

  Radius := Sqrt(P);

  if (Odd(Iter)) and (ColorFact > 0) then
    begin
      V := 0.85 * V;
      Radius := 0.667 * Radius;
    end;

  H := Frac(Angle * 10);
  S := Frac(Radius);
   
  HSVtoRGB(H * 360, S, V, R, G, B);
  
  Result := RGB(R, G, B);
end;

function Mandelbrot(xt, yt : Float) : Integer;
{ Performs the iterations at point (xt, yt) and returns the color }
var
  c, z, zp1, zn, dz, dzn : Complex;
  Module, LnMod, DMod    : Float;
begin
  if Julia then
    begin
      c  := Cmplx(c_X, c_Y);
      z  := Cmplx(xt, yt);
      dz := Cmplx(1, 0);
    end
  else
    begin
      c  := Cmplx(xt, yt);
      z  := Cmplx(0, 0);
      dz := z;
    end;

  Iter := 0;
  Module := CAbs(z);

  while (Iter < MaxIter) and (Module < Esc) do
    begin
      zp1 := CRealPower(z, p1);      { z^(p-1) }

      zn   := z * zp1;
      zn.X := zn.X + c.X;
      zn.Y := zn.Y + c.Y;            { z(n+1) = z(n)^p + c }

      dzn   := zp1 * dz;        { Derivative : dz(n+1)/dc }
      dzn.X := p * dzn.X;
      dzn.Y := p * dzn.Y;

      if not Julia then dzn.X := dzn.X + 1.0;

      Module := CAbs(zn);

      z := zn;
      dz := dzn;
      Inc(Iter);
   end;

  if Iter = MaxIter then
    begin
      Result := clWhite;  { Color the set in white }
      Exit;
    end;

  LnMod := Ln(Module); DMod := CAbs(dzn);
  if DMod > 0.0 then Dist  := p * Module * LnMod / DMod;
  Dwell := Iter - Ln(LnMod) / Lnp + LLE;

  Result := MdbCol;
end;

procedure GetCoord(x0, y0 : Float; Nx, Ny : Integer; var x, y : Float);
{ Computes the user coordinates (x, y) from the pixel coord. (Nx, Ny) }
begin
  x := x0 + ScaleFact * (Nx - HalfPicWidth);
  y := y0 - ScaleFact * (Ny - HalfPicHeight);
end;

function SetFractal(Exponent, x_Julia, y_Julia : Float) : Boolean;
begin
  if Exponent <= 1 then
    begin
      Result := False;
      Exit;
    end;

  p   := Exponent;
  p1  := p - 1;
  Lnp := Ln(p);
  LLE := Ln(Ln(Esc)) / Lnp;

  c_X := x_Julia;
  c_Y := y_Julia;

  Result := True;
end;

procedure SetGraphParams(x_Center,
                         y_Center       : Float;
                         Max_Iterations : Integer;
                         Zoom_Factor,
                         Dist_Factor,
                         Color_Factor   : Float);
begin
  x0        := x_Center;
  y0        := y_Center;
  MaxIter   := Max_Iterations;
  ZoomFact  := Zoom_Factor;
  DistFact  := Dist_Factor;
  ColorFact := 0.01 * Color_Factor;
  AbsColor  := Abs(ColorFact);
  ScaleFact := 4.0 / (Scale * ZoomFact);
end;

{ ---------------------------------------------------------
  Main subroutines
  --------------------------------------------------------- }

procedure TForm1.SetCaption;
begin
  if Julia then
    Button4.Caption := 'Mandelbrot / &Julia'
  else
    Button4.Caption := '&Mandelbrot / Julia';
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  { Display center coordinates in exponential format
    using function FloatStr from unit ustrings }
  SetFormat(27, 17, True, False);
  Edit2.Text := FloatStr(x0);
  Edit3.Text := FloatStr(y0);

  { Display other parameters with Delphi FloatToStr }
  Edit1.Text := FloatToStr(p);
  Edit4.Text := IntToStr(MaxIter);
  Edit5.Text := FloatToStr(ZoomFact);
  Edit6.Text := FloatToStr(DistFact);
  Edit7.Text := FloatToStr(ColorFact);
  Edit8.Text := FloatToStr(c_X);
  Edit9.Text := FloatToStr(c_Y);

  GroupBox1.Visible := Julia;
  Form1.SetCaption;
end;

procedure TForm1.Image1Click(Sender: TObject);
{ Get the mouse coordinates }
var
  P    : TPoint;
  X, Y : Float;
begin
  GetCursorPos(P) ;
  P := ScreenToClient(P);

  GetCoord(x0, y0, P.X - Image1.Left, P.Y - Image1.Top, X, Y);

  Edit2.Text := FloatStr(X);
  Edit3.Text := FloatStr(Y);
end;

procedure TForm1.Button1Click(Sender: TObject);
{ Plots the Mandelbrot or Julia set }
var
  Nx, Ny : Integer;
  xt, yt : Float;
begin
  if not SetFractal(StrToFloat(Trim(Edit1.Text)),
                    StrToFloat(Trim(Edit8.Text)),
                    StrToFloat(Trim(Edit9.Text))) then
    begin
      MessageDlg('The exponent p must be >= 1', mtError, [mbOk], 0);
      Exit;
    end;

  SetGraphParams(StrToFloat(Trim(Edit2.Text)),
                 StrToFloat(Trim(Edit3.Text)),
                 StrToInt(Trim(Edit4.Text)),
                 StrToFloat(Trim(Edit5.Text)),
                 StrToFloat(Trim(Edit6.Text)),
                 StrToFloat(Trim(Edit7.Text)));

  for Ny := 0 to Pred(PicHeight) do
    begin
      yt := y0 - ScaleFact * (Ny - HalfPicHeight);
      for Nx := 0 to Pred(PicWidth) do
        begin
          xt := x0 + ScaleFact * (Nx - HalfPicWidth);
          Image1.Canvas.Pixels[Nx, Ny] := Mandelbrot(xt, yt);
        end;
    end;

  Image1.Visible := True;
end;

procedure TForm1.Button2Click(Sender: TObject);
{ Load picture and parameters }
var
  F        : TextFile;
  FileName : string;
  Ok       : Boolean;
begin
  if not OpenDialog1.Execute then Exit;

  FileName := OpenDialog1.FileName;
  AssignFile(F, FileName);
  Reset(F);

  Readln(F, p);
  Readln(F, x0);
  Readln(F, y0);
  Readln(F, MaxIter);
  Readln(F, ZoomFact);
  Readln(F, DistFact);
  Readln(F, ColorFact);
  Readln(F, c_X);
  Readln(F, c_Y);

  CloseFile(F);

  Julia := (c_X <> 0.0) or (c_Y <> 0.0);

  { Display parameters }
  Form1.FormActivate(Sender);

  { Load image if present }
  FileName := ChangeFileExt(OpenDialog1.FileName, '.bmp');
  Ok := FileExists(FileName);
  if Ok then Image1.Picture.LoadFromFile(FileName);
  Image1.Visible := Ok;
end;

procedure TForm1.Button3Click(Sender: TObject);
{ Save picture and parameters }
var
  F : TextFile;
begin
  if not SaveDialog1.Execute then Exit;

  AssignFile(F, SaveDialog1.FileName);
  Rewrite(F);

  Writeln(F, FloatStr(p));
  Writeln(F, FloatStr(x0));
  Writeln(F, FloatStr(y0));
  Writeln(F, MaxIter);
  Writeln(F, FloatStr(ZoomFact));
  Writeln(F, FloatStr(DistFact));
  Writeln(F, FloatStr(ColorFact * 100));
  Writeln(F, FloatStr(c_X));
  Writeln(F, FloatStr(c_Y));

  CloseFile(F);

  Image1.Picture.SaveToFile(ChangeFileExt(SaveDialog1.FileName, '.bmp'));
end;

procedure TForm1.Button4Click(Sender: TObject);
{ Toggle Mandelbrot / Julia }
begin
  Julia := not Julia;
  GroupBox1.Visible := Julia;
  Form1.SetCaption;

  if Julia then
    begin
      Edit8.Text := Edit2.Text;
      Edit9.Text := Edit3.Text;
      Edit2.Text := '0';
      Edit3.Text := '0';
    end
  else
    begin
      Edit2.Text := Edit8.Text;
      Edit3.Text := Edit9.Text;
      Edit8.Text := '0';
      Edit9.Text := '0';
    end;
end;

end.
