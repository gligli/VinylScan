unit scan2points;

{$include 'compileroptions.inc'}

interface

uses
  Classes, SysUtils, Types, Math,
  utils, scan2track;

type

  { TScan2Points }

  TScan2Points = class
  private
    FScan2TrackRef: TScan2Track;
    FDPI: Integer;
    FTrackWH: Integer;

    FTrack: TPolarPointValueDynArray;
    FTrackImage: TDoubleDynArray;
    FTrackAccumulator: Double;
    FTrackPos: Integer;
    FTrackFailCount: Integer;

    function DrawTrackSample(Sender: TScan2Track; Sample, X, Y: Double; var Radius: Double; Percent: Double;
      Finished: Boolean): Boolean;

  public
    constructor Create(AScan2Track: TScan2Track);

    procedure BuildTrackPoints;

    property DPI: Integer read FDPI;
    property TrackWH: Integer read FTrackWH;

    property Track: TPolarPointValueDynArray read FTrack;
    property TrackImage: TDoubleDynArray read FTrackImage;

    property TrackPos: Integer read FTrackPos;
    property TrackFailCount: Integer read FTrackFailCount;
  end;


implementation
uses main;

{ TScan2Points }

function TScan2Points.DrawTrackSample(Sender: TScan2Track; Sample, X, Y: Double; var Radius: Double; Percent: Double; Finished: Boolean): Boolean;
const
  COverdrawLimit = 7.0;
var
  ppv: TPolarPointValue;
  pv: TPointValue;
  ix, iy, yx: Integer;
  odLimit, ox, oy: Double;
begin
  ox := X - Sender.Scan.Center.X;
  oy := Y - Sender.Scan.Center.Y;

  ppv.Radius := Sqrt(Sqr(oy) + Sqr(ox));
  ppv.Angle := ArcTan2(oy, ox);
  SinCos(ppv.Angle, ppv.Sin, ppv.Cos);
  ppv.Value := Sample;

  pv.X := ox + FTrackWH * 0.5;
  pv.Y := oy + FTrackWH * 0.5;
  pv.Value := ppv.Value;
  DrawPointValue(FTrackImage, pv, FTrackWH, FTrackWH, PositiveValue);

  ix := Trunc(pv.X);
  iy := Trunc(pv.Y);
  yx := iy * FTrackWH + ix;

  odLimit := COverdrawLimit;
  if FTrackPos >= Sender.PointsPerRevolution then
    odLimit *= Sqrt(FTrackAccumulator / FTrackPos);

  // no overdraw? (would be due to skips)
  Result := InRange(FTrackImage[yx], -odLimit, odLimit) and
      InRange(FTrackImage[yx + 1], -odLimit, odLimit) and
      InRange(FTrackImage[yx + FTrackWH], -odLimit, odLimit) and
      InRange(FTrackImage[yx + FTrackWH + 1], -odLimit, odLimit);

  if Result then
  begin
    FTrackAccumulator += Sqr(ppv.Value);

    //ppv.Value += 1.0;

    FTrack[FTrackPos] := ppv;
    Inc(FTrackPos);

    Assert(FTrackPos <= Length(FTrack));
  end
  else
  begin
    Radius -= C45RpmRecordingGrooveWidth * FDPI;
    Inc(FTrackFailCount);

    Result := True;
  end;
end;

constructor TScan2Points.Create(AScan2Track: TScan2Track);
begin
  FScan2TrackRef := AScan2Track;
  FDPI := FScan2TrackRef.Scan.DPI;
  FTrackWH := Round(C45RpmOuterSize * FDPI) + 1;
end;

procedure TScan2Points.BuildTrackPoints;
begin
  SetLength(FTrackImage, Sqr(FTrackWH));
  try
    FTrackAccumulator := 0.0;
    FTrackPos := 0;
    FTrackFailCount := 0;
    SetLength(FTrack, FScan2TrackRef.PointsPerRevolution * FScan2TrackRef.Scan.Width div 2);

    FScan2TrackRef.OnSample := @DrawTrackSample;

    FScan2TrackRef.EvalTrack;

    SetLength(FTrack, FTrackPos);

    //main.MainForm.DrawImage(FTrackImage, FTrackWH, FTrackWH);
    //Sleep(10000);
  finally
    SetLength(FTrackImage, 0);
  end;
end;

end.

