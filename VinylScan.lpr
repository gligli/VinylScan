program VinylScan;

{$mode objfpc}{$H+}

uses
  tbbmalloc,
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main, scan2track, utils, powell, inputscan, scancorrelator,
  { you can add units after this }
  SysUtils, LCLType, Controls;

{$R *.res}

type

{ TEvtHolder }

TEvtHolder = class
  procedure AppException(Sender : TObject; E : Exception);
end;

procedure TEvtHolder.AppException(Sender: TObject; E: Exception);
var
  I: Integer;
  Frames: PPointer;
  Report: string;
begin
  Screen.Cursor := crDefault;
  Report := 'Program exception! ' + LineEnding +
    'Stacktrace:' + LineEnding + LineEnding;
  if E <> nil then begin
    Report := Report + 'Exception class: ' + E.ClassName + LineEnding +
    'Message: ' + E.Message + LineEnding;
  end;
  Report := Report + BackTraceStrFunc(ExceptAddr);
  Frames := ExceptFrames;
  for I := 0 to ExceptFrameCount - 1 do
    Report := Report + LineEnding + BackTraceStrFunc(Frames[I]);
  WriteLn(Report);
  TApplication(Sender).MessageBox(PChar(Report), PChar(Application.Title), MB_ICONERROR);
end;

var
  EvtHolder: TEvtHolder;
begin
  EvtHolder := TEvtHolder.Create;
  try
    RequireDerivedFormResource:=True;
  Application.Scaled := True;
    Application.Initialize;
    Application.CreateForm(TMainForm, MainForm);
    Application.OnException := @EvtHolder.AppException;
    Application.Run;
  finally
    EvtHolder.Free;
  end;
end.

