unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Buttons, ComCtrls, Spin, lmcoordsys,
  uTypes, Transforms, lmPointsVec, lmnumericedits, lmRecursFilters, uRound, uMinMax, Types, uIntervals, uErrors;

type

  { TMainForm }

  PCoordsState = ^TCoordsState;
  TCoordsState = record
    Initiating : boolean; // just initiating line movement
    Borders : TInterval;
    DoingCursor : boolean; // making view windown range for X
    CursorHalfDone : boolean; // one borders of X view range was already found
    DoingBorders : boolean;   // same for Y
    BordersHalfDone : boolean;
    MouseCoords : TRealPoint;
    OldCoords : TRealPoint;
    DataCursor : TInterval;
  end;

  TMainForm = class(TForm)
    RefilterBtn: TButton;
    RippleEdit: TFloatSpinEdit;
    HighpassBox: TCheckBox;
    Freq3Edit: TFloatEdit;
    CornerFreqEdit: TFloatEdit;
    BandwidthEdit: TFloatEdit;
    Freq2Edit: TFloatEdit;
    Freq1Edit: TFloatEdit;
    FullScaleRSBtn: TButton;
    FullScaleFSBtn: TButton;
    FullScaleRFBtn: TButton;
    FullScaleFFBtn: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    NPolesEdit: TSpinEdit;
    XViewRSBtn: TButton;
    FilterBtn: TBitBtn;
    GendataBtn: TBitBtn;
    CloseBtn: TBitBtn;
    FilterBox: TComboBox;
    FilterBoxLabel: TLabel;
    SignalBoxLabel: TLabel;
    SignalBox: TComboBox;
    StaticText2: TStaticText;
    FilteredSignalCoords: TCoordSys;
    StaticText1: TStaticText;
    RawSignalCoords: TCoordSys;
    FilteredFourier: TCoordSys;
    RawFourier: TCoordSys;
    Fourierbtn: TBitBtn;
    PageControl1: TPageControl;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    TimeSheet: TTabSheet;
    FreqSheet: TTabSheet;
    ToolPanel: TPanel;
    XViewFSBtn: TButton;
    XViewFFBtn: TButton;
    XViewRFBtn: TButton;
    YViewRSBtn: TButton;
    YViewFSBtn: TButton;
    YViewRFBtn: TButton;
    YViewFFBtn: TButton;
    procedure FilterBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FourierbtnClick(Sender: TObject);
    procedure FullScaleFSBtnClick(Sender: TObject);
    procedure FullScaleRFBtnClick(Sender: TObject);
    procedure FullScaleFFBtnClick(Sender: TObject);
    procedure FullScaleRSBtnClick(Sender: TObject);
    procedure GendataBtnClick(Sender: TObject);
    procedure RawFourierDrawData(Sender: TObject);
    procedure FilteredFourierDrawData(Sender: TObject);
    procedure FilteredSignalCoordsDrawData(Sender: TObject);
    procedure RawFourierMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure RawSignalCoordsClick(Sender: TObject);
    procedure RawSignalCoordsDrawData(Sender: TObject);
    procedure RawSignalCoordsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure RawSignalCoordsMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean
      );
    procedure RefilterBtnClick(Sender: TObject);
    procedure SetNewCoords(Coords:TCoordSys;Points:TPoints);
    procedure XViewFSBtnClick(Sender: TObject);
    procedure XViewFFBtnClick(Sender: TObject);
    procedure XViewRFBtnClick(Sender: TObject);
    procedure XViewRSBtnClick(Sender: TObject);
    procedure YViewFSBtnClick(Sender: TObject);
    procedure YViewRFBtnClick(Sender: TObject);
    procedure YViewFFBtnClick(Sender: TObject);
    procedure YViewRSBtnClick(Sender: TObject);
    procedure SetData;
    procedure GetData;
  private
  public
    MarksPen : TPen;
    RSState, FSState, RFState, FFState : TCoordsState;
    ActiveState : PCoordsState;
  end;

var
  MainForm: TMainForm;
  RawTimeLine, FilteredTimeLine : TPoints;
  RawFourierPoints, FilteredFourierPoints : TPoints;
implementation

{$R *.lfm}
procedure MoveHorLine(Cs:TCoordSys; State: PCoordsState);
begin
  with Cs do
  begin
    Cs.Canvas.Pen.Mode := PMNotXor;
    with State^ do
    begin
      if not Initiating then
        PutLine(MinX,OldCoords.Y,MaxX,OldCoords.Y);
      PutLine(MinX,MouseCoords.Y,MaxX,MouseCoords.Y);
      Initiating := false;
    end;
  end;
end;

procedure MoveVertLine(Cs:TCoordSys; State: PCoordsState);
begin
  with Cs do
  begin
    Cs.Canvas.Pen.Mode := PMNotXor;
    with State^ do
    begin
      if not Initiating then
        PutLine(OldCoords.X,MinY,OldCoords.X,MaxY);
      PutLine(MouseCoords.X,MinY,MouseCoords.X,MaxY);
      Initiating := false;
    end;
  end;
end;

procedure TMainForm.RawSignalCoordsClick(Sender: TObject);
var
  Coords : TCoordSys;
begin
  Coords := Sender as TCoordSys;
  with ActiveState^ do
  begin
    if DoingCursor then
    begin
      if not CursorHalfDone then
      begin
        DataCursor.Lo := MouseCoords.X;
        Initiating := true;
        CursorHalfDone := true;
      end else
      begin
        DataCursor.Hi := MouseCoords.X;
        if DataCursor.Lo > DataCursor.Hi then
          Swap(DataCursor.Lo,DataCursor.Hi);
        Coords.MinX := DataCursor.Lo;
        Coords.MaxX := DataCursor.Hi;
        DoingCursor := false;
        CursorHalfDone := false;
        Coords.Invalidate;
      end;
    end else
    if DoingBorders then
    begin
      if not BordersHalfDone then
      begin
        Borders.Lo := MouseCoords.Y;
        Initiating := true;
        BordersHalfDone := true;
      end else
      begin
        Borders.Hi := MouseCoords.Y;
        if Borders.Lo > Borders.Hi then
          Swap(Borders.Lo,Borders.Hi);
        Coords.MinY := Borders.Lo;
        Coords.MaxY := Borders.Hi;
        DoingBorders := false;
        BordersHalfDone := false;
        Coords.Invalidate;
      end;
    end;
  end;
end;

procedure TMainForm.GendataBtnClick(Sender: TObject);
begin
  SetData;
  GenerateData(SignalBox.ItemIndex);
  RawTimeLine := TPoints.Combine(TimeVector,RawData,0,High(TimeVector));
  SetNewCoords(RawSignalCoords,RawTimeLine);
  RawSignalCoords.Repaint;
end;

procedure TMainForm.RawFourierDrawData(Sender: TObject);
begin
  if Assigned(RawFourierPoints) then
    RawFourier.FastDraw(RawFourierPoints.Points,0,N div 2 -1);
end;

procedure TMainForm.FilteredFourierDrawData(Sender: TObject);
begin
  if Assigned(FilteredFourierPoints) then
    FilteredFourier.FastDraw(FilteredFourierPoints.Points,0,N div 2 - 1);
end;

procedure TMainForm.FilteredSignalCoordsDrawData(Sender: TObject);
begin
  if assigned(FilteredTimeLine) then
    FilteredSignalCoords.FastDraw(FilteredTimeLine.Points,0,FilteredTimeLine.Count-1);
end;

procedure TMainForm.RawFourierMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
var
  TheCoords : TCoordSys;
begin
  TheCoords := Sender as TCoordSys;
  if ssCtrl in Shift then
  begin
    if ssShift in Shift then
      TheCoords.ReScale(1.0,1.1)
    else
      TheCoords.YScrollTo(TheCoords.MinY + 0.1*(TheCoords.MaxY-TheCoords.MinY));
  end else
  begin
    if ssShift in Shift then
      TheCoords.ReScale(1.1,1.0)
    else
      TheCoords.XScrollTo(TheCoords.MinX + 0.1*(TheCoords.MaxX-TheCoords.MinX));
  end;
end;

procedure TMainForm.RawSignalCoordsMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint;
  var Handled: Boolean);
var
  TheCoords : TCoordSys;
begin
  TheCoords := Sender as TCoordSys;
  if ssCtrl in Shift then
  begin
    if ssShift in Shift then
      TheCoords.ReScale(1.0,0.9)
    else
      TheCoords.YScrollTo(TheCoords.MinY - 0.1*(TheCoords.MaxY-TheCoords.MinY));
  end else
  begin
    if ssShift in Shift then
      TheCoords.ReScale(0.9,1.0)
    else
      TheCoords.XScrollTo(TheCoords.MinX - 0.1*(TheCoords.MaxX-TheCoords.MinX));
  end;
end;

procedure TMainForm.RefilterBtnClick(Sender: TObject);
begin
  SetData;
  FilterData(FilterBox.ItemIndex, true);
  if MathErr <> matOK then
  begin
    MessageDlg('Filtering error',MathErrMessage,mtError,[mbOK],'');
    Exit;
  end;
  FilteredTimeLine := TPoints.Combine(TimeVector,FilteredData,0,High(FilteredData));
  SetNewCoords(FilteredSignalCoords,FilteredTimeLine);
  FilteredSignalCoords.Repaint;
end;

procedure TMainForm.RawSignalCoordsDrawData(Sender: TObject);
begin
  if Assigned(RawTimeLine) then
    RawSignalCoords.FastDraw(RawTimeLine.Points,0,RawTimeLine.Count-1);
end;

procedure TMainForm.RawSignalCoordsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Coords : TCoordSys;
begin
  Coords := Sender as TCoordSys;
  if Coords = RawSignalCoords then
    ActiveState := @RSState
  else if Coords = FilteredSignalCoords then
    ActiveState := @FSState
  else if Coords = RawFourier then
    ActiveState := @RFState
  else if Coords = FilteredFourier then
    ActiveState := @FFState;
  with ActiveState^ do
  begin
    MouseCoords.X := Coords.XScreenToUser(X);
    MouseCoords.Y := Coords.YScreenToUser(Y);
    if DoingBorders then
    begin
      Coords.Canvas.Pen.Assign(MarksPen);
      MoveHorLine(Coords, ActiveState);
    end;
    if DoingCursor then
    begin
      Coords.Canvas.Pen.Assign(MarksPen);
      MoveVertLine(Coords, ActiveState);
    end;
    OldCoords := MouseCoords;
  end;
end;

procedure TMainForm.SetNewCoords(Coords:TCoordSys;Points:TPoints);
var
  MiY,MaY, MaxX, MiX : float;
  Margin : float;
begin
  if not assigned(Points) then
    Exit;
  MiY := Points.MinY;
  MaY := Points.MaxY;
  MaxX := Points.MaxX;
  MiX := Points.MinX;
  if SameValue(MiY,MaY) then
    Margin := 0.5
  else
    Margin := (MaY - MiY)/5;
  Coords.NewLimits(MiX,MiY-Margin,MaxX,MaY+Margin);
  Coords.XGridDist := floor((Coords.MaxX - Coords.MinX)/5);
  Coords.YGridDist := (Coords.MaxY - Coords.MinY) / 5;
  Coords.XGridNumbersDecimals := 2;
  Coords.YGridNumbersDecimals := 2;
end;

procedure TMainForm.XViewFSBtnClick(Sender: TObject);
begin
  FSstate.DoingCursor := true;
  FSState.CursorHalfDone := false;
  FSState.Initiating := true;
end;

procedure TMainForm.XViewFFBtnClick(Sender: TObject);
begin
  FFState.DoingCursor := true;
  FFState.CursorHalfDone := false;
  FFState.Initiating := true;
end;

procedure TMainForm.XViewRFBtnClick(Sender: TObject);
begin
  RFState.DoingCursor := true;
  RFState.CursorHalfDone := false;
  RFState.Initiating := true;
end;

procedure TMainForm.XViewRSBtnClick(Sender: TObject);
begin
  RSstate.DoingCursor := true;
  RSState.CursorHalfDone := false;
  RSState.Initiating := true;
end;

procedure TMainForm.YViewFSBtnClick(Sender: TObject);
begin
  FSState.DoingBorders := true;
  FSState.BordersHalfDone := false;
  FSState.Initiating := true;
end;

procedure TMainForm.YViewRFBtnClick(Sender: TObject);
begin
  RFState.DoingBorders := true;
  RFState.BordersHalfDone := false;
  RFState.Initiating := true;
end;

procedure TMainForm.YViewFFBtnClick(Sender: TObject);
begin
  FFState.DoingBorders := true;
  FFState.BordersHalfDone := false;
  FFState.Initiating := true;
end;

procedure TMainForm.YViewRSBtnClick(Sender: TObject);
begin
  RSState.DoingBorders := true;
  RSState.BordersHalfDone := false;
  RSState.Initiating := true;
end;

procedure TMainForm.SetData;
begin
  SinFreq1 := Freq1Edit.Value;
  SinFreq2 := Freq2Edit.Value;
  SinFreq3 := Freq3Edit.Value;
  CutOffFreq := CornerFreqEdit.Value;
  BW := BandWidthEdit.Value;
  NPoles := NPolesEdit.Value;
  HighPass := HighPassBox.State = cbChecked;
  Ripple := RippleEdit.Value;
end;

procedure TMainForm.GetData;
begin
  Freq1Edit.Value               := SinFreq1;
  Freq2Edit.Value               := SinFreq2;
  Freq3Edit.Value               := SinFreq3;
  CornerFreqEdit.Value          := CutOffFreq;
  BandWidthEdit.Value           := BW;
  NPolesEdit.Value              := NPoles;
  RippleEdit.Value              := Ripple;
  if HighPass then
    HighPassBox.State := cbChecked
  else
    HighPassBox.State := cbUnchecked;
end;

procedure TMainForm.FourierbtnClick(Sender: TObject);
begin
  if Assigned(RawData) then
  begin
    Fourier;
    if assigned(RawFreqVector) then
      RawFourierPoints := TPoints.Combine(FreqScale,RawFreqVector,0,High(FreqScale));
    if Assigned(FFreqVector) then
      FilteredFourierPoints := TPoints.Combine(FreqScale,FFreqVector,0,High(FreqScale));
    SetNewCoords(RawFourier,RawFourierPoints);
    SetNewCoords(FilteredFourier,FilteredFourierPoints);
    RawFourier.Repaint;
    FilteredFourier.Repaint;
  end;
end;

procedure TMainForm.FullScaleFSBtnClick(Sender: TObject);
begin
  if assigned(FilteredTimeLine) then
    SetNewCoords(FilteredSignalCoords, FilteredTimeLine);
end;

procedure TMainForm.FullScaleRFBtnClick(Sender: TObject);
begin
  if assigned(RawFourierPoints) then
    SetNewCoords(RawFourier, RawFourierPoints);
end;

procedure TMainForm.FullScaleFFBtnClick(Sender: TObject);
begin
  if assigned(FilteredFourierPoints) then
    SetNewCoords(FilteredFourier, FilteredFourierPoints);
end;

procedure TMainForm.FullScaleRSBtnClick(Sender: TObject);
begin
  if assigned(RawTimeLine) then
   SetNewCoords(RawSignalCoords, RawTimeLine);
end;

procedure TMainForm.FilterBtnClick(Sender: TObject);
begin
  SetData;
  FilterData(FilterBox.ItemIndex, false);
  if MathErr <> matOK then
  begin
    MessageDlg('Filtering error',MathErrMessage,mtError,[mbOK],'');
    SetErrCode(matOK);
    Exit;
  end;
  FilteredTimeLine := TPoints.Combine(TimeVector,FilteredData,0,High(FilteredData));
  SetNewCoords(FilteredSignalCoords,FilteredTimeLine);
  FilteredSignalCoords.Repaint;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  MarksPen.Free;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  MarksPen := TPen.Create;
  MarksPen.Color := clRed;
  MarksPen.Mode := PMNotXor;
  GetData;
end;

end.

