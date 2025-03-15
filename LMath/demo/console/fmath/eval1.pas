program eval1;
{$mode ObjFPC}{$H-}
uses
  sysutils, crt, crtext, utypes, ueval;

var
  WinHeight: integer;
  WinWidth: integer;

function PrepareLine(S:string; Le:integer):string;
var
  L, I: integer;
begin
  L := length(S);
  for I := L+1 to Le do
    S := S + ' ';
  Result := S;
end;

procedure WriteInfo(Info:string);
var
  X,Y:integer;
begin
  Y := WhereY;
  X := WhereX;
  GoToXY(1,1);
  InvertColors;
  write(prepareLine(Info,WinWidth));
  NormalColors;
  GoToXY(X,Y);
end;

procedure Help;
var
  CurrentPos, EndPos: integer;
  Ch:char;
  HelpData:TStrVector;
  LinesCount:integer;

  procedure OutHelp;
  var
    J: integer;
    I: integer;
    OutStr:ansistring;
  begin
    GoToXY(1,2);
    OutStr := '';
    for I := 2 to Winheight-1 do
    begin
      J := CurrentPos-2+I;
      if J >= LinesCount then
      begin
        Dec(J);
        break;
      end;
      OutStr := OutStr + PrepareLine(HelpData[J],WinWidth);
    end;
    write(OutStr);
    EndPos := J;
    if EndPos < LinesCount-1 then
    begin
      GoToXY(1,WinHeight);
      inc(EndPos);
      write(HelpData[EndPos]);
      ClrEol;
    end;
    if WhereY < WinHeight then
    for I := WhereY to WinHeight do
    begin
      GoToXY(1,I);
      ClrEOL;
    end;
    writeInfo('Arrow keys, PgUp and PgDn to navigate, [Esc] to return to calculator');
    Finalize(OutStr);
  end;
var
  HelpFile:string;
  TheFile:TextFile;
Label EndIt;
begin
  DimVector(HelpData,100);
  HelpFile := ExtractFilePath(ParamStr(0));
  HelpFile := HelpFile + 'eval.txt';
  Assign(TheFile,HelpFile);
  {$I-}
  Reset(TheFile);
  if IOResult <> 0 then
  begin
    writeln('Error opening help file.');
    GoTo EndIt;
  end;
  LinesCount := 0;
  while not EOF(TheFile) do
  begin
    readln(TheFile,HelpData[LinesCount]);
    if IOResult <> 0 then
    begin
      writeln('Error reading help file.');
      Close(TheFile);
      GoTo EndIt;
    end;
    inc(LinesCount);
  end;
 {I+}
  Close(TheFile);
  CurrentPos := 0;
  EndPos := 0;
  CursorOff;
  ClrScr;
  OutHelp;
  repeat
    Ch := readkey;
    if Ch = #0 then
      Ch := ReadKey;
    case Ch of
      #$48: //arrow up
        if CurrentPos > 0 then
        begin
          dec(CurrentPos);
          GoToXY(1,2);
          InsLine;
          Write(HelpData[CurrentPos]);
          EndPos := CurrentPos + WinHeight - 2;
          if EndPos > LinesCount - 1 then
            EndPos := LinesCount - 1;
          writeInfo('Arrow keys, PgUp and PgDn to navigate, [Esc] to return to calculator');
        end;
      #$49: begin  //Page up
        CurrentPos := CurrentPos - WinHeight + 2;
        if CurrentPos < 0 then CurrentPos := 0;
        OutHelp;
      end;
      #$50: // down arrow
        if CurrentPos < LinesCount - 2 then
        begin
          inc(CurrentPos);
          GoToXY(WinWidth,WinHeight);
          writeln;
          if EndPos < LinesCount - 1 then
          begin
            inc(EndPos);
            write(HelpData[EndPos]);
          end;
          writeInfo('Arrow keys, PgUp and PgDn to navigate, [Esc] to return to calculator');
        end;
      #$51: begin// PgDn
        CurrentPos := EndPos;
        OutHelp;
      end;
    end;
  until Ch = #27;
  GoToXY(1,WinHeight);
  ClrEOL;
EndIt:
  CursorOn;
  Finalize(HelpData);
  writeln;
end;

function Evaluate(S:string):float;
var
  X:float;
  V:string;
  I:integer;
begin
  S := Trim(S);
  V := 'X';
  I := Pos(':=',S);
  if I > 0 then begin
    V := copy(S,1,I-1);
    V := Trim(V);
    S := copy(S,I+2,length(S));
  end;
  X := eval(S);
  if ParsingError then
    writeln('Invalid expression: ',S)
  else
    writeln(X:10:4);
  SetVariable(V,X);
  Result := X;
end;

var
  S:string;
  I:integer;
begin
  InitEval;
  S := '';
  for I := 1 to ParamCount do
    S := S + ParamStr(I);
  S := UpperCase(S);
  WinHeight := GetWinHeight;
  WinWidth := GetWinWidth;
  if (S = '/?') or (S = '/HELP') then
  begin
    ClrScr;
    Help;
    Exit;
  end;
  if (S <> '') then
  begin
     Evaluate(S);
     Exit;
  end;
  ClrScr;
  GoToXY(1,2);
  repeat
    write('>');
    WriteInfo('Type expression to evaluate. "q" to exit, "?" for help.');
    Readln(S);
    S := UpperCase(S);
    if (S = 'HELP') or (S = '?') then
       Help
    else if (S = 'EXIT') or (S = 'Q') then
       exit
    else
       Evaluate(S);
    WriteInfo('Type expression to evaluate. "q" to exit, "?" for help.');
  until false;
  DoneEval;
end.
