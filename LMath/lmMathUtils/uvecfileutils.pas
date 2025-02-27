unit uVecFileUtils;
{$mode objfpc}{$H+}{$I-}
interface
uses Classes, SysUtils, uTypes, uErrors, uStrings, uVectorHelper;

// Saves vector V beginning from Lb to Ub as text file. If Ub > High(V), vector till the end is saved
procedure SaveVecToText(FileName:string; V:TVector; Lb, Ub:integer);

// Loads a vector from file.
// Allocated is TVector with length Lb+(number of lines in  the file).
// All lines which cannot be parsed are silently skipped.
// Highloaded contains highest index which is really filled in.
function LoadVecFromText(FileName:string; Lb:integer; out RowsLoaded : integer) : TVector;

//saves rectangular slice of matrix M into a file
procedure SaveMatToText(FileName : string; M : TMatrix; delimiter : char; FirstCol, LastCol, FirstRow, LastRow : integer);

// loads a matrix from a file. If a field cannot be parsed as a float, it is substituted with MD.
// If a line contains only MD, it is supposed to be header or subheader and is silently skipped.
function LoadMatFromText(FileName: string; delimiter: char; Lb:integer; MD:Float; out RowsLoaded:integer): TMatrix;

implementation

procedure SaveVecToText(FileName: string; V:TVector; Lb, Ub:integer);
var
  OutFile:Text;
  I:integer;
begin
  if (Lb > Ub) or (Lb < 0) then
  begin
    SetErrCode(MatErrDim);
    Exit;
  end;
  if Ub > High(V) then
    Ub := High(V);
  Assign(OutFile,FileName);
  Rewrite(OutFile);
  if IOResult <> 0 then
  begin
    SetErrCode(lmFileError,'Could not open file '+FileName+' for output.');
    Exit;
  end;
  for I := Lb to Ub do
  begin
    writeln(OutFile,FloatStr(V[I]));
    if IOResult <> 0 then
    begin
      SetErrCode(lmFileError,'Error writing file '+FileName);
      Close(OutFile);
      Break;
    end;
  end;
  Close(OutFile);
end;

function LoadVecFromText(FileName: string; Lb:integer; out RowsLoaded : integer): TVector;
var
  InFile:Text;
  I,L:integer;
  V:TVector;
  S:String;
  F:Float;
begin
  Result := nil;
  if Lb < 0 then
  begin
    SetErrCode(MatErrDim);
    Exit;
  end;
  Assign(InFile, FileName);
  Reset(InFile);
  if IOResult <> 0 then
  begin
    SetErrCode(lmFileError,'Could not open file '+FileName+' for reading.');
    Exit;
  end;
  L := 0;
  while not Eof(InFile) do
  begin  // count lines
    readln(InFile);
    if IOResult <> 0 then
    begin
      SetErrCode(lmFileError,'Error reading file '+FileName);
      Exit;
    end;
    inc(L);
  end;
  if L = 0 then
    Exit;
  Close(InFile);
  DimVector(V,Lb+L-1);
  Reset(InFile);
  I := Lb;
  while not Eof(InFile) do
  begin
    readln(InFile,S);
    if IOResult <> 0 then
    begin
      SetErrCode(lmFileError,'Error reading file '+FileName);
      Break;
    end;
    if TryStrToFloat(Trim(S),F) then
    begin
      V[I] := F;
      inc(I);
    end;
  end;
  Result := V;
  RowsLoaded := I-Lb;
  Close(InFile);
end;

procedure SaveMatToText(FileName: string; M:TMatrix; delimiter: char; FirstCol, LastCol, FirstRow, LastRow : integer);
var
  OutFile:Text;
  S:String;
  I,J:integer;
begin
  if LastCol > High(M[1]) then
    LastCol := High(M[1]);
  if LastRow > High(M) then
    LastRow := High(M);
  if (FirstCol > LastCol) or (FirstRow > LastRow) or (FirstCol < 0) or (FirstRow < 0) then
  begin
    SetErrCode(MatErrDim);
    Exit;
  end;
  Assign(OutFile,FileName);
  Rewrite(OutFile);
  if IOResult <> 0 then
  begin
    SetErrCode(lmFileError,'Could not open file '+FileName+' for output.');
    Exit;
  end;
  for I := FirstRow to LastRow do
  begin
    S := '';
    for J := FirstCol to LastCol-1 do
      S := S + FloatToStr(M[I,J]) + delimiter;
    S := S + FloatToStr(M[I,LastCol]);
    writeln(OutFile,S);
    if IOResult <> 0 then
    begin
      SetErrCode(lmFileError,'Error writing file '+FileName);
      Close(OutFile);
      Break;
    end;
  end;
  Close(OutFile);
end;

function LoadMatFromText(FileName: string; delimiter: char; Lb: integer; MD: Float; out RowsLoaded: integer): TMatrix;
var
  I,J,L,K,Len:integer;
  M:TMatrix;
  V:TVector;
  F:Float;
  Strings:TStringList;
  FieldsCount:integer;
  Fields:TStrVector;
begin
  Result := nil;
  try
    Strings := TStringList.Create;
    Strings.LoadFromFile(FileName);
  except
    On Exception do
    begin
      SetErrCode(lmFileError, 'Error reading file '+FileName);
      if Strings.Count = 0 then
      begin
        FreeAndNil(Strings);
        Exit;
      end;
    end;
  end;
  FieldsCount := 1;
  for I := 0 to Strings.Count - 1 do
  begin
    L := Length(Strings[I]);
    Len := 1;
    for J := 1 to L do
      if Strings[I][J] = delimiter then
        inc(Len);
    if Len > FieldsCount then
      FieldsCount := Len;
  end;
  SetLength(M,Lb+Strings.Count,FieldsCount+Lb);
  SetLength(V,FieldsCount);
  SetLength(Fields,FieldsCount);
  K := Lb;
  for I := 0 to Strings.Count - 1 do
  begin
    Parse(Strings[I],delimiter,Fields,Len);
    L := 0;
    for J := 0 to Len-1 do
      if TryStrToFloat(Trim(Fields[J]),F) then
      begin
        V[J] := F;
        inc(L);
      end else
        V[J] := MD;
    if L > 0 then // if there are valid data then
    begin
      M[K].InsertFrom(V,0,FieldsCount-1,Lb);
      inc(K);            // increase index of line
    end;
  end; // for with Strings.Count
  if K = Lb then
    Finalize(M);
  Finalize(V);
  FreeAndNil(Strings);
  Result := M;
  RowsLoaded := K - Lb;
end;

end.
