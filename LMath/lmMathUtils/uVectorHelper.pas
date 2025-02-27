unit uVectorHelper;
{$mode objfpc}
{$MODESWITCH TYPEHELPERS}
interface

uses
  Classes, uTypes, uErrors, uMinMax, uSorting, uStrings;

type

{ TVectorHelper }
     
TVectorHelper = type helper for TVector
  procedure Insert(value:Float; index:integer);
  procedure Remove(index:integer);
  procedure Swap(ind1,ind2:integer);
  procedure Clear;
  procedure Fill(Lb, Ub : integer; Val:Float);
  procedure FillWithArr(Lb : integer; Vals:array of Float);
  procedure Sort(Descending:boolean);
  //inserts Source[Lb,Ub] into self beginning from Ind position.
  procedure InsertFrom(Source:TVector; Lb, Ub: integer; ind:integer); overload;
  procedure InsertFrom(constref source: array of float; ind: integer); overload;
  // Converts one item to string representation
  function ToString(Index:integer):string;
  //Sends string representation of the subarray to Dest. If Indices, sends
  //indices as well
  function ToStrings(Dest:TStrings; First, Last:integer;
            Indices:boolean; Delimiter: char):integer;
end;

{ TIntVectorHelper }

TIntVectorHelper = type helper for TIntVector
  procedure Insert(value:Integer; index:integer);
  procedure Remove(index:integer);
  procedure Swap(ind1,ind2:integer);
  procedure Clear;
  procedure Fill(Lb, Ub : integer; Val:Integer);
  procedure FillWithArr(Lb : integer; Vals:array of Integer);
  procedure InsertFrom(Source:TIntVector; Lb, Ub: integer; ind:integer); overload;
  procedure InsertFrom(constref source: array of integer; ind: integer); overload;
  function ToString(Index:integer):string;
  //Sends string representation of the subarray to Dest. If Indices, sends
  //indices as well
  function ToStrings(Dest:TStrings; First, Last:integer;
            Indices:boolean; Delimiter: char):integer;
end;


implementation

{ TIntVectorHelper }

procedure TIntVectorHelper.Insert(value: Integer; index: integer);
var
  I: DWord;
begin
  if Index > High(self) then
  begin
    SetErrCode(MatErrDim);
    Exit;
  end;
  for I := High(self) downto index+1 do
    self[I] := self[I-1];
  self[Index] := value;
end;

procedure TIntVectorHelper.Remove(index: integer);
var
  I: Integer;
begin
  if Index > High(self) then
  begin
    SetErrCode(MatErrDim);
    Exit;
  end;
  for I := index to high(self)-1 do
    self[I] := self[I+1];
  self[high(self)] := 0;
end;

procedure TIntVectorHelper.Swap(ind1, ind2: integer);
var
  F:integer;
begin
  if (Ind1 > High(self)) or (Ind2 > High(Self)) then
  begin
    SetErrCode(MatErrDim);
    Exit;
  end;
  F := self[ind1];
  self[ind1] := self[ind2];
  self[ind2] := F;
end;

procedure TIntVectorHelper.Clear;
begin
  FillWord(Self[0],length(Self)*SizeOf(Integer) div 2,0);
end;

procedure TIntVectorHelper.Fill(Lb, Ub: integer; Val: Integer);
var
  I: Integer;
begin
  if Self = nil then
    DimVector(Self,Ub);
  for I := Lb to min(Ub, high(self)) do
    self[I] := Val;
end;

procedure TIntVectorHelper.FillWithArr(Lb: integer; Vals: array of Integer);
var
  L:integer;
begin
  if Self = nil then
    SetLength(Self,Lb + length(Vals));
  L := min(length(Self) - Lb,length(Vals));
  Move(Vals[0],Self[Lb],SizeOf(Integer)*L);
end;

procedure TIntVectorHelper.InsertFrom(Source: TIntVector; Lb, Ub: integer;
  ind: integer);
var
  I, C, H: Integer;
begin
  if Self = nil then
    DimVector(Self, ind + Ub - Lb);
  H := High(Self);
  if (Lb > High(Source)) or (Ind > H) then
  begin
    SetErrCode(MatErrDim);
    Exit;
  end;
  Ub := min(Ub,High(Source));
  C := min(Ub - Lb, H-Ind)+1;
  for I := high(self) downto ind + C do
    self[I] := self[I-C]; // moving existing values freeing place for newly inserted
  Move(Source[Lb],Self[Ind],C*SizeOf(Integer));
end;

procedure TIntVectorHelper.InsertFrom(constref source: array of integer; ind: integer);
var
  I, C, H, Ub: Integer;
  begin
    Ub := high(source);
    if Self = nil then
      DimVector(Self, ind + Ub);
    H := High(Self);
    if Ind > H then
    begin
      SetErrCode(MatErrDim);
      Exit;
    end;
    C := min(Ub, H-Ind) + 1;
    for I := H downto ind + C do
      self[I] := self[I-C]; // moving existing values freeing place for newly inserted
    for I := 0 to C - 1 do
      self[ind+I] := source[I];
end;


function TIntVectorHelper.ToString(Index: integer): string;
begin
  Result := IntStr(self[Index]);
end;

function TIntVectorHelper.ToStrings(Dest: TStrings; First, Last: integer;
  Indices: boolean; Delimiter: char): integer;
var
  I:integer;
  Buf:string;
begin
  if (not Assigned(Dest)) then
  begin
    Result := -1;
    Exit;
  end;
  for I := First to Last do
  begin
    if Indices then
      Buf := IntStr(I)+Delimiter+' '
    else
      Buf := '';
    Buf := Buf + ToString(I);
    Dest.Add(Buf);
  end;
  Result := 0;
end;

{ TVectorHelper }

procedure TVectorHelper.Insert(value: Float; index: integer);
var
  I: DWord;
begin
  if Index > High(self) then
  begin
    SetErrCode(MatErrDim);
    Exit;
  end;
  for I := High(self) downto index+1 do
    self[I] := self[I-1];
  self[Index] := value;
end;

procedure TVectorHelper.Remove(index: integer);
var
  I: Integer;
begin
  if Index > High(self) then
  begin
    SetErrCode(MatErrDim);
    Exit;
  end;
  for I := index to high(self)-1 do
    self[I] := self[I+1];
  self[high(self)] := 0;
end;

procedure TVectorHelper.Swap(ind1, ind2: integer);
var
  F:Float;
begin
  if (Ind1 > High(self)) or (Ind2 > High(Self)) then
  begin
    SetErrCode(MatErrDim);
    Exit;
  end;
  F := self[ind1];
  self[ind1] := self[ind2];
  self[ind2] := F;
end;

procedure TVectorHelper.Clear;
begin
  FillWord(Self[0],length(Self)*SizeOf(Float) div 2,0);
end;

procedure TVectorHelper.Fill(Lb, Ub: integer; Val: Float);
var
  I: Integer;
begin
  if Self = nil then
    DimVector(Self,Ub);
  for I := Lb to min(Ub, high(Self)) do
    self[I] := Val;
end;

procedure TVectorHelper.FillWithArr(Lb : integer; Vals: array of Float);
var
  L:integer;
begin
  if not Assigned(Self) then
    SetLength(Self,Lb + length(Vals));
  L := min(length(Self) - Lb,length(Vals));
  Move(Vals[0],Self[Lb],SizeOf(Float)*L);
end;

procedure TVectorHelper.Sort(Descending: boolean);
begin
  if length(self) > 0 then
    HeapSort(self, 0, high(self), Descending);
end;

procedure TVectorHelper.InsertFrom(Source:TVector; Lb, Ub: integer; ind:integer);
var
  I, C, H: Integer;
  begin
    if Self = nil then
      DimVector(Self, ind + Ub - Lb);
    H := High(Self);
    if (Lb > High(Source)) or (Ind > H) then
    begin
      SetErrCode(MatErrDim);
      Exit;
    end;
    Ub := min(Ub,High(Source));
    C := min(Ub - Lb + 1, H-Ind+1);
    for I := H downto ind + C do
      self[I] := self[I-C]; // moving existing values freeing place for newly inserted
    Move(Source[Lb],Self[Ind],C*SizeOf(Float));
  end;

procedure TVectorHelper.InsertFrom(constref source: array of float; ind: integer);
var
  I, C, H, Ub: Integer;
  begin
    Ub := high(source);
    if Self = nil then
      DimVector(Self, ind + Ub);
    H := High(Self);
    if Ind > H then
    begin
      SetErrCode(MatErrDim);
      Exit;
    end;
    C := min(Ub, H-Ind) + 1;
    for I := H downto ind + C do
      self[I] := self[I-C]; // moving existing values freeing place for newly inserted
    for I := 0 to C - 1 do
      self[ind+I] := source[I];
end;

function TVectorHelper.ToString(Index: integer): string;
begin
  Result := FloatStr(self[Index]);
end;

function TVectorHelper.ToStrings(Dest: TStrings; First, Last: integer; Indices: boolean; Delimiter: char): integer;
var
  I:integer;
  Buf:string;
begin
  if (not Assigned(Dest)) then
  begin
    Result := -1;
    Exit;
  end;
  for I := First to Last do
  begin
    if Indices then
      Buf := IntStr(I)+Delimiter+' '
    else
      Buf := '';
    Buf := Buf + ToString(I);
    Dest.Add(Buf);
  end;
  Result := 0;
end;

end.

