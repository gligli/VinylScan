unit usearchtrees;
{$mode objfpc}{$H+}
interface
uses Objects, SysUtils;

type

  PStringTreeNode = ^TStringTreeNode;
  TStringTreeNode = object
    Name : string;
    Left : PStringTreeNode;
    Right: PStringTreeNode;
    constructor Init(AName:string);
    destructor Done;
    function Find(AName:string; out Comparison:integer):PStringTreeNode;
  end;

implementation

constructor TStringTreeNode.Init(AName:string);
begin
  Name := AName;
  Left := nil; Right := nil;
end;

destructor TStringTreeNode.Done;
begin
  if Right <> nil then
    dispose(Right,Done);
  if Left <> nil then
    dispose(Left,Done);
end;

function TStringTreeNode.Find(AName: string; out Comparison:integer): PStringTreeNode;
begin
  Comparison := AnsiCompareStr(AName, Name);
  if (Comparison < 0) and Assigned(Left) then
    Result := Left^.Find(AName, Comparison)
  else if (Comparison > 0) and Assigned(Right) then
    Result := Right^.Find(AName, Comparison)
  else
    Result := @Self;
end;

end.
