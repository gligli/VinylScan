unit uIntervals;
{$mode objfpc}{$H+}{$MODESWITCH ADVANCEDRECORDS}
interface

uses uTypes, uMinMax;

type

TInterval = record
  Lo:Float;
  Hi:Float;
  function Length:float;
end;

//returns true if intervals [Lo1; Hi1] and [Lo2; Hi2] intersect
function IntervalsIntersect(Lo1, Hi1, Lo2, Hi2:Float):boolean; overload;

//returns true if intervals [Lo1; Hi1] and [Lo2; Hi2] intersect
function IntervalsIntersect(Lo1, Hi1, Lo2, Hi2:Integer):boolean; overload;

//returns true if intervals Interval1 and Interval2 intersect
function IntervalsIntersect(Interval1, Interval2:TInterval):boolean; overload;

// true if ContainedInterval is completely inside containing
// i.e. ContainedInterval.Lo > ContainingInterval.Lo and
// ContainedInterval.Hi < ContainingInterval.Hi
function Contained(ContainedInterval,ContainingInterval:TInterval):boolean;

// returns intersection of Interval1 and Interval2
// If no connection, result is (0;0)
function Intersection(Interval1, Interval2:TInterval):TInterval;

// true if V is inside AInterval
function Inside(V:Float; AInterval:TInterval):boolean; overload;

// true if V is inside (ALo, AHi) (similar to Math.InRange)
function Inside(V:float; ALo, AHi:float):boolean; overload;

// true if AInterval.Lo < AInterval.Hi
function IntervalDefined(AInterval:TInterval):boolean;

// constructor of TInterval from ALo and AHi
function DefineInterval(ALo,AHi:Float):TInterval;

// move interval by a value (it is added to both Lo and Hi)
procedure MoveInterval(V:float; var AInterval:TInterval);

// move interval to a value (Lo is set to this value, Hi adjusted such that length remains constant)
procedure MoveIntervalTo(V:Float; var AInterval:TInterval);

implementation

function DefineInterval(ALo, AHi:Float):TInterval;
begin
  Result.Lo := ALo;
  Result.Hi := AHi;
end;

procedure MoveInterval(V: float; var AInterval: TInterval);
begin
  AInterval.Hi := AInterval.Hi + V;
  AInterval.Lo := AInterval.Lo + V;
end;

procedure MoveIntervalTo(V: Float; var AInterval: TInterval);
var
  L:Float;
begin
  L := AInterval.Length;
  AInterval.Lo := V;
  AInterval.Hi := AInterval.Lo + L;
end;

function IntervalsIntersect(Lo1, Hi1, Lo2, Hi2: Float): boolean;
begin
  Result := ((Hi1 <= Lo2) and (Hi1 >= Lo2)) or ((Lo2 <= Hi1) and (Hi2 >= Lo1));
end;

function IntervalsIntersect(Lo1, Hi1, Lo2, Hi2: Integer): boolean;
begin
   Result := ((Lo1 <= Lo2) and (Hi1 >= Lo2)) or ((Lo2 <= Lo1) and (Hi2 >= Lo1));
end;

function IntervalsIntersect(Interval1, Interval2:TInterval):boolean; overload;
begin
  Result := ((Interval1.Lo <= Interval2.Lo) and (Interval1.Hi >= Interval2.Lo))
         or ((Interval2.Lo <= Interval1.Lo) and (Interval2.Hi >= Interval1.Lo));
end;

function TInterval.Length: float;
begin
  Result := Hi - Lo;
  if Result < 0 then Result := 0;
end;

function Inside(V: Float; AInterval:TInterval): boolean;
begin
  Result := (V >= AInterval.Lo) and (V <= AInterval.Hi);
end;

function Inside(V: float; ALo, AHi: float): boolean;
begin
  Result := (V >= ALo) and (V <= AHi);
end;

function IntervalDefined(AInterval:TInterval):boolean;
begin
  Result := not SameValue(AInterval.Hi, AInterval.Lo)
     and not (AInterval.Hi < AInterval.Lo);
end;

function Contained(ContainedInterval, ContainingInterval: TInterval): boolean;
begin
  Result := Intervaldefined(ContainedInterval) and Intervaldefined(ContainingInterval) and
    (ContainedInterval.Lo >= ContainingInterval.Lo) and (ContainedInterval.Hi <= ContainingInterval.Hi);
end;

function Intersection(Interval1, Interval2 :TInterval):TInterval;
begin
  if IntervalsIntersect(Interval1, Interval2) then
  begin
    Result.Lo := max(Interval1.Lo, Interval2.Lo);
    Result.Hi := min(Interval1.Hi, Interval2.Hi);
  end else
  begin
    Result.Lo := 0;
    Result.Hi := 0;
  end
end;


end.

