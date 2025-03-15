program testunitsformat;
uses uUnitsformat, uTypes;
var
  Val:float;
begin
  writeln('Enter any number and it will be represented with decimal prefix.');
  writeln('Appropriate one from u (utto) to P (peta) will be used.');
  writeln('Type -1 to leave.');

  repeat
    readln(Val);
    writeln(FormatUnits(Val,'S'));
  until Val = -1;
end.

