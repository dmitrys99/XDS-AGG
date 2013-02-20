program testd;
{$Apptype Console}
uses 
  math;
var
  x: extended;
begin
  x := arctan2(0.76, 0);
  Writeln(x);
end.