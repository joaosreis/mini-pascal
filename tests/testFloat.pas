program testFloat;

var x, y, z : real;
var i : integer;

begin
  x := 23423423.232234;
  y := 2423423.112;
  z := 234234.1;
  writeln(x + y * z - 23423432.5 / 5545.4);
  i := 5;
  writeln(-i);
  writeln(2 ^ 3 * i);
  if (i % 2 <> 0) then writeln(2.0 ^ 3.1) else writeln(3.14)
end.
