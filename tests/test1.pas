program test;
var
   n,f : integer;
begin
   n := 10;
   f := 1;
   while n > 1 do begin f := n * f; n := n - 1 end;
   writeln(f)
end.
