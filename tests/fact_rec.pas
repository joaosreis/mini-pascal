program fact;

var f : integer;

procedure fact(n : integer);
begin
   if n <= 1 then f := 1 else begin fact(n-1); f := n * f end
end;

begin fact(10); writeln(f) end.
