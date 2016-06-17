program fib;

var f : integer;

procedure fib(n	: integer);
   procedure somme();
   var tmp : integer;
   begin fib(n-2); tmp := f; fib(n-1); f := f + tmp end;
begin
   if n <= 1 then f := n else somme()
end;

begin fib(10); writeln(f) end.
