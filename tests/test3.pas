program test;

procedure q(var a : integer; b : integer);
begin
   writeln(a);
   writeln(b);
   a := a + 1;
   b := b + 1;
   writeln(a);
   writeln(b)
end;

procedure p(n : integer;  var f : integer);
begin
   f := n;
   writeln(n);
   writeln(f);
   q(f, n);
   writeln(n);
   writeln(f);
   q(n, f);
   writeln(n);
   writeln(f)
end;

var x : integer;
   
begin
   p(42, x);
   writeln(x)
end.
