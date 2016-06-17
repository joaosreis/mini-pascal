
program test;

procedure fact1(n : integer);
var f : integer;
begin
   f := 1;
   while n > 1 do begin
      f := n * f;
      n := n - 1
   end;
   writeln(f)
end;

procedure fact2(n : integer);
var f : integer;
   procedure mult();
   begin
      f := n * f;
      n := n - 1
   end;
begin
   f := 1;
   while n > 1 do mult();
   writeln(f)
end;

procedure fact3(n : integer);
var f : integer;
   procedure mult();
      procedure decr();
      begin n := n - 1 end;
   begin
      f := n * f;
      decr()
   end;
begin
   f := 1;
   while n > 1 do mult();
   writeln(f)
end;

procedure p();
begin
   fact1(5)
end;

begin
   fact1(5);
   p();
   fact2(5);
   fact3(5)
end.
