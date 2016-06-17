
program test;

procedure fact1(n : integer; var f : integer);
begin
   f := 1;
   while n > 1 do begin
      f := n * f;
      n := n - 1
   end
end;

procedure fact2(n : integer; var f : integer);
   procedure mult();
   begin
      f := n * f;
      n := n - 1
   end;
begin
   f := 1;
   while n > 1 do mult()
end;

procedure fact3(n : integer; var f : integer);
   procedure mult();
      procedure decr();
      begin n := n - 1 end;
   begin
      f := n * f;
      decr()
   end;
begin
   f := 1;
   while n > 1 do mult()
end;

var x : integer;
   
begin
   fact1(5, x);
   writeln(x);
   fact2(5, x);
   writeln(x);
   fact3(5, x);
   writeln(x)
end.
