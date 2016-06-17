program test;

procedure comb(n : integer; k: integer; var v: integer);
var tmp : integer;
begin
   if k = 0 or k = n then v := 1
   else begin
      comb(n-1, k-1, tmp);
      comb(n-1, k,   v  );
      v := v + tmp
   end
end;

procedure row(n : integer);
var line, k, v :  integer;
begin
   line := 0;
   k := 0;
   while k <= n do begin comb(n, k, v); line := 10 * line + v; k := k + 1 end;
   writeln(line)
end; { row }

var r: integer;

begin
   r := 0;
   while r <= 4 do begin row(r); r := r + 1 end
end.
