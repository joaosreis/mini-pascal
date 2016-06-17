program syracuse;

procedure syracuse(max : integer);
var i : integer;
   procedure length();
   var v,j : integer;
      procedure step();
      begin v := v+1; if j = 2*(j/2) then j := j/2 else j := 3*j+1 end;
   begin v := 0; j := i; while j <> 1 do step(); writeln(v) end;
begin
   i := 1;
   while i <= max do begin length(); i := i+1 end
end;

begin syracuse(6) end.
