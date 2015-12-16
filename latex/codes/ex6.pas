program main;
procedure teste ();
var a, x, y: integer;
	b: real;
begin
	x := 1;

	while( x <> 3 ) do
	begin
		printInt(x);
		printString("\n");
		x := x + 1;
	end;
end;
begin 
	teste();
end.
