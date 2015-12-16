program main;
var return: integer;

function teste () integer;
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

	teste := x;
end;
begin 
	printInt(teste());	
end.
