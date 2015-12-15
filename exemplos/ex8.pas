program main;
var return: integer;

function pares (inicio : integer; fim : integer) integer;
begin
	while( inicio < fim ) do
	begin
		if (inicio mod 2 = 0) then 
		begin
			printInt(inicio);
			printString("\n");
		end;
		
		inicio := inicio + 1;
	end;

	teste := inicio;
end;
begin 
	printInt(pares(1, 10));	
	printString("\n");
end.
