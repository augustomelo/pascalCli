program main;
var return: integer;

function fat (numero : integer) integer;
begin
	if ( numero = 0 ) then
	begin
		fat := 1;
	end
	else
	begin
		fat := numero * fat( numero - 1 );
	end;
end;
begin 
	printString(" O fatorial eh: ");
	printInt(fat(5));	
	printString("\n");
end.
