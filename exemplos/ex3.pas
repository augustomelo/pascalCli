program main;
var a, x, y: integer;
	b: real;
begin 
	x := -10;
    if (x > 0) then
    begin 
     	x := 1;
    end;
	if (x < 0) then 
	begin
        x := -1;
    end
    else
    begin 
		x := 0;
    end;
    
    printInt(x);
end.
