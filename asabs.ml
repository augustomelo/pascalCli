type cmd = Return of expr
and expr = Int of int 
and decfn = { fn_nome: string;
	      (*fn_formais: (string) list;*)
	      fn_locais: (string) list;
	      fn_corpo: cmd list
	    }
