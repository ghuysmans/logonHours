%{
  let next_hour (h, m) = h + 1, m
%}

%token ALLOW
%token DENY
%token <Day.t> DAY
%token COMMA
%token <int * int> TIME
%token MINUS
%token NL
%token EOF
%type <Ast.command> command
%start command
%type <Ast.script> script
%start script
%%

maybe_day:
| DAY { Some $1 }
| DAY COMMA { Some $1 }
| { None }
interval:
| maybe_day TIME { {Ast.day = $1; from = $2; until = next_hour $2} }
| maybe_day TIME MINUS TIME { {Ast.day = $1; from = $2; until = $4} }
command:
| interval { Ast.Allow $1 }
| ALLOW interval { Ast.Allow $2 }
| DENY interval { Ast.Deny $2 }
script:
| command NL script { $1 :: $3 }
| EOF { [] }
%%

let script f lexbuf = List.rev (script f lexbuf)
