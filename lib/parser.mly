%token <Ast.command> COMMAND
%token NL
%token EOF
%type <Ast.script> script
%start script
%%

script:
| COMMAND NL script { $1 :: $3 }
| EOF { [] }
%%

let script f lexbuf = List.rev (script f lexbuf)
