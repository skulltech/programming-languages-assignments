{
  open Parser   (* The tokens are defined in parser.mly *)
  exception Eof
}

let whitespace = [' ' '\t' '\n']+
let eol        = ['.']
let var        = ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_' ''']*
let const      = ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_' ''']*

rule token = parse
  | whitespace { token lexbuf }
  | ['.']  { EOL   }
  | [',']  { COMMA }
  | ['(']  { LP    }
  | [')']  { RP    }
  | [':-'] { SEP   }
  | var as ident   { VAR   (ident) }
  | const as ident { CONST (ident) }
  | eof {EOF}
