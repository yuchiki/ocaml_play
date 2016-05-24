{
  open Parser
  exception Eof
}
rule token = parse
    [' ' '\t']    {token lexbuf}
    | ['\n']      {EOL}
    | ['0'-'9']+  {INT(int_of_string(Lexing.lexeme lexbuf))}
    | ['0'-'9']+'.'['0'-'9']+   {FLOAT(float_of_string(Lexing.lexeme lexbuf))}
    | '+'         {PLUS}
    | '*'         {MUL}
    | '('         {LPAREN}
    | ')'         {RPAREN}
    | '/'         {DIV}
    | '-'         {MINUS}
    | "true"      {TRUE}
    | "false"     {FALSE}
    | "let"       {LET}
    | "<="        {LE}
    | ">="        {GE}
    | ">"         {GT}
    | "<"         {LT}
    | "<>"        {NEQ}
    | "="         {EQUAL}
    | "if"        {IF}
    | "then"      {THEN}
    | "else"      {ELSE}
    | "not"       {NOT}
    | "||"        {OR}
    | "&&"        {AND}
    | ['A'-'Z' 'a'-'z' '_']['A'-'Z' 'a'-'z' '_' '0'-'9']* {ID(Lexing.lexeme lexbuf)}
    | eof         {raise Eof}
