{

  let keyword_tbl =
    let assoc =
      [
        ("fun", Parser.FUN);
        ("if", Parser.IF);
        ("then", Parser.THEN);
        ("else", Parser.ELSE);
        ("true", Parser.TRUE);
        ("false", Parser.FALSE);
        ("let", Parser.LET);
        ("rec", Parser.REC);
        ("in", Parser.IN);
        ("shift", Parser.SHIFT);
        ("reset", Parser.RESET);
        ("begin", Parser.BEGIN);
        ("end", Parser.END);
        ("match", Parser.MATCH);
        ("with", Parser.WITH);
        ("as", Parser.AS);
      ]
    in
    let tbl = BatHashtbl.create 20 in
    List.iter (fun (key, token) -> BatHashtbl.add tbl key token) assoc;
    tbl

}

rule main = parse
| [' ' '\009' '\012' '\n']+
  { main lexbuf }

| "-"? ['0'-'9']+
  { Parser.INTLIT (int_of_string (Lexing.lexeme lexbuf)) }

| "(" { Parser.LPAREN }
| ")" { Parser.RPAREN }

| "->" { Parser.RARROW }
| ";;" { Parser.SEMISEMI }
| "="  { Parser.EQ }
| ";"  { Parser.SEMICLN }

| "[" { Parser.LBRACKET }
| "]" { Parser.RBRACKET }
| "::" { Parser.CLNCLN }
| "_" { Parser.UNDERSCORE }
| "|" { Parser.VBAR }

| "+" { Parser.PLUS }
| "*" { Parser.ASTERISK }
| "-" { Parser.MINUS }
| "<" { Parser.LT }

| ['a'-'z'] ['a'-'z' '0'-'9' '_' '\'' ]*
  {
    let id = Lexing.lexeme lexbuf in
    BatHashtbl.find_default keyword_tbl id (Parser.IDENT id)
  }

| eof { exit 0 }
