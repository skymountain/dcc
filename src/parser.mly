%{

  open Syntax

%}

%token FUN RARROW
%token LET REC EQ IN
%token<Syntax.id> IDENT
%token SEMISEMI
%token LPAREN RPAREN
%token<int> INTLIT
%token IF THEN ELSE TRUE FALSE
%token SHIFT RESET
%token LBRACKET RBRACKET CLNCLN
%token MATCH WITH VBAR
%token UNDERSCORE AS
%token BEGIN END
%token SEMICLN

%token PLUS ASTERISK MINUS LT

%nonassoc below_VBAR
%nonassoc VBAR

%start toplevel
%type <Syntax.program> toplevel

%%

toplevel :
| Program SEMISEMI { $1 }

Program :
| Expr { PExpr $1 }
| LetDecl { PLet $1 }

LetDecl :
| LET IDENT Bindlist EQ Expr { DLet ($2, $3, $5) }
| LET REC IDENT Bindlist EQ Expr { DLetRec ($3, $4, $6) }

Bindlist :
| { [] }
| IDENT Bindlist { $1::$2 }

Expr :
| FUN Bindlist RARROW Expr { EFun ($2, $4) }
| LetDecl IN Expr { ELet ($1, $3) }
| MATCH Expr WITH PatternMatchList { EMatch ($2, $4) }
| IfExpr { $1 }

IfExpr :
| IF Expr THEN Expr ELSE Expr { EIf ($2, $4, $6) }
| LtExpr { $1 }

LtExpr :
| ConsExpr LT ConsExpr { EBin (BLt, $1, $3) }
| ConsExpr { $1 }

ConsExpr :
| PlusExpr CLNCLN ConsExpr { ECons ($1, $3) }
| PlusExpr { $1 }

PlusExpr :
| PlusExpr PLUS MinusExpr { EBin (BPlus, $1, $3) }
| MinusExpr { $1 }

MinusExpr :
| MinusExpr MINUS MultExpr { EBin (BMinus, $1, $3) }
| MultExpr { $1 }

MultExpr :
| MultExpr ASTERISK AppExpr { EBin (BMult, $1, $3) }
| AppExpr { $1 }

AppExpr :
| AppExpr PExpr { EApp ($1, $2) }
| PExpr { $1 }

PExpr :
| IDENT { EVar $1 }
| Const { EConst $1 }
| LBRACKET ListLit RBRACKET { $2 }
| SHIFT { EShift }
| RESET { EReset }
| LPAREN Expr RPAREN { $2 }
| BEGIN Expr END { $2 } 

Const :
| INTLIT { CInt $1 }
| TRUE { CBool true }
| FALSE { CBool false }
| LPAREN RPAREN { CUnit }

ListLit :
| { EConst CNil }
| IfExpr { ECons ($1, EConst CNil) }
| IfExpr SEMICLN ListLit { ECons ($1, $3) }

PatternMatchList :
| PatternMatch %prec below_VBAR { [$1] }
| PatternMatch VBAR PatternMatchList { $1::$3 }

PatternMatch :
| Pattern RARROW Expr { ($1,$3) }

Pattern :
| Pattern AS IDENT { PatAs ($1, $3) }
| OrPattern { $1 }

OrPattern :
| OrPattern VBAR ConsPattern { PatOr ($1, $3) }
| ConsPattern { $1 }

ConsPattern :
| SimplePattern CLNCLN ConsPattern { PatCons ($1, $3) }
| SimplePattern { $1 }

SimplePattern :
| Const { PatConst $1 }
| UNDERSCORE { PatWildcard }
| IDENT { PatIdent $1 }
| LBRACKET PatternList RBRACKET { $2 }
| LPAREN Pattern RPAREN { $2 }

PatternList :
| { PatConst CNil }
| Pattern { PatCons ($1, PatConst CNil) }
| Pattern SEMICLN PatternList { PatCons ($1, $3) }
