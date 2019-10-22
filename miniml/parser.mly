%{
open Syntax
%}

%token <string> VAR  // x, y, abc, ...
%token <int> INT     // 0, 1, 2, ...

%token PLUS     // '+'
%token MINUS    // '-'
%token ASTERISK // '*'
%token EQUAL    // '='
%token COLCOL   // "::"

%token LPAREN   // '('
%token RPAREN   // ')'
%token LBRA     // '['
%token RBRA     // ']'

%token ARROW    // "->"
%token VBAR     // '|'
%token SEMICOL  // ';'

%token TRUE     // "true"
%token FALSE    // "false"
%token FUN      // "fun"
%token LET      // "let"
%token REC      // "rec"
%token IN       // "in"
%token IF       // "if"
%token THEN     // "then"
%token ELSE     // "else"
%token MATCH    // "match"
%token WITH     // "with"
%token HEAD     // "List.hd"
%token TAIL     // "List.tl"

%token EOF

%nonassoc IN ELSE ARROW WITH
%left VBAR
%right COLCOL
%left PLUS MINUS
%left ASTERISK
%nonassoc UNARY
%left VAR INT TRUE FALSE LBRA LPAREN

%start main
%type <Syntax.exp> main

%%

main:
  | exp EOF
    { $1 }
;

list_inner:
  | exp { Cons($1, Empty) }
  | exp SEMICOL { Cons($1, Empty) }
  | exp SEMICOL list_inner { Cons($1, $3) }

arg_exp:
  | VAR
    { Var $1 }
  | INT
    { IntLit $1 }
  | TRUE
    { BoolLit true }
  | FALSE
    { BoolLit false }
  | LBRA RBRA { Empty }
  | LBRA list_inner RBRA { $2 }
  | LPAREN exp RPAREN
    { $2 }
;

exp:
  | arg_exp
    { $1 }
  | exp arg_exp
    { App ($1, $2) }
  | MINUS exp %prec UNARY
    { Binop (Minus, IntLit 0, $2) }
  | exp PLUS exp
    { Binop (Plus, $1, $3) }
  | exp MINUS exp
    { Binop (Minus, $1, $3) }
  | exp ASTERISK exp
    { Binop (Times, $1, $3) }
  | exp EQUAL exp
    { Binop (Eq, $1, $3) }
  | exp COLCOL exp
    { Cons ($1, $3) }
  | HEAD arg_exp
    { Head $2 }
  | TAIL arg_exp
    { Tail $2 }
  | FUN VAR ARROW exp
    { Fun ($2, $4) }
  | LET VAR EQUAL exp IN exp
    { Let ($2, $4, $6) }
  | LET REC VAR VAR EQUAL exp IN exp
    { LetRec ($3, $4, $6, $8) }
  | IF exp THEN exp ELSE exp
    { If ($2, $4, $6) }
  | MATCH exp WITH cases_rev
    { Match ($2, List.rev $4) }
  | error
    {
      let message =
        Printf.sprintf
          "parse error near characters %d-%d"
          (Parsing.symbol_start ())
	        (Parsing.symbol_end ())
	    in
	    failwith message
	  }
;

cases_rev:
  | pattern ARROW exp
    { [($1, $3)] }
  | cases_rev VBAR pattern ARROW exp
    { ($3, $5) :: $1 }
;

pattern:
  | VAR
    { Var $1 }
  | INT
    { IntLit $1 }
  | TRUE
    { BoolLit true }
  | FALSE
    { BoolLit false }
  | LBRA RBRA
    { Empty }
  | pattern COLCOL pattern
    { Cons ($1, $3) }
;
