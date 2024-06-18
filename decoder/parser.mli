type token =
  | LID of (
# 60 "parser.mly"
        string
# 6 "parser.mli"
)
  | CID of (
# 61 "parser.mly"
        string
# 11 "parser.mli"
)
  | OID of (
# 62 "parser.mly"
        string
# 16 "parser.mli"
)
  | LPAREN
  | RPAREN
  | LBRACK
  | RBRACK
  | LBRACE
  | RBRACE
  | TICK
  | UNDERSCORE
  | QMARK
  | COLON
  | ARROW
  | STAR
  | COMMA
  | MUTABLE
  | AS
  | OF
  | AND
  | EXCEPTION
  | MODULE
  | TYPE
  | VAL
  | LT
  | GT
  | EQ
  | ELLIPSIS
  | SEMICOLON
  | SHARP
  | DOT
  | OR
  | BACKTICK
  | EOF
  | SIG
  | END

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Specification.t list
