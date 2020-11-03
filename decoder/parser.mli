type token =
  | LID of (string)
  | CID of (string)
  | OID of (string)
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
