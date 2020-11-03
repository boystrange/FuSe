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

open Parsing;;
let _ = parse_error;;
# 19 "parser.mly"
				      let of_tuple_type =
					function
					  [t] -> t
					| ts -> Ast.Constructor (`Tuple, ts)
					
  let expand t0 =
    let defs = Hashtbl.create 16 in
    let rec collect =
      function
      | Ast.Var _ as t -> t
      | Ast.Tagged (ctor, tags) ->
	 Ast.Tagged (ctor, List.map collect_tag tags)
      | Ast.Constructor (`As x, [t]) ->
	 Hashtbl.add defs x (collect t);
	 Ast.Var x
      | Ast.Constructor (`As _, _) -> assert false (* impossible *)
      | Ast.Constructor (ctor, ts) ->
	 Ast.Constructor (ctor, List.map collect ts)
      | _ -> assert false (* impossible *)
    and collect_tag (name, t) = name, collect t
    in
    let rec aux evars =
      function
      | Ast.Var x when List.mem x evars -> Ast.RecVar x
      | Ast.Var x when Hashtbl.mem defs x ->
	 Ast.Rec (x, aux (x :: evars) (Hashtbl.find defs x))
      | Ast.Var _ as t -> t
      | Ast.Tagged (ctor, tags) ->
	 Ast.Tagged (ctor, List.map (aux_tag evars) tags)
      | Ast.Constructor (ctor, ts) ->
	 Ast.Constructor (ctor, List.map (aux evars) ts)
      | _ -> assert false
    and aux_tag evars (name, t) = (name, aux evars t)
    in aux [] (collect t0)

  let cons_opt =
    function
    | None -> fun x -> x
    | Some x -> fun y -> x :: y
# 82 "parser.ml"
let yytransl_const = [|
  260 (* LPAREN *);
  261 (* RPAREN *);
  262 (* LBRACK *);
  263 (* RBRACK *);
  264 (* LBRACE *);
  265 (* RBRACE *);
  266 (* TICK *);
  267 (* UNDERSCORE *);
  268 (* QMARK *);
  269 (* COLON *);
  270 (* ARROW *);
  271 (* STAR *);
  272 (* COMMA *);
  273 (* MUTABLE *);
  274 (* AS *);
  275 (* OF *);
  276 (* AND *);
  277 (* EXCEPTION *);
  278 (* MODULE *);
  279 (* TYPE *);
  280 (* VAL *);
  281 (* LT *);
  282 (* GT *);
  283 (* EQ *);
  284 (* ELLIPSIS *);
  285 (* SEMICOLON *);
  286 (* SHARP *);
  287 (* DOT *);
  288 (* OR *);
  289 (* BACKTICK *);
    0 (* EOF *);
  290 (* SIG *);
  291 (* END *);
    0|]

let yytransl_block = [|
  257 (* LID *);
  258 (* CID *);
  259 (* OID *);
    0|]

let yylhs = "\255\255\
\001\000\004\000\004\000\003\000\003\000\005\000\005\000\005\000\
\005\000\005\000\005\000\009\000\009\000\010\000\011\000\011\000\
\011\000\011\000\008\000\008\000\007\000\007\000\014\000\014\000\
\012\000\012\000\013\000\015\000\015\000\016\000\017\000\017\000\
\018\000\006\000\019\000\019\000\020\000\020\000\021\000\021\000\
\022\000\022\000\022\000\022\000\022\000\022\000\022\000\024\000\
\026\000\026\000\023\000\023\000\025\000\025\000\025\000\025\000\
\027\000\027\000\002\000\002\000\029\000\028\000\028\000\000\000"

let yylen = "\002\000\
\001\000\001\000\003\000\000\000\002\000\001\000\004\000\002\000\
\004\000\006\000\002\000\001\000\003\000\002\000\000\000\002\000\
\002\000\002\000\001\000\003\000\002\000\003\000\000\000\002\000\
\001\000\003\000\003\000\000\000\003\000\004\000\000\000\001\000\
\002\000\001\000\001\000\003\000\001\000\003\000\001\000\003\000\
\001\000\004\000\003\000\001\000\002\000\004\000\001\000\002\000\
\000\000\002\000\001\000\003\000\004\000\004\000\006\000\003\000\
\000\000\001\000\001\000\003\000\004\000\002\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\051\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\064\000\001\000\000\000\006\000\
\041\000\000\000\035\000\000\000\000\000\044\000\047\000\000\000\
\000\000\000\000\000\000\000\000\000\000\058\000\000\000\000\000\
\033\000\000\000\000\000\008\000\000\000\000\000\011\000\000\000\
\002\000\000\000\000\000\005\000\000\000\000\000\000\000\045\000\
\052\000\043\000\000\000\048\000\000\000\000\000\000\000\000\000\
\056\000\000\000\000\000\000\000\000\000\021\000\000\000\000\000\
\000\000\014\000\000\000\000\000\000\000\036\000\038\000\040\000\
\000\000\050\000\046\000\054\000\000\000\053\000\000\000\060\000\
\000\000\022\000\024\000\000\000\000\000\009\000\000\000\000\000\
\016\000\000\000\017\000\018\000\013\000\003\000\007\000\000\000\
\000\000\061\000\000\000\000\000\032\000\000\000\000\000\000\000\
\000\000\000\000\055\000\010\000\020\000\027\000\000\000\000\000\
\026\000\063\000\029\000\000\000\030\000"

let yydgoto = "\002\000\
\013\000\030\000\014\000\043\000\015\000\016\000\090\000\086\000\
\039\000\040\000\066\000\091\000\092\000\062\000\102\000\103\000\
\104\000\017\000\018\000\019\000\020\000\021\000\022\000\026\000\
\023\000\052\000\031\000\097\000\032\000"

let yysindex = "\003\000\
\013\255\000\000\000\000\236\254\069\255\035\255\045\255\049\255\
\014\255\050\255\069\255\055\255\000\000\000\000\013\255\000\000\
\000\000\036\255\000\000\058\255\025\255\000\000\000\000\040\255\
\015\255\075\255\030\255\030\255\072\255\000\000\076\255\052\255\
\000\000\073\255\019\255\000\000\016\255\060\255\000\000\062\255\
\000\000\082\255\077\255\000\000\078\255\069\255\069\255\000\000\
\000\000\000\000\069\255\000\000\040\255\252\254\084\255\070\255\
\000\000\030\255\069\255\069\255\069\255\000\000\059\255\090\255\
\043\255\000\000\069\255\089\255\069\255\000\000\000\000\000\000\
\079\255\000\000\000\000\000\000\063\255\000\000\069\255\000\000\
\040\255\000\000\000\000\013\255\067\255\000\000\255\254\083\255\
\000\000\071\255\000\000\000\000\000\000\000\000\000\000\097\255\
\094\255\000\000\074\255\090\255\000\000\093\255\081\255\103\255\
\014\255\063\255\000\000\000\000\000\000\000\000\083\255\092\255\
\000\000\000\000\000\000\069\255\000\000"

let yyrindex = "\000\000\
\106\000\000\000\000\000\000\000\000\000\076\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\006\000\000\000\
\000\000\065\000\000\000\001\000\097\000\000\000\000\000\000\000\
\000\000\000\000\254\254\076\255\000\000\000\000\000\000\002\255\
\000\000\000\000\129\000\000\000\000\000\136\000\000\000\171\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\102\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\033\000\000\000\000\000\080\255\199\000\000\000\129\000\012\255\
\000\000\164\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\101\255\000\000\000\000\000\000\000\000\012\255\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\053\000\241\255\000\000\000\000\253\255\103\000\014\000\
\049\000\000\000\000\000\012\000\000\000\000\000\007\000\000\000\
\000\000\074\000\000\000\075\000\073\000\063\000\242\255\072\000\
\000\000\000\000\050\000\018\000\000\000"

let yytablesize = 490
let yytable = "\044\000\
\037\000\025\000\076\000\001\000\057\000\004\000\048\000\038\000\
\059\000\049\000\024\000\060\000\031\000\003\000\004\000\035\000\
\005\000\061\000\006\000\050\000\028\000\077\000\007\000\057\000\
\008\000\003\000\004\000\059\000\063\000\024\000\051\000\060\000\
\042\000\009\000\010\000\011\000\012\000\061\000\075\000\047\000\
\003\000\004\000\064\000\003\000\087\000\033\000\005\000\073\000\
\006\000\034\000\088\000\037\000\007\000\045\000\008\000\041\000\
\082\000\083\000\042\000\027\000\028\000\089\000\029\000\038\000\
\034\000\095\000\048\000\029\000\099\000\003\000\004\000\046\000\
\005\000\056\000\006\000\098\000\054\000\055\000\007\000\053\000\
\008\000\067\000\057\000\058\000\068\000\059\000\065\000\007\000\
\079\000\069\000\078\000\085\000\084\000\094\000\051\000\096\000\
\039\000\100\000\106\000\101\000\107\000\110\000\105\000\112\000\
\116\000\004\000\049\000\062\000\108\000\111\000\080\000\036\000\
\117\000\109\000\004\000\093\000\113\000\115\000\070\000\072\000\
\071\000\081\000\074\000\114\000\000\000\000\000\000\000\000\000\
\023\000\000\000\000\000\000\000\000\000\000\000\000\000\015\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\025\000\000\000\000\000\000\000\000\000\
\000\000\000\000\012\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\019\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\037\000\037\000\000\000\037\000\037\000\037\000\037\000\
\000\000\000\000\037\000\000\000\037\000\000\000\000\000\000\000\
\037\000\000\000\037\000\000\000\037\000\037\000\037\000\037\000\
\037\000\000\000\037\000\037\000\000\000\037\000\000\000\000\000\
\037\000\000\000\000\000\037\000\042\000\042\000\042\000\042\000\
\004\000\000\000\042\000\000\000\042\000\000\000\042\000\042\000\
\042\000\000\000\042\000\000\000\042\000\042\000\042\000\042\000\
\042\000\000\000\042\000\042\000\000\000\042\000\000\000\000\000\
\042\000\034\000\034\000\042\000\034\000\034\000\034\000\034\000\
\000\000\000\000\034\000\000\000\034\000\000\000\000\000\000\000\
\034\000\000\000\000\000\000\000\034\000\034\000\034\000\034\000\
\034\000\000\000\034\000\034\000\000\000\034\000\000\000\000\000\
\034\000\000\000\000\000\034\000\039\000\039\000\039\000\039\000\
\000\000\000\000\039\000\000\000\039\000\000\000\039\000\000\000\
\039\000\000\000\039\000\000\000\039\000\039\000\039\000\039\000\
\039\000\000\000\039\000\039\000\000\000\039\000\000\000\000\000\
\039\000\023\000\023\000\039\000\023\000\000\000\023\000\000\000\
\015\000\015\000\023\000\015\000\023\000\015\000\000\000\000\000\
\000\000\015\000\000\000\015\000\023\000\023\000\023\000\023\000\
\023\000\000\000\000\000\015\000\015\000\015\000\015\000\015\000\
\023\000\000\000\000\000\023\000\025\000\025\000\000\000\025\000\
\000\000\025\000\015\000\012\000\012\000\025\000\012\000\025\000\
\012\000\000\000\000\000\000\000\012\000\000\000\012\000\025\000\
\025\000\025\000\025\000\025\000\000\000\000\000\000\000\012\000\
\012\000\012\000\012\000\000\000\000\000\000\000\025\000\019\000\
\019\000\000\000\019\000\000\000\019\000\012\000\000\000\000\000\
\019\000\000\000\019\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\019\000\019\000\019\000\019\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\019\000"

let yycheck = "\015\000\
\000\000\005\000\007\001\001\000\007\001\000\000\021\000\011\000\
\007\001\024\000\031\001\013\001\001\001\001\001\002\001\002\001\
\004\001\019\001\006\001\005\001\009\001\026\001\010\001\026\001\
\012\001\001\001\002\001\026\001\013\001\031\001\016\001\013\001\
\000\000\021\001\022\001\023\001\024\001\019\001\053\000\015\001\
\001\001\002\001\027\001\001\001\002\001\001\001\004\001\051\000\
\006\001\001\001\008\001\002\001\010\001\018\001\012\001\001\001\
\060\000\061\000\004\001\025\001\026\001\065\000\033\001\067\000\
\000\000\069\000\081\000\033\001\084\000\001\001\002\001\014\001\
\004\001\002\001\006\001\079\000\027\000\028\000\010\001\005\001\
\012\001\020\001\007\001\032\001\003\001\013\001\027\001\010\001\
\019\001\013\001\007\001\002\001\034\001\005\001\016\001\033\001\
\000\000\031\001\002\001\017\001\007\001\009\001\032\001\001\001\
\013\001\000\000\005\001\007\001\035\001\029\001\058\000\009\000\
\116\000\100\000\035\001\067\000\105\000\111\000\045\000\047\000\
\046\000\059\000\051\000\106\000\255\255\255\255\255\255\255\255\
\000\000\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
\255\255\255\255\000\000\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\000\000\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\002\001\255\255\004\001\005\001\006\001\007\001\
\255\255\255\255\010\001\255\255\012\001\255\255\255\255\255\255\
\016\001\255\255\018\001\255\255\020\001\021\001\022\001\023\001\
\024\001\255\255\026\001\027\001\255\255\029\001\255\255\255\255\
\032\001\255\255\255\255\035\001\004\001\005\001\006\001\007\001\
\035\001\255\255\010\001\255\255\012\001\255\255\014\001\015\001\
\016\001\255\255\018\001\255\255\020\001\021\001\022\001\023\001\
\024\001\255\255\026\001\027\001\255\255\029\001\255\255\255\255\
\032\001\001\001\002\001\035\001\004\001\005\001\006\001\007\001\
\255\255\255\255\010\001\255\255\012\001\255\255\255\255\255\255\
\016\001\255\255\255\255\255\255\020\001\021\001\022\001\023\001\
\024\001\255\255\026\001\027\001\255\255\029\001\255\255\255\255\
\032\001\255\255\255\255\035\001\004\001\005\001\006\001\007\001\
\255\255\255\255\010\001\255\255\012\001\255\255\014\001\255\255\
\016\001\255\255\018\001\255\255\020\001\021\001\022\001\023\001\
\024\001\255\255\026\001\027\001\255\255\029\001\255\255\255\255\
\032\001\001\001\002\001\035\001\004\001\255\255\006\001\255\255\
\001\001\002\001\010\001\004\001\012\001\006\001\255\255\255\255\
\255\255\010\001\255\255\012\001\020\001\021\001\022\001\023\001\
\024\001\255\255\255\255\020\001\021\001\022\001\023\001\024\001\
\032\001\255\255\255\255\035\001\001\001\002\001\255\255\004\001\
\255\255\006\001\035\001\001\001\002\001\010\001\004\001\012\001\
\006\001\255\255\255\255\255\255\010\001\255\255\012\001\020\001\
\021\001\022\001\023\001\024\001\255\255\255\255\255\255\021\001\
\022\001\023\001\024\001\255\255\255\255\255\255\035\001\001\001\
\002\001\255\255\004\001\255\255\006\001\035\001\255\255\255\255\
\010\001\255\255\012\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\021\001\022\001\023\001\024\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\035\001"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  LBRACK\000\
  RBRACK\000\
  LBRACE\000\
  RBRACE\000\
  TICK\000\
  UNDERSCORE\000\
  QMARK\000\
  COLON\000\
  ARROW\000\
  STAR\000\
  COMMA\000\
  MUTABLE\000\
  AS\000\
  OF\000\
  AND\000\
  EXCEPTION\000\
  MODULE\000\
  TYPE\000\
  VAL\000\
  LT\000\
  GT\000\
  EQ\000\
  ELLIPSIS\000\
  SEMICOLON\000\
  SHARP\000\
  DOT\000\
  OR\000\
  BACKTICK\000\
  EOF\000\
  SIG\000\
  END\000\
  "

let yynames_block = "\
  LID\000\
  CID\000\
  OID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'specification_list) in
    Obj.repr(
# 107 "parser.mly"
                     ( _1 )
# 386 "parser.ml"
               : Specification.t list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 111 "parser.mly"
      ( _1 )
# 393 "parser.ml"
               : 'value_name))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 112 "parser.mly"
                    ( _2 )
# 400 "parser.ml"
               : 'value_name))
; (fun __caml_parser_env ->
    Obj.repr(
# 116 "parser.mly"
  ( [] )
# 406 "parser.ml"
               : 'specification_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'specification) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'specification_list) in
    Obj.repr(
# 117 "parser.mly"
                                   ( cons_opt _1 _2 )
# 414 "parser.ml"
               : 'specification_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'type_expr) in
    Obj.repr(
# 121 "parser.mly"
            ( Some (Specification.Type (expand _1)) )
# 421 "parser.ml"
               : 'specification))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'value_name) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'type_expr) in
    Obj.repr(
# 122 "parser.mly"
                                 ( Some (Specification.Val (_2, expand _4)) )
# 429 "parser.ml"
               : 'specification))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'constr_decl) in
    Obj.repr(
# 123 "parser.mly"
                        ( None )
# 436 "parser.ml"
               : 'specification))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'module_path) in
    Obj.repr(
# 124 "parser.mly"
                            ( None )
# 444 "parser.ml"
               : 'specification))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'specification_list) in
    Obj.repr(
# 125 "parser.mly"
                                              ( Some (Specification.Module (_2, _5)) )
# 452 "parser.ml"
               : 'specification))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'type_definition_ne_list) in
    Obj.repr(
# 126 "parser.mly"
                               ( None )
# 459 "parser.ml"
               : 'specification))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'type_definition) in
    Obj.repr(
# 130 "parser.mly"
                  ( )
# 466 "parser.ml"
               : 'type_definition_ne_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'type_definition) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'type_definition_ne_list) in
    Obj.repr(
# 131 "parser.mly"
                                              ( )
# 474 "parser.ml"
               : 'type_definition_ne_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'type_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'type_information) in
    Obj.repr(
# 135 "parser.mly"
                             ( )
# 482 "parser.ml"
               : 'type_definition))
; (fun __caml_parser_env ->
    Obj.repr(
# 139 "parser.mly"
  ( )
# 488 "parser.ml"
               : 'type_information))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'type_expr) in
    Obj.repr(
# 140 "parser.mly"
               ( )
# 495 "parser.ml"
               : 'type_information))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'constr_decl_ne_list) in
    Obj.repr(
# 141 "parser.mly"
                         ( )
# 502 "parser.ml"
               : 'type_information))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'record_decl) in
    Obj.repr(
# 142 "parser.mly"
                 ( )
# 509 "parser.ml"
               : 'type_information))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 146 "parser.mly"
      ( )
# 516 "parser.ml"
               : 'module_path))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'module_path) in
    Obj.repr(
# 147 "parser.mly"
                      ( )
# 524 "parser.ml"
               : 'module_path))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'constr_args_opt) in
    Obj.repr(
# 151 "parser.mly"
                      ( )
# 532 "parser.ml"
               : 'constr_decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'type_expr) in
    Obj.repr(
# 152 "parser.mly"
                      ( )
# 540 "parser.ml"
               : 'constr_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 156 "parser.mly"
  ( )
# 546 "parser.ml"
               : 'constr_args_opt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'type_expr) in
    Obj.repr(
# 157 "parser.mly"
               ( )
# 553 "parser.ml"
               : 'constr_args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'constr_decl) in
    Obj.repr(
# 161 "parser.mly"
              ( )
# 560 "parser.ml"
               : 'constr_decl_ne_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'constr_decl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'constr_decl_ne_list) in
    Obj.repr(
# 162 "parser.mly"
                                     ( )
# 568 "parser.ml"
               : 'constr_decl_ne_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'field_decl_list) in
    Obj.repr(
# 166 "parser.mly"
                                ( )
# 575 "parser.ml"
               : 'record_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 170 "parser.mly"
  ( )
# 581 "parser.ml"
               : 'field_decl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'field_decl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'field_decl_list) in
    Obj.repr(
# 171 "parser.mly"
                                       ( )
# 589 "parser.ml"
               : 'field_decl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'mutable_opt) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'type_expr) in
    Obj.repr(
# 175 "parser.mly"
                                  ( )
# 598 "parser.ml"
               : 'field_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 179 "parser.mly"
  ( )
# 604 "parser.ml"
               : 'mutable_opt))
; (fun __caml_parser_env ->
    Obj.repr(
# 180 "parser.mly"
          ( )
# 610 "parser.ml"
               : 'mutable_opt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 184 "parser.mly"
           ( _2 )
# 617 "parser.ml"
               : 'type_var))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'as_type_expr) in
    Obj.repr(
# 188 "parser.mly"
               ( _1 )
# 624 "parser.ml"
               : 'type_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arrow_type_expr) in
    Obj.repr(
# 192 "parser.mly"
                  ( _1 )
# 631 "parser.ml"
               : 'as_type_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'as_type_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'type_var) in
    Obj.repr(
# 193 "parser.mly"
                           ( Ast.Constructor (`As _3, [_1]) )
# 639 "parser.ml"
               : 'as_type_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tuple_type_expr) in
    Obj.repr(
# 197 "parser.mly"
                  ( of_tuple_type _1 )
# 646 "parser.ml"
               : 'arrow_type_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'tuple_type_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arrow_type_expr) in
    Obj.repr(
# 199 "parser.mly"
  ( Ast.Constructor (`Arrow, [of_tuple_type _1; _3]) )
# 654 "parser.ml"
               : 'arrow_type_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_type_expr) in
    Obj.repr(
# 203 "parser.mly"
                   ( [_1] )
# 661 "parser.ml"
               : 'tuple_type_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomic_type_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tuple_type_expr) in
    Obj.repr(
# 204 "parser.mly"
                                        ( _1 :: _3 )
# 669 "parser.ml"
               : 'tuple_type_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'type_var) in
    Obj.repr(
# 208 "parser.mly"
           ( Ast.Var _1 )
# 676 "parser.ml"
               : 'atomic_type_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_type_expr) in
    Obj.repr(
# 209 "parser.mly"
                                   ( _4 )
# 684 "parser.ml"
               : 'atomic_type_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'type_expr) in
    Obj.repr(
# 210 "parser.mly"
                          ( _2 )
# 691 "parser.ml"
               : 'atomic_type_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'type_constr) in
    Obj.repr(
# 211 "parser.mly"
              ( Ast.Constructor (`Apply _1, []) )
# 698 "parser.ml"
               : 'atomic_type_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'atomic_type_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'type_constr) in
    Obj.repr(
# 212 "parser.mly"
                               ( Ast.Constructor (`Apply _2, [_1]) )
# 706 "parser.ml"
               : 'atomic_type_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'type_expr_comma_ne_list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'type_constr) in
    Obj.repr(
# 213 "parser.mly"
                                                    ( Ast.Constructor (`Apply _4, _2) )
# 714 "parser.ml"
               : 'atomic_type_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'polymorphic_variant_type) in
    Obj.repr(
# 214 "parser.mly"
                           ( Ast.Tagged (`Variant, _1) )
# 721 "parser.ml"
               : 'atomic_type_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'type_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'comma_type_expr_list) in
    Obj.repr(
# 218 "parser.mly"
                                 ( _1 :: _2 )
# 729 "parser.ml"
               : 'type_expr_comma_ne_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 222 "parser.mly"
  ( [] )
# 735 "parser.ml"
               : 'comma_type_expr_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'type_expr_comma_ne_list) in
    Obj.repr(
# 223 "parser.mly"
                                ( _2 )
# 742 "parser.ml"
               : 'comma_type_expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 227 "parser.mly"
      ( [_1] )
# 749 "parser.ml"
               : 'type_constr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'type_constr) in
    Obj.repr(
# 228 "parser.mly"
                      ( _1 :: _3 )
# 757 "parser.ml"
               : 'type_constr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'tag_spec_list) in
    Obj.repr(
# 232 "parser.mly"
                                 ( _3 )
# 764 "parser.ml"
               : 'polymorphic_variant_type))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'tag_spec_list) in
    Obj.repr(
# 233 "parser.mly"
                                 ( _3 )
# 771 "parser.ml"
               : 'polymorphic_variant_type))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'tag_spec_list) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'tag_ne_list) in
    Obj.repr(
# 234 "parser.mly"
                                                ( _3 )
# 779 "parser.ml"
               : 'polymorphic_variant_type))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'tag_spec_list) in
    Obj.repr(
# 235 "parser.mly"
                              ( _2 )
# 786 "parser.ml"
               : 'polymorphic_variant_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 239 "parser.mly"
  ( [] )
# 792 "parser.ml"
               : 'tag_spec_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : (string * Ast.t) list) in
    Obj.repr(
# 240 "parser.mly"
                   ( _1 )
# 799 "parser.ml"
               : 'tag_spec_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tag_spec) in
    Obj.repr(
# 244 "parser.mly"
           ( [_1] )
# 806 "parser.ml"
               : (string * Ast.t) list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'tag_spec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : (string * Ast.t) list) in
    Obj.repr(
# 245 "parser.mly"
                               ( _1 :: _3 )
# 814 "parser.ml"
               : (string * Ast.t) list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'type_expr) in
    Obj.repr(
# 249 "parser.mly"
                            ( (_2, _4) )
# 822 "parser.ml"
               : 'tag_spec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 253 "parser.mly"
               ( )
# 829 "parser.ml"
               : 'tag_ne_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tag_ne_list) in
    Obj.repr(
# 254 "parser.mly"
                           ( )
# 837 "parser.ml"
               : 'tag_ne_list))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Specification.t list)
;;
