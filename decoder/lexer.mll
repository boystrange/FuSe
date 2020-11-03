(* This file is part of FuSe.                                           *)
(*                                                                      *)
(* FuSe is free software: you can redistribute it and/or modify         *)
(* it under the terms of the GNU General Public License as published by *)
(* the Free Software Foundation, either version 3 of the License, or    *)
(* (at your option) any later version.                                  *)
(*                                                                      *)
(* FuSe is distributed in the hope that it will be useful,              *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of       *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *)
(* GNU General Public License for more details.                         *)
(*                                                                      *)
(* You should have received a copy of the GNU General Public License    *)
(* along with FuSe.  If not, see <http://www.gnu.org/licenses/>.        *)
(*                                                                      *)
(* Copyright 2015-2016 Luca Padovani                                    *)

{

open Parser
exception UnexpectedCharacter of char

let line_num = ref 1
let keyword_table = Hashtbl.create 53
let _ =
  List.iter
    (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
    [ "val", VAL;
      "sig", SIG;
      "end", END;
      "exception", EXCEPTION;
      "module", MODULE;
      "type", TYPE;
      "mutable", MUTABLE;
      "as", AS;
      "and", AND;
      "of", OF; ]

let get_line () = !line_num
                
}

let space  = ' '|'\t'|'\r'
let digit  = ['0'-'9']
let letter = ['a'-'z''A'-'Z']
let id     = (letter | '_') (letter | digit | '_' | '\'')*
let lid    = (['a'-'z'] | '_') (letter | digit | '_' | '\'')*
let cid    = ['A'-'Z'] (letter | digit | '_' | '\'')*
let opchar = '!'|'$'|'%'|'&'|'*'|'+'|'-'|'.'|'/'|':'|'<'|'='|'>'|'?'|'@'|'^'|'|'|'~'
let oid    = opchar opchar+
	       
rule token = parse
  '\n' { incr line_num; token lexbuf }
           
| space { token lexbuf }

| lid as s
  {
    try
      Hashtbl.find keyword_table s
    with
      Not_found -> LID s
  }

| cid as s { CID s }

| '(' { LPAREN }
| ')' { RPAREN }
| '[' { LBRACK }
| ']' { RBRACK }
| '{' { LBRACE }
| '}' { RBRACE }
| ',' { COMMA }
| '\'' { TICK }
| '`' { BACKTICK }
| "->" { ARROW }
| '=' { EQ }
| '>' { GT }
| '<' { LT }
| ':' { COLON }
| ';' { SEMICOLON }
| '.' { DOT }
| '|' { OR }
| '*' { STAR }
| ".." { ELLIPSIS }
| '_' { UNDERSCORE }
| '?' { QMARK }
| oid as s { OID s }
| _ as ch
  { 
    prerr_endline ("invalid character " ^ String.make 1 ch);
    raise (UnexpectedCharacter ch)
  }
| eof { EOF }
