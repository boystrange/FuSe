/* This file is part of FuSe.                                           */
/*                                                                      */
/* FuSe is free software: you can redistribute it and/or modify         */
/* it under the terms of the GNU General Public License as published by */
/* the Free Software Foundation, either version 3 of the License, or    */
/* (at your option) any later version.                                  */
/*                                                                      */
/* FuSe is distributed in the hope that it will be useful,              */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of       */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        */
/* GNU General Public License for more details.                         */
/*                                                                      */
/* You should have received a copy of the GNU General Public License    */
/* along with FuSe.  If not, see <http://www.gnu.org/licenses/>.        */
/*                                                                      */
/* Copyright 2015-2016 Luca Padovani                                    */

%{
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
%}

%token <string> LID
%token <string> CID
%token <string> OID

%token LPAREN RPAREN
%token LBRACK RBRACK
%token LBRACE RBRACE
%token TICK
%token UNDERSCORE
%token QMARK
%token COLON
%token ARROW
%token STAR
%token COMMA
%token MUTABLE
%token AS
%token OF
%token AND
%token EXCEPTION
%token MODULE
%token TYPE
%token VAL
%token LT GT EQ
%token ELLIPSIS
%token SEMICOLON
%token SHARP
%token DOT
%token OR
%token BACKTICK
%token EOF
%token SIG
%token END
       
%nonassoc AS
%right ARROW
%nonassoc STAR
%nonassoc CID LID
%right OR
       
%start main

%type <Specification.t list> main
%type <(string * Ast.t) list> tag_spec_ne_list
			   
%%

main
: specification_list { $1 }
;

value_name
: LID { $1 }
| LPAREN OID RPAREN { $2 }
;
  
specification_list
: { [] }
| specification specification_list { cons_opt $1 $2 }
;
  
specification
: type_expr { Some (Specification.Type (expand $1)) }
| VAL value_name COLON type_expr { Some (Specification.Val ($2, expand $4)) }
| EXCEPTION constr_decl { None }
| MODULE CID EQ module_path { None }
| MODULE CID COLON SIG specification_list END { Some (Specification.Module ($2, $5)) }
| TYPE type_definition_ne_list { None }
;

type_definition_ne_list
: type_definition { }
| type_definition AND type_definition_ne_list { }
;

type_definition
: type_expr type_information { }
;
  
type_information
: { }
| EQ type_expr { }
| EQ constr_decl_ne_list { }
| EQ record_decl { }
;

module_path
: CID { }
| CID DOT module_path { }
;

constr_decl
: CID constr_args_opt { }
| CID COLON type_expr { }
;

constr_args_opt
: { }
| OF type_expr { }
;

constr_decl_ne_list
: constr_decl { }
| constr_decl OR constr_decl_ne_list { }
;
  
record_decl
: LBRACE field_decl_list RBRACE { }
;

field_decl_list
: { }
| field_decl SEMICOLON field_decl_list { }
;

field_decl
: mutable_opt LID COLON type_expr { }
;
  
mutable_opt
: { }
| MUTABLE { }
;
  
type_var
: TICK LID { $2 }
;
  
type_expr
: as_type_expr { $1 }
;

as_type_expr
: arrow_type_expr { $1 }
| as_type_expr AS type_var { Ast.Constructor (`As $3, [$1]) }
;
  
arrow_type_expr
: tuple_type_expr { of_tuple_type $1 }
| tuple_type_expr ARROW arrow_type_expr
  { Ast.Constructor (`Arrow, [of_tuple_type $1; $3]) }
;

tuple_type_expr
: atomic_type_expr { [$1] }
| atomic_type_expr STAR tuple_type_expr { $1 :: $3 }
;
  
atomic_type_expr
: type_var { Ast.Var $1 }
| QMARK LID COLON atomic_type_expr { $4 }
| LPAREN type_expr RPAREN { $2 }
| type_constr { Ast.Constructor (`Apply $1, []) }
| atomic_type_expr type_constr { Ast.Constructor (`Apply $2, [$1]) }
| LPAREN type_expr_comma_ne_list RPAREN type_constr { Ast.Constructor (`Apply $4, $2) }
| polymorphic_variant_type { Ast.Tagged (`Variant, $1) }
;

type_expr_comma_ne_list
: type_expr comma_type_expr_list { $1 :: $2 }
;

comma_type_expr_list
: { [] }
| COMMA type_expr_comma_ne_list { $2 }
;

type_constr
: LID { [$1] }
| CID DOT type_constr { $1 :: $3 }
;

polymorphic_variant_type
: LBRACK GT tag_spec_list RBRACK { $3 }
| LBRACK LT tag_spec_list RBRACK { $3 }
| LBRACK LT tag_spec_list GT tag_ne_list RBRACK { $3 }
| LBRACK tag_spec_list RBRACK { $2 }
;

tag_spec_list
: { [] }
| tag_spec_ne_list { $1 }
;

tag_spec_ne_list
: tag_spec { [$1] }
| tag_spec OR tag_spec_ne_list { $1 :: $3 }
;

tag_spec
: BACKTICK CID OF type_expr { ($2, $4) }
;

tag_ne_list
: BACKTICK CID { }
| BACKTICK CID tag_ne_list { }
;
  
%%
