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

let rec split_string s =
  try
    let i = String.index s '.' in
    let s1 = String.sub s 0 i in
    let s2 = String.sub s i (String.length s - i) in
    s1 :: split_string s2
  with
    Not_found -> [s]
      
let rec pp_specification =
  function
  | Specification.Type t ->
     Ast.pp (Ast.decode t)

  | Specification.Val (x, t) ->
     Format.open_hvbox 2;
     Format.print_string ("val " ^ x ^ " :");
     Format.print_break 1 0;
     Ast.pp (Ast.decode t);
     Format.close_box ()
     
  | Specification.Module (name, sl) ->
     Format.open_vbox 0;
     Format.open_vbox 2;
     Format.print_string ("module " ^ name ^ " : sig");
     Format.print_cut();
     pp_specifications sl;
     Format.close_box ();
     Format.print_cut ();
     Format.print_string "end";
     Format.close_box ()
     
and pp_specifications =
  function
  | [] -> ()
  | [x] -> pp_specification x
  | x :: xs -> pp_specification x;
	       Format.print_cut ();
	       pp_specifications xs
  
let _ =
  let options =
    [ "-margin", Arg.Int Format.set_margin, "sets the right margin (in characters)";
      "-show-end", Arg.Set Configuration.show_end, "show trailing end";
      "-sequence-polarity", Arg.Set Configuration.sequence_polarity, "show sequence polarity";
      "-prefix", Arg.String (fun s -> Configuration.set_prefix (split_string s)), "set module path (default is Session)";
      "-no-prefix", Arg.Unit Configuration.reset_prefix, "reset module path (default is Session)";
      "-session-type", Arg.Set_string Configuration.session_type, "sets the session type (default is st)";
    ]
  in
  Arg.parse options (fun _ -> ()) "Usage: rosetta [OPTIONS]";
  try
    let lexbuf = Lexing.from_channel stdin in
    Format.open_vbox 0;
    pp_specifications (Parser.main Lexer.token lexbuf);
    Format.print_cut ();
    Format.close_box ()
  with
  | Lexer.UnexpectedCharacter ch ->
     print_endline ("unexpected character " ^ String.make 1 ch);
     exit 1
  | Parsing.Parse_error ->
     print_endline ("syntax error on line " ^ string_of_int (Lexer.get_line ()));
     exit 1
