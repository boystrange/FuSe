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

module Session = Session.Bare
	     
let opt_echo_client ep opt x =
  if opt then
    let ep = Session.select (fun x -> `Msg x) ep in
    let ep = Session.send x ep in
    let reply, ep = Session.receive ep in
    Session.close ep;
    reply
  else
    let ep = Session.select (fun x -> `End x) ep in
    Session.close ep; x
    
let opt_echo_service ep =
    match Session.branch ep with
	`Msg ep ->  let x, ep = Session.receive ep in
		    let ep = Session.send x ep in
		    Session.close ep
      | `End ep ->  Session.close ep
        
let _ =
  let a, b = Session.create () in   
  let _ = Thread.create opt_echo_service a in 
  print_string (opt_echo_client b true "Hello")
