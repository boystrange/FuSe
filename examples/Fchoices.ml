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
	     
let math_service ep =
  match Session.branch ep with
    `Add ep -> let x, ep = Session.receive ep in
	      let y, ep = Session.receive ep in
	      let ep = Session.send (x + y) ep in
	      Session.close ep
  | `Inc ep -> let x, ep = Session.receive ep in
	      let ep = Session.send (x + 1) ep in
	      Session.close ep

let math_client ep x y =
  let ep = Session.select (fun x -> `Mul x) ep in (* select `Mul operation *)
  let ep = Session.send x ep in
  let ep = Session.send y ep in
  let result, ep = Session.receive ep in
  Session.close ep;
  result

let _ =
  let a, b = Session.create () in
  let _ = Thread.create math_service a in
  print_int (math_client b 1 2)
