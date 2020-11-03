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
	     
let rec_math_service =
  let rec aux ep =
    match Session.branch ep with
    | `Add ep ->    let x, ep = Session.receive ep in
		   let y, ep = Session.receive ep in
		   let ep = Session.send (x + y) ep in
		   aux ep
    | `Dec ep ->    let x, ep = Session.receive ep in
		   let ep = Session.send (x - 1) ep in
		   aux ep
    | `IsZero ep -> let x, ep = Session.receive ep in
		   let ep = Session.send (x == 0) ep in
		   aux ep
    | `Quit ep ->   Session.close ep
  in aux

let rec_math_client ep =
  let rec aux acc ep n =
    let ep = Session.select (fun x -> `IsZero x) ep in
    let ep = Session.send n ep in
    let is_zero, ep = Session.receive ep in
    if is_zero then
      let ep = Session.select (fun x -> `Quit x) ep in
      Session.close ep;
      acc
    else
      let ep = Session.select (fun x -> `Add x) ep in
      let ep = Session.send acc ep in
      let ep = Session.send n ep in
      let acc, ep = Session.receive ep in
      let ep = Session.select (fun x -> `Dec x) ep in
      let ep = Session.send n ep in
      let n, ep = Session.receive ep in
      aux acc ep n
  in aux 0 ep
       
let _ =
  let a, b = Session.create () in
  let _ = Thread.create rec_math_service a in
  print_int (rec_math_client b 10)
  
