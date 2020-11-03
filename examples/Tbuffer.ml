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

open Session.Bare
             
let stack_skeleton =
  let rec none u =
    match branch u with
    | `Push u -> none (some @> u)
    | `Done u -> u
  and some u =
    match branch u with
    | `Push u -> some (some @> u)
    | `Pop u  -> u
  in none

let stack_raw =
  let rec none u =
    match branch u with
    | `Push u -> let x, u = receive u in
		 none (some x u)
    | `Done u -> u
  and some x u =
    match branch u with
    | `Push u -> let y, u = receive u in
		 some x (some y u)
    | `Pop u  -> send x u
  in none

let stack_raw_2 =
  let rec aux u =
    match branch u with
    | `Push u -> let x, u = receive u in
		 let u = aux u in
		 let `Pop u = branch u in
		 let u = send x u in
		 aux u
    | `Done u -> u
  in aux
	
let rec client l u =
  let rec aux l u =
    match l with
    | [] -> u
    | x :: xs -> let u = select (fun x -> `Push x) u in
		 let u = send x u in
		 let u = client xs @> u in
		 let u = select (fun x -> `Pop x) u in
		 let y, u = receive u in
		 assert (x = y);
		 u
  in select (fun x -> `Done x) (aux l u)
  
let stack =
  let rec none u =
    match branch u with
    | `Push u -> let x, u = receive u in
		 none (some x @> u)
    | `End u  -> u
  and some x u =
    match branch u with
    | `Push u -> let y, u = receive u in
		 some x (some y @> u)
    | `Pop u  -> send x u
  in none
       
