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
(* Copyright 2015-2017 Luca Padovani                                    *)

module Session = Session.Bare

let (@>) = Session.(@>)
let (@=) = Session.(@=)

let id x = x
let __Nil x = `Nil x
let __Cons x = `Cons x

let client l ep =
  let rec aux l ep =
    match l with
    | [] -> ([], Session.select __Nil ep)
    | x :: xs -> let ep = Session.select __Cons ep in
                 let ep = Session.send x ep in
                 let ys, ep = aux xs @= ep in
                 let y, ep = Session.receive ep in
                 (y :: ys, ep)
  in
  let l', ep = aux l ep in
  assert (l = l');
  Session.close ep

let sort_server ep =
  let rec aux xs ep =
    match Session.branch ep with
    | `Nil ep -> List.sort compare xs, ep
    | `Cons ep -> let x, ep = Session.receive ep in
                  let ys, ep = aux (x :: xs) @= ep in
                  List.tl ys, Session.send (List.hd ys) ep
  in
  let _, ep = aux [] ep in
  Session.close ep

let _ =
  let c, s = Session.create () in
  let _ = Thread.create sort_server s in
  client [5; 4; 3; 2; 1] c
