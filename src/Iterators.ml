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

open Session.Bare

let select_iter f =
  let rec aux xs ep =
    match xs with
    | [] -> select_false ep
    | x :: xs ->
       let ep = select_true ep in
       let ep = f x @> ep in
       aux xs ep
  in aux

let accept_iter f =
  let rec aux ep =
    match branch ep with
    | `False ep -> ep
    | `True ep -> aux (f @> ep)
  in aux

let select_map f =
  let rec aux ys xs ep =
    match xs with
    | [] -> List.rev ys, select_false ep
    | x :: xs ->
       let ep = select_true ep in
       let y, ep = f x @= ep in
       aux (y :: ys) xs ep
  in aux []

let accept_map f =
  let rec aux xs ep =
    match branch ep with
    | `False ep -> List.rev xs, ep
    | `True ep ->
       let x, ep = f @= ep in
       aux (x :: xs) ep
  in aux []

let select_fold f =
  let rec aux acc xs ep =
    match xs with
    | [] -> acc, select_false ep
    | x :: xs ->
       let ep = select_true ep in
       let acc, ep = f acc x @= ep in
       aux acc xs ep
  in aux
       
let accept_fold f =
  let rec aux acc ep =
    match branch ep with
    | `False ep -> acc, ep
    | `True ep ->
       let acc, ep = f acc @= ep in
       aux acc ep
  in aux
