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

type 'a t = 'a Event.channel

let create = Event.new_channel

let accept ch = Event.sync (Event.receive ch)

let request ch =
  let a, b = Session.Bare.create () in
  Event.sync (Event.send ch a); b

let spawn f =
  let ch = create () in
  let rec server f =
    let _ = Thread.create f (accept ch) in
    server f
  in
  let _ = Thread.create server f in
  ch
