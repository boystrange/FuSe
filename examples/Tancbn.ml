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

module S = Session.Bare

let (@>) = S.(@>)
let (@=) = S.(@=)

module NotPrecise = struct
  let rec work x y =
    match S.branch x with
    | `A x -> let u, v = S.create () in
              let _ = Thread.create (work x) u in
              let x, v = S.receive v in
              S.close v;
              ( match S.branch x with
                | `B x -> S.send x y )
    | `C x -> S.send x y

(*
  let ab x =
    let u, v = create () in
    let _ = Thread.create (work x) u in
    let x, v = receive v in
    close v;
    close x
*)
end

module Resumed = struct
  let rec work x y =
    match S.branch x with
    | `A x -> let u, v = S.create () in
              let _ = Thread.create (work x) u in
              let x, v = S.receive @= v in
              S.close v;
              ( match S.branch x with
                | `B x -> S.send x @> y )
    | `C x -> S.send x @> y

(*
  let ab x =
    let u, v = S.create () in
    let _ = Thread.create (work x) u in
    let x, v = S.receive @= v in
    S.close v;
    S.close x
*)
end
