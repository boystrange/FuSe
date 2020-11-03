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

module S = Session
module Session = Session.Bare

let client_with_overlap ep =
  let _ = Session.send 1 ep in
  let ep = Session.send 2 ep in (* overlapping use of ep *)
  Session.close ep

let server ep =
  let n, ep = Session.receive ep in
  Session.close ep;
  print_int n

let test c s =
  try
    c (Service.request (Service.spawn s));
    assert false
  with
    S.InvalidEndpoint ->
    print_endline "Invalid endpoint usage detected!"

let _ =
  test client_with_overlap server
