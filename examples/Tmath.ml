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

let rec server s =
  match S.branch s with
    `Quit s -> S.close s
  | `Plus s -> let n, s = S.receive s in
	       let m, s = S.receive s in
	       let s = S.send (n + m) s in
	       server s
  | `Mult s -> let n, s = S.receive s in
	       let m, s = S.receive s in
	       let s = S.send (n * m) s in
	       server s
  | `Div s ->  let n, s = S.receive s in
               let m, s = S.receive s in
               let s = S.send (n / m) s in
               let s = S.send (n mod m) s in
               server s
  | `Neg s  -> let n, s = S.receive s in
	       let s = S.send (-n) s in
	       server s
  | `Eq s   -> let n, s = S.receive s in
	       let m, s = S.receive s in
	       let s = S.send (n = m) s in
	       server s

let factorial n s =
  let rec aux acc n s =
    if n = 0 then
      let s = S.select (fun x -> `Quit x) s in
      S.close s; acc
    else
      let s = S.select (fun x -> `Mult x) s in
      let s = S.send acc s in
      let s = S.send n s in
      let res, s = S.receive s in
      aux res (n - 1) s
  in aux 1 n s

let prime n =
  let rec aux k s =
    if k > n / 2 then
      let s = S.select (fun x -> `Quit x) s in
      S.close s;
      true
    else
      let s = S.select (fun x -> `Div x) s in
      let s = S.send n s in
      let s = S.send k s in
      let _, s = S.receive s in
      let r, s = S.receive s in
      if r = 0 then
        begin
          S.close (S.select (fun x -> `Quit x) s);
          false
        end
      else
        aux (k + 1) s
  in
  aux 2
            
let _ =
  let ch = Service.spawn server in
  print_int (factorial 10 (Service.request ch));
  print_newline ();
  print_endline (if prime 131 (Service.request ch) then "true" else "false")

