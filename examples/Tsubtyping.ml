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
	     
type in_A   = [ `A of Session.et ] Session.it
type out_A  = [ `A of Session.et ] Session.ot
type in_AB  = [ `A of Session.et | `B of Session.et ] Session.it
type out_AB = [ `A of Session.et | `B of Session.et ] Session.ot

type in_in_A   = (in_A * Session.et) Session.it  (* ?[?[A].end].end   *)
type in_in_AB  = (in_AB * Session.et) Session.it (* ?[?[A+B].end].end *)
type out_in_A  = (in_A * Session.et) Session.ot  (* ![?[A].end].end   *)
type out_in_AB = (in_AB * Session.et) Session.ot (* ![?[A+B].end].end *)

type in_int_in_A = (int * in_A) Session.it       (* ?int.?[A].end     *)
type in_int_in_AB = (int * in_AB) Session.it     (* ?int.?[A+B].end   *)
type out_int_out_A = (int * in_A) Session.ot     (* !int.![A].end     *)
type out_int_out_AB = (int * in_AB) Session.ot   (* !int.![A+B].end   *)
       
let select_A (s : out_A) =
  S.close (S.select (fun x -> `A x) s)
  
let select_AB (s : out_AB) =
  S.close
    (if true then S.select (fun x -> `A x) s
     else S.select (fun x -> `B x) s)

let branch_A (s : in_A) =
  S.close
    (match S.branch s with
       `A x -> x)

let branch_AB (s : in_AB) =
  S.close
    (match S.branch s with
       `A x -> x
     | `B x -> x)

let (a : in_A),  (b : out_A)  = S.create ()
let (c : in_AB), (d : out_AB) = S.create ()

let test1 x = (x : in_A :> in_AB)                   (* covariant subtyping         *)
let test2 x = (x : out_AB :> out_A)                 (* contravariant subtyping     *)
let test3 x = (x : in_in_A :> in_in_AB)             (* H/O covariant subtyping     *)
let test4 x = (x : out_in_AB :> out_in_A)           (* H/O contravariant subtyping *)
let test5 x = (x : in_int_in_A :> in_int_in_AB)     (* covariant tail subtyping    *)
let test6 x = (x : out_int_out_AB :> out_int_out_A) (* covariant tail subtyping    *)
		
let foo () =
  branch_A a;
  branch_AB c;
  branch_AB (a : in_A :> in_AB);   (* covariant subtyping *)
  select_A b;
  select_AB d;
  select_A (d : out_AB :> out_A)   (* contravariant subtyping *)
