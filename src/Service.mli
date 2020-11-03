(** OCaml implementation of featherweight services.
 @author Luca Padovani
 @version 0.4
 *)

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

(** The type of a service that accepts sessions of type ['a]. *)
type 'a t

(** [create ()] creates a new service. *)
val create  : unit -> 'a t

(** [accept s] waits for a connection from a client to service [s].
 @return the endpoint of the established session. *)
val accept  : ('a, 'b) Session.st t -> ('a, 'b) Session.st

(** [request s] requests a connection to a service [s].
 @return the endpoint of the established session. *)
val request : ('a, 'b) Session.st t -> ('b, 'a) Session.st

(** [spawn f] creates a persistent service that spawns a new thread [f ep] for each connection with endpoint [ep] accepted from a client. *)
val spawn   : (('a, 'b) Session.st -> unit) -> ('a, 'b) Session.st t
