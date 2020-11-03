(** OCaml implementation of binary sessions.
 @author Luca Padovani and Hernán Melgratti
 @version 0.7
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

(** Exception raised whenever an invalid endpoint is used. *)
exception InvalidEndpoint

(** Empty type. *)
type _0

(** The type of endpoints for {e receiving} messages of type
['a] and {e sending} messages of type ['b]. *)
type (+'a, -'b) st

(** The type of endpoints that can only be closed. *)
type et = (_0, _0) st

(** The type of endpoints for {e receiving} messages of type
['a]. *)
type +'a it = ('a, _0) st

(** The type of endpoints for {e sending} messages of type
['a]. *)
type -'a ot = (_0, 'a) st

(** The type of a sequentially composed protocol, where ['a] is the
protocol to be performed first, followed by ['b]. *)
type (+'a, +'b) seq

(** The type of a binary choice, where ['a] is the protocol to be
performed if [`True] is selected and ['b] is the protocol to be
performed if [`False] is selected. *)
type (+'a, +'b) choice = [ `True of 'a | `False of 'b ]

module Bare : sig
  (** {2 Session initiation and termination} *)

  (** [create ()] creates a new session.  @return a pair with two
valid endpoints and dual types. *)
  val create : ?name:string -> unit -> ('a, 'b) st * ('b, 'a) st

  (** [close ep] closes endpoint [ep].  @raise InvalidEndpoint if the
 endpoint [ep] is invalid. *)
  val close : et -> unit

  (** {2 Basic message passing} *)

  (** [send e ep] sends [e] on the endpoint [ep] with output
 capability.  @return the endpoint [ep].  @raise InvalidEndpoint if
 [ep] is invalid. *)
  val send : 'm -> ('m * ('a, 'b) st) ot -> ('b, 'a) st

  (** [receive ep] receives a message from the endpoint [ep] with
 input capability.  @return a pair [(v, ep)] with the received message
 [v] and the endpoint [ep].  @raise InvalidEndpoint if the endpoint
 [ep] is invalid.  *)
  val receive : ('m * ('a, 'b) st) it -> 'm * ('a, 'b) st

  (** {2 Choices} *)

  (** [select f ep] sends [f] to the peer endpoint of [ep], where it
is used to compute the received message.  @return the endpoint [ep].
@raise InvalidEndpoint if the endpoint [ep] is invalid. *)
  val select : (('a, 'b) st -> 'm) -> 'm ot -> ('b, 'a) st

  (** [select_true ep] selects the [True] branch of a choice.  @return
 the endpoint [ep] after the selection.  @raise InvalidEndpoint if the
 endpoint [ep] is invalid. *)
  val select_true : [> `True of ('a, 'b) st] ot -> ('b, 'a) st

  (** [select_false ep] selects the [False] branch of a choice.
 @return the endpoint [ep] after the selection.  @raise
 InvalidEndpoint if the endpoint [ep] is invalid. *)
  val select_false : [> `False of ('a, 'b) st] ot -> ('b, 'a) st

  (** [branch ep] receives a selection from the endpoint [ep] with
 input capability.  @return the endpoint [ep] injected through the
 selected tag.  @raise InvalidEndpoint if the endpoint [ep] is
 invalid.  *)
  val branch : ([>] as 'm) it -> 'm

  (** {2 Endpoint validity and identity} *)

  (** [is_valid ep] determines whether [ep] is a valid endpoint or not.
 @return [true] if [ep] is valid, [false] otherwise. *)
  val is_valid : ('a, 'b) st -> bool

  (** [acquire ep] acquires the endpoint [ep], if it is valid.  @return
 the unique valid reference to the endpoint [ep].  @raise
 InvalidEndpoint if [ep] invalid. *)
  val acquire : ('a, 'b) st -> ('a, 'b) st

  (** [try_acquire ep] attempts to acquire the endpoint [ep].  @return
 [Some ep] where [ep] is the unique valid reference to the endpoint,
 if [ep] is valid, and [None] otherwise. *)
  val try_acquire : ('a, 'b) st -> ('a, 'b) st option

  (** [same_session ep ep'] checks whether [ep] and [ep'] are endpoints
 of the same session (but not necessarily peer endpoints).  @return
 [true] if [ep] and [ep'] are (possibly peer) endpoints pertaining the
 same session, [false] otherwise. *)
  val same_session : ('a, 'b) st -> ('c, 'd) st -> bool

  (** [string_of_endpoint ep] returns a textual representation of the
endpoint [ep]. *)
  val string_of_endpoint : ('a, 'b) st -> string

  (** {2 Resumption combinators} *)

  (** [f @= ep] evaluates [f ep].  @return the pair to which [f ep]
 evaluates.  @raise InvalidEndpoint if the second component of [f ep]
 is an endpoint other than [ep]. *)
  val (@=) :
    (('a, 'b) st -> 'm * et) ->
    ((('a, 'b) st, ('c, 'd) st) seq, (('b, 'a) st, ('d, 'c) st) seq) st ->
    'm * ('c, 'd) st

  (** [f @> ep] evaluates [f ep].  @return the endpoint [ep].  @raise
 InvalidEndpoint if [f ep] evaluates to an endpoint different from
 [ep]. *)
  val (@>) :
    (('a, 'b) st -> et) ->
    ((('a, 'b) st, ('c, 'd) st) seq, (('b, 'a) st, ('d, 'c) st) seq) st ->
    ('c, 'd) st
end

module Monadic : sig
  (** The type of a computation that returns a result of type ['a]
  while using a session endpoint and changing its type from ['t0] to
  ['t1]. *)
  type ('t0, 't1, 'a) t

  (** [return e] is the trivial monadic computation that does not
  perform communications and returns the value of [e]. *)
  val return  : 'm -> ('t0, 't0, 'm) t

  (** The monadic composition operator. *)
  val (>>=) : ('t0, 't1, 'a) t -> ('a -> ('t1, 't2, 'b) t) -> ('t0, 't2, 'b) t

  (** [m1 >>> m2] is a shortcut for [m1 >>= fun _ -> m2]. *)
  val (>>>) : ('t0, 't1, 'a) t -> ('t1, 't2, 'b) t -> ('t0, 't2, 'b) t

  (** Fixpoint operator for monadic computations. [fix (fun x -> m)]
  represents the same computation as [m] in which [x] is bound to [m]
  itself. *)
  val fix     : (('t0, 't1, 'a) t -> ('t0, 't1, 'a) t) -> ('t0, 't1, 'a) t

  (** [connect ms mc] creates a new session that connects the server
  [ms], spawned into a new thread, and the client [mc]. The result is
  that returned by the client. *)
  val connect : (('a, 'b) st, et, unit) t -> (('b, 'a) st, et, 'm) t -> 'm

  (** [receive] waits for a message from the session endpoint and
  returns its value. *)
  val receive : (('m * ('a, 'b) st) it, ('a, 'b) st, 'm) t

  (** [send e] sends the message [e] on the session endpoint. *)
  val send    : 'm -> (('m * ('b, 'a) st) ot, ('a, 'b) st, unit) t

  (** [branch mtrue mfalse] accepts a boolean selection from the
  session endpoint and executes either [mtrue] or [mfalse]
  accordingly. *)
  val branch  : ('t0, 't2, 'a) t -> ('t1, 't2, 'a) t -> (('t0, 't1) choice it, 't2, 'a) t

  (** [select_true] selects the [True] branch of a choice. *)
  val select_true : ((('b, 'a) st, ('d, 'c) st) choice ot, ('a, 'b) st, unit) t

  (** [select_false] selects the [False] branch of a choice. *)
  val select_false : ((('b, 'a) st, ('d, 'c) st) choice ot, ('c, 'd) st, unit) t
end

