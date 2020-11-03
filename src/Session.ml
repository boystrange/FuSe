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

exception InvalidEndpoint

module UnsafeChannel : sig
  type t
  val create     : unit -> t
  val send       : 'a -> t -> unit
  val receive    : t -> 'a
end = struct
  type t         = unit Event.channel
  let create     = Event.new_channel
  let send x ch  = Event.sync (Event.send ch (Obj.magic x))
  let receive ch = Obj.magic (Event.sync (Event.receive ch))
end

module Flag : sig
  type t
  val create     : unit -> t
  val use        : t -> unit
  val try_use    : t -> bool
  val is_valid   : t -> bool
end = struct
  type t         = bool ref
  let create ()  = ref true
  let use f      =
    (* BEGIN ATOMIC *)
    if !f then f := false else raise InvalidEndpoint
    (* END ATOMIC *)
  let try_use f  = let valid = !f in f := false; valid
  let is_valid f = !f
end

type _0
type (+'a, -'b) st = { name     : string;
		       channel  : UnsafeChannel.t;
		       polarity : int;
		       once     : Flag.t }
type et            = (_0, _0) st
type +'a it        = ('a, _0) st
type -'a ot        = (_0, 'a) st
type (+'a, +'b) seq
type (+'a, +'b) choice = [ `True of 'a | `False of 'b ]

module Bare = struct
  let fresh ep = { ep with once = Flag.create () }

  (**********************************)
  (*** INITIATION AND TERMINATION ***)
  (**********************************)

  let create ?(name = "channel") () =
    let ch = UnsafeChannel.create () in
    let ep1 = { name = name ^ "⁺";
                channel = ch;
                polarity = +1;
                once = Flag.create () }
    and ep2 = { name = name ^ "⁻";
                channel = ch;
                polarity = -1;
                once = Flag.create () }
    in (ep1, ep2)

  let close ep = Flag.use ep.once

  (****************)
  (*** IDENTITY ***)
  (****************)

  let same_session ep ep' = ep.channel == ep'.channel
  let same_endpoint ep ep' = same_session ep ep' && ep.polarity == ep'.polarity
  let string_of_endpoint ep = ep.name

  (*****************)
  (*** LINEARITY ***)
  (*****************)

  let is_valid ep = Flag.is_valid ep.once
  let acquire ep = Flag.use ep.once; fresh ep
  let try_acquire ep = if Flag.try_use ep.once then Some (fresh ep) else None

  (***********************)
  (*** MESSAGE PASSING ***)
  (***********************)

  let send x ep  = Flag.use ep.once; UnsafeChannel.send x ep.channel; fresh ep
  let receive ep = Flag.use ep.once; (UnsafeChannel.receive ep.channel, fresh ep)

  (***************)
  (*** CHOICES ***)
  (***************)

  let select f ep     = Flag.use ep.once; UnsafeChannel.send f ep.channel; fresh ep
  let select_true ep  = select (fun x -> `True x) ep
  let select_false ep = select (fun x -> `False x) ep
  let branch ep       = Flag.use ep.once; (UnsafeChannel.receive ep.channel) (fresh ep)

  (******************************)
  (*** SEQUENTIAL COMPOSITION ***)
  (******************************)

  let (@=) scope ep =
    let result, ep' = scope (Obj.magic ep) in
    if same_endpoint ep ep' then (result, Obj.magic ep')
    else raise InvalidEndpoint

  let (@>) scope ep =
    snd ((fun ep -> ((), scope ep)) @= ep)
end

module Monadic = struct
  type ('t0, 't1, 'a) t = 't0 -> ('a * 't1)

  let (>>=) m f ep = let x, ep = m ep in f x ep
  let (>>>) m1 m2 = m1 >>= fun _ -> m2
  let return m ep = (m, ep)

  let rec fix f = f (fun ep -> fix f ep)

  let connect ms mc =
    let eps, epc = Bare.create () in
    let _ = Thread.create (fun ep -> let (), ep = ms ep in Bare.close ep) eps in
    let x, epc = mc epc in
    Bare.close epc;
    x

  let receive = Bare.receive
  let send x ep = let ep = Bare.send x ep in ((), ep)
  let select_true ep = let ep = Bare.select_true ep in ((), ep)
  let select_false ep = let ep = Bare.select_false ep in ((), ep)
  let branch m1 m2 ep =
    match Bare.branch ep with
    | `True ep -> m1 ep
    | `False ep -> m2 ep
end
