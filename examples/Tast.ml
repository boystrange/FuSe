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
            
let producer_prefix = "producer "
let consumer_prefix = "consumer "
                        
let swap (x, y) = (y, x)

let swap_bind ((x, y), z) = ((x, z), y)

let x_y_seq x y f u v = swap (x (fun u -> swap (y (f u) v)) u)

let x_y_z_seq x y z f u v w =
  let (v, w), u =
    x
      (fun u ->
        let (u, w), v =
          y (fun v -> swap_bind (z (f u v) w)) v
        in (v, w), u
      ) u
  in (u, v), w

let (@@=) f u v = x_y_seq (@=) (@=) f u v
let (@@@=) f u v w = x_y_z_seq (@=) (@=) (@=) f u v w
            
let consumer c =
  let rec aux c =
    match S.branch c with
    | `Const c -> S.receive c
    | `Add c ->
       let x, c = aux @= c in
       let y, c = aux @= c in (x + y), c
    | `Sub c ->
       let x, c = aux @= c in
       let y, c = aux @= c in (x - y), c
  in let res, c = aux c in
     print_endline ("result = " ^ string_of_int res);
     S.close c
              
let _Const x = `Const x
let _Add x = `Add x
let _Sub x = `Sub x
              
let rec tee src dst1 dst2 =
  match S.branch src with
  | `Const src ->
     let dst1 = S.select _Const dst1 in
     let dst2 = S.select _Const dst2 in
     let n, src = S.receive src in
     let dst1 = S.send n dst1 in
     let dst2 = S.send n dst2 in
     (src, dst1), dst2
  | `Add src ->
     let dst1 = S.select _Add dst1 in
     let dst2 = S.select _Add dst2 in
     let (src, dst1), dst2 = (@@@=) tee src dst1 dst2 in
     let (src, dst1), dst2 = (@@@=) tee src dst1 dst2 in
     (src, dst1), dst2
  | `Sub src ->
     let dst1 = S.select _Sub dst1 in
     let dst2 = S.select _Sub dst2 in
     let (src, dst1), dst2 = (@@@=) tee src dst1 dst2 in
     let (src, dst1), dst2 = (@@@=) tee src dst1 dst2 in
     (src, dst1), dst2
       
let transformer src dst =
  let rec aux neg src dst =
    match S.branch src with
    | `Const src ->
       let dst = S.select _Const dst in
       let n, src = S.receive src in
       let dst = S.send (neg * n) dst in
       (src, dst)
    | `Add src ->
       let dst = S.select _Add dst in
       let src, dst = (@@=) (aux neg) src dst in
       let src, dst = (@@=) (aux neg) src dst in
       (src, dst)
    | `Sub src ->
       let dst = S.select _Add dst in
       let src, dst = (@@=) (aux neg) src dst in
       let src, dst = (@@=) (aux (-neg)) src dst in
       (src, dst)
  in aux 1 src dst
                   
let rec producer depth c =
  if depth = 0 then
    let c = S.select _Const c in
    let c = S.send (Random.int 101 - 200) c in c
  else
    let op = if Random.bool () then _Add else _Sub in
    let c = S.select op c in
    let c = producer (depth - 1) @> c in
    let c = producer (depth - 1) @> c in c

let _ =
  let a, b = S.create () in
  let c, d = S.create () in
  let e, f = S.create () in
  let g, h = S.create () in
  let _ = Thread.create (producer (int_of_string Sys.argv.(1))) a in
  let _ = Thread.create (tee b c) e in
  let _ = Thread.create consumer d in
  let _ = Thread.create (transformer f) g in
  consumer h
