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

module Session = Session.Bare

type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

let (@>) = Session.(@>)
let (@=) = Session.(@=)

let __Leaf x = `Leaf x
let __Node x = `Node x

module Raw = struct
  let send_tree t0 c =
    let rec aux t c =
      match t with
      | Leaf -> Session.select __Leaf c
      | Node (v, l, r) -> let c = Session.select __Node c in
                          let c = Session.send v c in
                          let c = aux l c in
                          let c = aux r c in c
    in
    Session.select (fun x -> `Done x) (aux t0 c)

  let receive_tree c =
    let rec aux c =
      match Session.branch c with
      | `Leaf c -> Leaf, c
      | `Node c -> let v, c = Session.receive c in
		   let l, c = aux c in
		   let r, c = aux c in
		   Node (v, l, r), c
      | _ -> assert false (* impossible *)
    in
    let t, c = aux c in
    match Session.branch c with
    | `Done c -> t, c
    | _ -> assert false (* impossible *)
end

module NotPrecise = struct
  let rec send_tree t ep =
    match t with
    | Leaf -> Session.select (fun x -> `Leaf x) ep
    | Node (v, l, r) -> let ep = Session.select (fun x -> `Node x) ep in
		       let ep = Session.send v ep in
                       let ep = send_tree l ep in
                       let ep = send_tree r ep in ep
end
		      
module Resumed = struct
  let rec send_tree t ep =
    match t with
    | Leaf -> Session.select (fun x -> `Leaf x) ep
    | Node (v, l, r) -> let ep = Session.select (fun x -> `Node x) ep in
                       let ep = Session.send v ep in
                       let ep = send_tree l @> ep in
                       let ep = send_tree r ep in ep

  let rec receive_tree ep =
    match Session.branch ep with
    | `Leaf ep -> Leaf, ep
    | `Node ep -> let v, ep = Session.receive ep in
		 let l, ep = receive_tree @= ep in
		 let r, ep = receive_tree ep in
	         Node (v, l, r), ep

  let rec transform t c =
    match t with
    | Leaf -> Leaf, Session.select __Leaf c
    | Node (v, l, r) ->
       let c = Session.select __Node c in
       let c = Session.send v c in
       let l', c = transform l @= c in
       let r', c = transform r @= c in
       let v', c = Session.receive c in
       Node (v', l', r'), c

  let rec tree_sum c =
    match Session.branch c with
    | `Leaf c -> 0, c
    | `Node c -> let x, c = Session.receive c in
	         let l, c = tree_sum @= c in
	         let r, c = tree_sum @= c in
	         let c = Session.send (x + l + r) c in
	         x + l + r, c
end
                   
let a_tree = Node (3, Leaf, Node (4, Leaf, Leaf))

let _ =
  let c, s = Session.create () in
  let _ = Thread.create Resumed.tree_sum s in
  Resumed.transform a_tree c
