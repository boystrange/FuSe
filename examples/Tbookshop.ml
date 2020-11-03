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
	     
let rec shopLoop s order =
  match Session.branch s with
    `Add s -> let book, s = Session.receive s in
	      shopLoop s (book :: order)
  | `CheckOut s -> let card, s = Session.receive s in
		   let address, s = Session.receive s in
		   Session.close s
					   
let shop shopAccess =
  shopLoop (Service.accept shopAccess) []

let isChildrensBook book = true
  
let voucher card address c book =
  let c = if isChildrensBook book then
	    let c = Session.select (fun x -> `Add x) c in
	    Session.send book c
	  else c in
  let c = Session.select (fun x -> `CheckOut x) c in
  let c = Session.send card c in
  let c = Session.send address c in
  Session.close c

let mother card address shopAccess sonAccess book =
  let c = Service.request shopAccess in
  let c = Session.select (fun x -> `Add x) c in
  let c = Session.send book c in
  let s = Service.request sonAccess in
  let s = Session.send (voucher card address c) s in
  (* let sonBook, s = Session.receive s in *)
  Session.close s

let son sonAccess book =
  let s = Service.accept sonAccess in
  let f, s = Session.receive s in
  f book;
  f book;
  (* let s = Session.send (f book) s in *)
  Session.close s

let _ =
  let mCard = "0123 4567 7654 3210" in
  let mAddress = "17 session type rd" in
  let mBook = "Life of Ada Lovelace" in
  let sBook = "Types and programming languages" in
  let shopAccess = Service.create () in
  let sonAccess = Service.create () in
  let _ = Thread.create shop shopAccess in
  let _ = Thread.create (son sonAccess) sBook in
  mother mCard mAddress shopAccess sonAccess mBook
