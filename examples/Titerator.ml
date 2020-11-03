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
open Iterators
	     
let rec range m n =
  if m > n then [] else m :: range (m + 1) n

let string_of_list xs =
  "[" ^ String.concat ", " (List.map string_of_int xs) ^ "]"

let echo_service ep =
  Session.close
    (accept_iter
       (fun ep -> let x, ep = Session.receive ep in
                 print_endline x; ep) ep)

let echo_client ep l =
  Session.close (select_iter Session.send l ep)

let echo_service_2 ep =
  let l, ep = accept_map Session.receive ep in
  Session.close ep;
  List.iter print_endline l

let inc_service ep =
  Session.close
    (accept_iter
       (fun ep -> let x, ep = Session.receive ep in
                 Session.send (x + 1) ep) ep)
            
let inc_client ep l =
  let l', ep =
    select_map (fun x ep -> Session.receive (Session.send x ep)) l ep
  in Session.close ep; l'
      
let service2 a =
  Session.close
    (accept_iter
       (fun ep -> let x, ep = Session.receive ep in
                 Session.send (x * x) ep) a)

let client2 b n =
  Session.close
    (select_iter
       (fun x ep ->
         let ep = Session.send x ep in
         let y, ep = Session.receive ep in
         print_endline ("sent " ^ string_of_int x ^ " and received " ^ string_of_int y);
         ep)
       (range 1 n) b)

let service3 a =
  let xs, a = accept_map Session.receive a in
  print_endline (string_of_list xs);
  Session.close a

let service4 a =
  let s, a =
    accept_fold
      (fun s ep -> let x, ep = Session.receive ep in (s + x), ep) 0 a
  in print_endline (string_of_int s);
     Session.close a

let client3 ep n =
  let s, ep = select_fold
                (fun x acc ep -> let ep = Session.send x ep in
                                 let y, ep = Session.receive ep in
                                 (acc + y), ep)
                0
                (range 1 n)
                ep
  in print_endline (string_of_int s);
     Session.close ep
                   
let print_list l = print_endline (string_of_list l)
                               
let _ =
  echo_client (Service.request (Service.spawn echo_service)) (List.map string_of_int (range 1 10));
  echo_client (Service.request (Service.spawn echo_service_2)) (List.map string_of_int (range 1 10));
  print_list (inc_client (Service.request (Service.spawn inc_service)) (range 1 10));
  client2 (Service.request (Service.spawn service2)) 10;
  echo_client (Service.request (Service.spawn service3)) (range 10 20);
  echo_client (Service.request (Service.spawn service4)) (range 20 30);
  client3 (Service.request (Service.spawn service2)) 10;
