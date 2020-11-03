
open Session.Bare
   
let rec even u v =
  let x, u = receive u in
  let v = send x v in odd u v
and odd u v =
  let _, u = receive u in
  even u v

let rec link u v =
  let x, u = receive u in
  let v = send x v in
  link u v

let tail u v =
  let _, u = receive u in
  link u v

let rec inv u v =
  let x, u = receive u in
  print_endline (string_of_int x);
  let v = send (1 - x) v in
  inv u v

let rec copy u v w =
  let x, u = receive u in
  let v = send x v in
  let w = send x w in
  copy u v w
  
let rec zip u v w =
  let x, u = receive u in
  let w = send x w in
  zip v u w

let source x u v =
  let v = send x v in
  link u v
  
let _ =
  let a, a' = create () in
  let b, b' = create () in
  let c, c' = create () in
  let d, d' = create () in
  let e, e' = create () in
  let f, f' = create () in
  let g, g' = create () in
  let _ = Thread.create (source 0 g) a' in
  let _ = Thread.create (copy a b') c' in
  let _ = Thread.create (even b) d' in
  let _ = Thread.create (inv d) e' in
  let _ = Thread.create (tail c) f' in
  let _ = Thread.create (zip e f) g' in
  Thread.delay 10.0
