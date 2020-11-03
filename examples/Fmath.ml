include Session

let rec server s =
  match branch s with                  (* receive client's request *)
    `L s -> close s                    (* close session            *)
  | `R s -> let n, s = receive s in    (* receive first operand    *)
	    let m, s = receive s in    (* receive second operand   *)
	    let s = send s (n + m) in  (* send result              *)
	    server s                   (* serve other requests     *)

let client n s =
  let rec aux acc n s =                (* add n naturals           *)
    if n = 0 then begin
      close (left s);                  (* close session            *)
      acc                              (* return result            *)
    end else
      let s = right s in               (* select plus operation    *)
      let s = send s acc in            (* send first operand       *)
      let s = send s n in              (* send second operand      *)
      let res, s = receive s in        (* receive result           *)
      aux res (n - 1) s                (* possibly add more        *)
  in aux 0.0 n s
		      
let _ =
  let a, b = create () in
  let _ = Thread.create server a in
  print_int (client 100 b)
