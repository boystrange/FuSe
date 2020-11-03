open Session.Bare

let rec server x =
  match branch x with                  (* wait for a request       *)
  | `False x -> close x                     (* close session            *)
  | `True x  -> let n, x = receive x in     (* receive first operand    *)
	       let m, x = receive x in     (* receive second operand   *)
	       let x = send (n + m) x in   (* send result              *)
	       server x                    (* serve more requests      *)

let client =
  let rec aux acc y n =                (* add n naturals           *)
    if n = 0 then begin
      close (select_false y); acc      (* close session and return *)
    end else
      let y = select_true y in         (* select plus operation    *)
      let y = send acc y in            (* send first operand       *)
      let y = send n y in              (* send second operand      *)
      let res, y = receive y in        (* receive result           *)
      aux res y (n - 1)                (* possibly add more        *)
  in aux 0
		      
let _ =
  let a, b = create () in                (* create the session       *)
  let _ = Thread.create server a in      (* spawn the server         *)
  print_int (client b 100)               (* run the client           *)
