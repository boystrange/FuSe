open Session.Monadic

let server =
  fix
    (fun server ->
      branch
	(receive >>= fun n -> receive
	         >>= fun m -> send (n + m)
		 >>> server)
	(return ()))

let client =
  let rec aux acc n =
    if n = 0 then
      select_false >>> return acc
    else
      select_true >>> send acc
                  >>> send n
                  >>> receive
                  >>= fun res -> aux res (n - 1)
  in aux 0

let _ =
  print_int (connect server (client 100))
