module Session = Session.Bare

let rec shopLoop s order =
  match Session.branch s with
  | `Add s -> let book, s = Session.receive s in
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
  Session.close c;
  book

let mother card address shopAccess sonAccess book =
  let c = Service.request shopAccess in
  let c = Session.select (fun x -> `Add x) c in
  let c = Session.send book c in
  let s = Service.request sonAccess in
  let s = Session.send (voucher card address c) s in
  let sonBook, s = Session.receive s in
  Session.close s

let son sonAccess book =
  let s = Service.accept sonAccess in
  let f, s = Session.receive s in
  let s = Session.send (f book) s in
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
