A Simple Library Implementation of Binary Sessions
==================================================

Overview
--------

FuSe is a lightweight OCaml module that implements the session-based
communication primitives in [GayVasconcelos10] and enables session
type checking and inference. It works with any out-of-the-box
installation of OCaml and supports the following features:

* delegation
* equi-recursive session types
* polymorphic session types
* context-free session types
* session types with labeled branches
* session type inference
* duality constraints on session type variables
* hybrid static/dynamic linearity checking
* session subtyping [GayHole05]
* higher-order resumption combinators
* shared channels for session initiation
* type pretty-printer (external utility)

Installation Instructions
-------------------------

You need [OCaml](http://www.ocaml.org) to compile FuSe.

1.  Compile the library and the examples:

    ```bash
    make
    ```

2.  Test an example:

    ```bash
    ./examples/Tmath
    ```

3.  Test session type inference:

    ```bash
    cd examples
    make Tmath.st
    ```

Documentation
-------------

Users may refer to the [API
documentation](https://github.com/boystrange/FuSe) extracted with
`ocamldoc` in the `docs` folder and to the papers (see below).

Example
-------

Below is an almost verbatim transcription in OCaml+FuSe of the
bookshop example found in [GayVasconcelos10].

```ocaml
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
```

Here are the (session) types inferred by OCaml, pretty printed in a
more readable form by the `rosetta` utility that accompanies the
library. Note the amount of parametric polymorphism compared to the
typing of the example as given in [GayVasconcelos10].

```ocaml
val shopLoop : rec X.&[ Add: ?α.X | CheckOut: ?β.?γ ] → α list → unit
val shop : rec X.&[ Add: ?α.X | CheckOut: ?β.?γ ] Service.t → unit
val isChildrensBook : α → bool
val voucher : α → β → rec X.⊕[ Add: !γ.X | CheckOut: !α.!β ] → γ → γ
val mother : α → β → &[ Add: ?γ.rec X.&[ Add: ?δ.X | CheckOut: ?α.?β ] ] Service.t →
             ?(δ → δ).!ε Service.t → γ → unit
val son : ?(α → β).!β Service.t → α → unit
```

Bibliography
------------

[GayHole05]: Simon J. Gay and Malcolm Hole: [Subtyping for session
types in the pi
calculus](http://dx.doi.org/10.1007/s00236-005-0177-z), *Acta
Informatica*, 2005.

[GayVasconcelos10]: Simon J. Gay, Vasco T. Vasconcelos, [Linear type
theory for asynchronous session
types](http://doi.org/10.1017/S0956796809990268), *Journal of
Functional Programming*, 2010.

[Padovani17A]: Luca Padovani: [A Simple Library Implementation of
Binary Sessions](http://dx.doi.org/10.1017/S0956796816000289),
*Journal of Functional Programming*, 2017.

[Padovani17B]: Luca Padovani: [Context-Free Session Type
Inference](http://hal.archives-ouvertes.fr/hal-01385258/document),
*Proceedings of ESOP'17*, 2017.

[ThiemannVasconcelos16]: Peter Thiemann and Vasco T. Vasconcelos:
[Context-Free Session Types](http://doi.org/10.1145/3022670.2951926),
*Proceedings of ICFP'16*, 2016.
