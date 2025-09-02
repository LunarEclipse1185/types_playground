(** string to list. tail_mod_cons under the rug *)
(* let explode s = List.init (String.length s) (String.get s) *)

(** Monadic let for Result *)
let (let*) = Result.bind

(** Error reporting (not yet) parser of Lambda-> language and the Applicative structure *)
type 'a result_t = ('a * string, Core.errmsg) result
type 'a parser = Parser of (string -> 'a result_t)
let syntax_error_msg: Core.errmsg = {msg = "Syntax error"; start = 0; stop = 0}
let syntax_error: 'a result_t = Error syntax_error_msg

let run (Parser p) src = p src

(** Functor structure *)

(** For penetrating on the result of a parser. Note: Technically [(fun) <$>] can be replaced with [pure (fun) <*>] *)
let (<$>) f p = Parser (fun input ->
  let* (x, input) = run p input in
  Ok (f x, input))
let fmap = (<$>)

(** Applicative structure *)

(** For discarding and replacing the result of a success parser. *)
let pure x: 'a parser = Parser (fun input ->
  Ok (x, input))

(** For currying a multi argument function though the Applicative *)
let (<*>) p1 p2 = Parser (fun input ->
  let* (f, input) = run p1 input in
  let* (x, input) = run p2 input in
  Ok (f x, input))
let apply = (<*>)
(* the following two implementations cannot be eta reducted to
let (<* ) = (<*>) <.> (<$>) (fun x _ -> x)
because of the Value Restriction. see my note in Obsidian. *)
let (<* ) p1 p2 = (fun x _ -> x) <$> p1 <*> p2
(* let (<* ) = Fun.compose (<*>) $ (<$>) (fun x _ -> x) *)
let ( *>) p1 p2 = (fun _ x -> x) <$> p1 <*> p2

(** 
Also known as [Applicative.all] in Jane Street Base.

Proves (constructs a way to) commute the inner state of Parser, [input], and the Traversable structure of List, namely the forward direction.
*)

(* unused *)
let sequenceA (ps: 'a parser list): 'a list parser =
  let rec iter ps acc =
    match ps with
    | [] -> List.rev <$> acc
    | p :: ps ->
      let acc = (fun xs x -> x :: xs) <$> acc <*> p in
      iter ps acc
  in iter ps @@ pure []

(* We wished to achieve with sequenceA:
get by [sequenceA . map charP "asd"] without actual computation, no Error possiblel
run by [runParser p "asdf"] resulting in a Seq, still no computation, as usual Error is possible;
finalize by [List.from_seq] doing actual computation, but since eval deferred Error can only arise at this stage.
Concisely, we cannot hope to obtain a Result value after running runParser, while producing a lazy structure in Ok branch.
Until the value of Result itself is lazy eval'ed.
Which it is not in OCaml. *)

(* failed attempt to use tail_mod_cons. this only applies to calles directly wrapped in a data constructor in the tail position, to be transformed into DPS. Composition with Monad(option) / Applicative(inner state) create brances. well actually Applicative does not cause branches but using tuple destruction, or equivalently(?) fst, snd is not data constructor. *)
(* let sequenceA (ps: 'a parser list): 'a list parser = Parser (fun input ->
  let[@tail_mod_cons] rec iter ps input =
    match ps with
    | [] -> Ok ([], input) (* semantically, constructing from tail *)
    | p :: ps -> (* (fun x xs -> x :: xs) <$> p <*> sequenceA ps *)
      match runParser p input with
      | Error _ as e -> e
      | Ok (x, input) -> 
        match iter ps input with
        | Error _ as e -> e
        | Ok (xs, input) -> Ok (x :: xs, input)
  in iter ps input) *)

(* another failure by OCaml: constructors are not first class functions, hence cannot be <$>'ed and passed in arguments *)


(** Alternative structure *)

let empty = Parser (Fun.const syntax_error)

let map_on_error f = function
| Ok _ as r -> r
| Error e -> f e

(** Accounted for eager-eval *)
let (<|>) p1 p2 = Parser (fun input ->
  run p1 input |> map_on_error @@ Fun.const @@
  run p2 input |> map_on_error @@ Fun.const syntax_error)


(** Combinators *)

(* Decorators *)
let not p = Parser (fun input ->
  run p input |> function
  | Ok _ -> syntax_error
  | Error _ -> Ok ((), input))

let only_if f p = Parser (fun input ->
  let* (x, input) = run p input in
  if f x then Ok (x, input) else syntax_error)

let optional p = Parser (fun input ->
  run p input |> function
  | Ok (x, input) -> Ok (Some x, input)
  | Error _ -> Ok (None, input))

(* The Fixed-Point combinator, Y combinator, used to construct recursive expressions that does not loop when evaluating. Essentially a CPS structure. *)
(* let rec fix f =
  f (fix f)
let fixs fs =
  fix (fun self fs -> List.map (fun f x -> f (self fs) x) fs) fs *)

(* Alternative for the fixed-point combinator that is somewhat more flexible and less verbose to use *)
let defer f =
  let p = lazy (f ()) in
    Parser (fun input -> run (Lazy.force p) input)

let rec many p = some p <|> pure []
and some p = let some p () = List.cons <$> p <*> many p in
  defer @@ some p

(* possibly empty, separated list *)
let sepBy sep p = List.cons <$> p <*> many (sep *> p) <|> pure []


(* Important: tuples cannot be recursively typed!!
Record and Variant types are BOXED, while tuples are merely type aliases *)
(* 
(* this method requires a separate type definition and pollutes the namespace *)
type 'a dt = {
  even: 'a dt -> 'a;
  odd: 'a dt -> 'a;
}
let (even, odd) =
  let even dt n = n = 0 || dt.odd dt (n-1) in
  let odd dt n = n <> 0 && dt.even dt (n-1) in
  let dt = {even; odd} in
  (even dt, odd dt) *)
(* let (even, odd) =
  let obj = object (self)
      method even n = (n = 0) || self#odd (n - 1)
      method odd n = (n <> 0) && self#even (n - 1)
  end in (obj#even, obj#odd) *)


(* let [ even; odd ] =
  let open_even [ even; odd ] n = n = 0 || odd (n - 1)
  and open_odd [ even; odd ] n = n <> 0 && even (n - 1) in
  fixs [ open_even; open_odd ] *)


(** Concrete parsers *)

let part s n = try String.(sub s 0 n, sub s n (length s - n)) with
  Invalid_argument _ -> ("", "")

let charP c: char parser = Parser (fun input ->
  let x, xs = part input 1 in
  if x = String.make 1 c then Ok (c, xs) else syntax_error)

(** directly implemented string parser due to string being primitive *)
let stringP s: string parser = Parser (fun input ->
  if String.starts_with input ~prefix:s
    then Ok (part input @@ String.length s)
    else syntax_error)

(** Non-empty character span. maybe can be implemented as [some charPred f]?*)
let spanP f: string parser = Parser (fun input ->
  let exception PredFail of int in
  match try
    String.fold_left (fun n c -> if f c then n+1 else raise (PredFail n)) 0 input
  with PredFail n -> n with
  | 0 -> syntax_error
  | n -> Ok (part input n))
  
(** exactly match a word, where the predicate [f] indicates chars accepted in a word *)
let tokenP f s: string parser = spanP f |> only_if ((=) s)

let optionalSpanP f: string parser = Parser (fun input ->
  let exception PredFail of int in
  let len = try
    String.fold_left (fun n c -> if f c then n+1 else raise (PredFail n)) 0 input
  with PredFail n -> n in
  Ok (part input len))

let is_space = function
| ' ' | '\012' | '\n' | '\r' | '\t' -> true
| _ -> false
let ws = optionalSpanP is_space
