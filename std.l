let I = \x.x
let K = \x.\y.x
let S = \x.\y.\z. x z (y z)
let iota = \x. x S K
let U = \x.x x
let default Omega = (\x.x x) (\x.x x)
let default Omega_3 = (\x.x x x) (\x.x x x)

let default true = \x.\y.x
let default false = \x.\y.y

let default and = \p.\q. p q p
let default or = \p.\q. p p q
let default not = \a. a false true
let default not2 = \p.\a.\b. p b a
let default xor = \a.\b. a (not b) b
let if = \p.\a.\b. p a b

let default pair = \x.\y.\z. z x y
let default first = \p.p true
let default second = \p.p false

let 0 = \f. I
let default 1 = \f.\x. f x
let default 2 = \f.\x. f (f x)
let default 3 = \f.\x. f (f (f x))
let default 4 = \f.\x. f (f (f (f x)))
let default 5 = \f.\x. f (f (f (f (f x))))
let default 6 = \f.\x. f (f (f (f (f (f x)))))
let default 7 = \f.\x. f (f (f (f (f (f (f x))))))
let default 8 = \f.\x. f (f (f (f (f (f (f (f x)))))))
let default 9 = simp \f.\x.f (8 f x)

let default succ = \n. \f.\x. f (n f x)
let default compose = \f.\g. \x. f (g x)
let default add = \n.\m. \f. compose (n f) (m f)
let add2 = \n.\m. n succ m
let default mul = \n.\m. \f.\x. m (n f) x
let default pow = \n.\m. m n
let default pred = \n. \f.\x. n (\r.\i. i (r f)) (\f.x) I
let default half = \n. \f.\x. n (\r. \a.\b. a (r b a)) (\a.\b. x) I f
let default fact = \n. \f.\x. n (\r. \a. a (r (succ a))) (\a. f) 1 x
let default sub = \n.\m. m pred n
let default tet = \n. n (\r. \a. r a a) 0

let default isZero = \n. n (\x. false) true
let default le = \n.\m. isZero (sub n m)
let default eq = \n.\m. and (isZero n m) (isZero m n)