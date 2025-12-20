let id = \x.x
let true = \x.\y.x
let false = \x.\y.y
let not = simp \a. a false true

let 0 = \f. id
let 1 = \f.\x. f x
let 2 = \f.\x. f (f x)
let 3 = \f.\x. f (f (f x))
let 4 = \f.\x. f (f (f (f x)))
let 5 = \f.\x. f (f (f (f (f x))))
let 6 = \f.\x. f (f (f (f (f (f x)))))
let 7 = \f.\x. f (f (f (f (f (f (f x))))))
let 8 = \f.\x. f (f (f (f (f (f (f (f x)))))))
let 9 = simp \f.\x.f (8 f x)
let succ = \n. \f.\x. f (n f x)
let compose = \f.\g. \x. f (g x)
let add = simp \n.\m. \f. compose (n f) (m f)
let mul = simp \n.\m. \f. n (compose (m f))
let mul_alter = simp \n.\m. \f. n (compose (\g.\x. g (m f x)))
let pow = \n.\m. m n

let Omega = (\x.x x) (\x.x x)
let Omega_3 = (\x.x x x) (\x.x x x)