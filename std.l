let default id = \x.x
let default true = \x.\y.x
let default false = \x.\y.y
let default not = simp \a. a false true

let 0 = \f. id
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
let default add = simp \n.\m. \f. compose (n f) (m f)
let default mul = simp \n.\m. \f. n (compose (m f))
let default mul_alter = simp \n.\m. \f. n (compose (\g.\x. g (m f x)))
let default pow = \n.\m. m n

let default Omega = (\x.x x) (\x.x x)
let default Omega_3 = (\x.x x x) (\x.x x x)