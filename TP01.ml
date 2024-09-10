(* TP 01*)
(* 1.1 Fractions *)

(* 1. PGCD*)
let rec pgcd a b = 
  if (a mod b) = 0 then 
    b
  else
    pgcd b (a mod b)

(* 2. Type enregistrement *)
type frac = {num:int; denom:int}

(* 3. sign *)
let sign i = 
  if i = 0 then 
    0
  else if i < 0 then 
  -1
  else 
  1

(* 4. simpf *)
let simpf f = 
  let num, denom = 
  if sign f.num * sign f.denom >=0 then (abs f.num , abs f.denom)
  else (-(abs f.num), abs f.denom) in 
  let p = pgcd num denom in 
  {num = num/p; denom = denom/p}

(* 5. frac*)
let frac a b =
  simpf {num = a; denom = b}

let string_of_frac f = 
  Printf.sprintf "%d/%d" f.num f.denom 

(* 6. Test *)
(* 7. Fonctions*)
let add_frac f1 f2 = frac ((f1.num * f2.denom ) + (f2.num * f1.denom)) (f1.denom * f2.denom)
let neg_frac f= {num = -f.num; denom = f.denom}
let sub_frac f1 f2 = add_frac f1 (neg_frac f2)
let mul_frac f1 f2 = simpf {num = f1.num*f2.num; denom = f2.denom*f1.denom}
let inv_frac f = {num = f.denom; denom = f.num}
let div_frac f1 f2 = mul_frac f1 (inv_frac f2)
let float_of_frac f = float f.num /. float f.denom

(* Question 08 *)
let rec fof_aux f =
   let r, i = modf f in
   let fi = (frac ( int_of_float i) 1) in
   if r < 0.001 then fi
   else
   add_frac fi
   (inv_frac (fof_aux (1.0 /.r)))
  
   let frac_of_float f =
   let s = if f < 0.0 then -1 else 1 in
   mul_frac (frac s 1) (fof_aux ( abs_float f))

(* 1.2 Nombres*)
type num = |Int of int |Float of float| Frac of frac

(* 1. String of type*)
let string_of_num n = 
  match n with 
  |Int i -> string_of_int i
  |Float fl -> string_of_float fl
  |Frac fr-> string_of_frac fr

  


