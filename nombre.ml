type frac = { num : int; denom : int }

let rec pgcd a b = if b == 0 then a else pgcd b (a mod b)
let neg_frac f = { num = -f.num; denom = f.denom }
let sign i = if i > 0 then 1 else if i == 0 then 0 else -1

let simp_frac f =
  let num, denom =
    if sign f.num * sign f.denom >= 0 then (abs f.num, abs f.denom)
    else (-abs f.num, abs f.denom)
  in
  let p = pgcd (abs num) denom in
  { num = num / p; denom = denom / p }

let frac a b = simp_frac { num = a; denom = b }

let add_frac f1 f2 =
  frac ((f1.num * f2.denom) + (f2.num * f1.denom)) (f1.denom * f2.denom)

let sub_frac f1 f2 = add_frac f1 (neg_frac f2)
let mul_frac f1 f2 = frac (f1.num * f2.num) (f1.denom * f2.denom)
let inv_frac f = frac f.denom f.num
let div_frac f1 f2 = mul_frac f1 (inv_frac f2)
let string_of_frac f = Printf.sprintf "%d/%d" f.num f.denom
let float_of_frac f = float f.num /. float f.denom


(** conversion d'un flottant en fraction proche.
On utilise l'algorithme consistant à développer
le flottant en fraction continue:
- on décompose le flottant en partie entière i
  et partie fractionnaire r
- si r est plus petit qu'une certaine précision, on s'arrête
  et on renvoie la fractio i/1
- sinon, on ajoute
  i/1 et l'inverse de la fraction obtenue récursivement
  pour 1.0/. r
*)
let rec fof_aux f =
  let r, i = modf f in
  let fi = (frac (int_of_float i) 1) in
  if r  < 0.001 then fi
  else
    add_frac fi
    (inv_frac (fof_aux (1.0 /.r)))

let frac_of_float f =
  let s = if f < 0.0 then -1 else 1 in
  let f = abs_float f in
  mul_frac (frac s 1) (fof_aux f)

    

let test_frac () =
  Printf.printf "%s \n" (string_of_frac (frac 2 10));
  Printf.printf "%s \n" (string_of_frac (frac 5 10));
  Printf.printf "%s \n" (string_of_frac (frac 100 20));
  Printf.printf "%s \n" (string_of_frac (add_frac (frac 2 10) (frac 4 10)));
  Printf.printf "%s \n" (string_of_frac (frac (-2) 10));
  Printf.printf "%s \n" (string_of_frac (frac (-5) (-10)));
  Printf.printf "%s \n" (string_of_frac (frac 100 (-20)));
  Printf.printf "%s \n"
    (string_of_frac (mul_frac (frac (-2) 10) (frac (-4) 10)))

(* let () = test_frac () *)

type num = Float of float | Int of int | Frac of frac

let string_of_num n =
  match n with
  | Int i -> string_of_int i
  | Float fl -> string_of_float fl
  | Frac fr -> string_of_frac fr

let all_of_num n =
  match n with
  | Int i -> (i, float i, frac i 1)
  | Float f -> (int_of_float f, f, frac_of_float f)
  | Frac fr ->
      let f = float_of_frac fr in
      (int_of_float f, f, fr)

let exec_op n1 n2 op_i op_fr op_fl =
  match (n1, n2) with
  | Float fl1, Float fl2 -> Float (op_fl fl1 fl2)
  | Float fl1, Frac fr2 -> Float (op_fl fl1 (float_of_frac fr2))
  | Float fl1, Int i2 -> Float (op_fl fl1 (float i2))
  | Frac fr1, Float fl2 -> Float (op_fl (float_of_frac fr1) fl2)
  | Frac fr1, Frac fr2 -> Frac (op_fr fr1 fr2)
  | Frac fr1, Int i2 -> Frac (op_fr fr1 (frac i2 1))
  | Int i1, Float fl2 -> Float (op_fl (float i1) fl2)
  | Int i1, Frac fr2 -> Frac (op_fr (frac i1 1) fr2)
  | Int i1, Int i2 -> Frac (op_fr (frac i1 1) (frac i2 1))

let add_num n1 n2 = exec_op n1 n2 ( + ) add_frac ( +. )
let sub_num n1 n2 = exec_op n1 n2 ( - ) sub_frac ( -. )
let neg_num n = sub_num (Int 0) n
let mul_num n1 n2 = exec_op n1 n2 ( * ) mul_frac ( *. )
let div_num n1 n2 = exec_op n1 n2 ( / ) div_frac ( /. )
let inv_num n = div_num (Int 1) n

(* let () = test () *)

let num_of_string s =
  match String.index_opt s '.' with
  | Some _ -> Float (float_of_string s)
  | None -> begin
      match String.index_opt s '/' with
      | Some i ->
          let sn = String.sub s 0 i in
          let sd = String.sub s (i + 1) (String.length s - i - 1) in
          Frac (frac (int_of_string sn) (int_of_string sd))
      | None -> Int (int_of_string s)
      end

let num_of_string_opt s =
  try Some (num_of_string s) with _ -> None    

let op_of_string_opt s =
  match s with
  "+" -> Some add_num
  | "-" -> Some sub_num
  | "*" -> Some mul_num
  | "/" -> Some div_num
  | _ -> None


let rec read_until f =
  let s = read_line () in
  match f s with None -> read_until f
  | Some v -> v

let rec calculatrice () =
  let n1 = read_until num_of_string_opt in
  let o = read_until op_of_string_opt in
  let n2 = read_until num_of_string_opt in
  let r = o n1 n2 in
  let i,f, fr = all_of_num r in
  let () = Printf.printf "%d, %f, %s\n" i f (string_of_frac fr) in
  calculatrice ()
let () = Sys.catch_break true
let () = calculatrice ()