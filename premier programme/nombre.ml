(* à compléter :
   le fichier peut être compilé avec ocamlc -o nombre.exe nombre.ml
   *)
let rec pgcd a b = 
   if b = 0 then 
      a
   else
      let res = a mod b in 
      pgcd b res

let () =
   Printf.printf "PGCD DE 10 et 2 est %d" (pgcd 2 (-10));

type frac = {num:int; denom:int}

let sign i = 
   if i>0 then 
      1
   else if i < 0 then 
      -1
   else
      0

let simp f = 
   let num, denom = 
      if sign f.num * sign f.denom >= 0 then 
         (abs f.num, abs f.denom)
      else 
         (-abs f.num ,abs f.denom) 
      in
      let div = pgcd num denom in
      {num = num/div; denom = denom/div}

let x = simp {num = 5; denom = 0}

let () = 
   Printf.printf "simp de 10 5 est %d / %d" x.num x.denom

let frac a b = 
   simp {num=a; denom = b}

let string_of_frac f = 
   Printf.sprintf "%d / %d" f.num f.denom

let pr_frac a b =
   Printf.printf "%d / %d : %s\n" a b ( string_of_frac (frac a b))

let test1() = 
   pr_frac (-1) 2;
   pr_frac 2 4;
   pr_frac 10 25


let test=
   test1()


(*Question 07*)

let add_frac f1 f2 = 
   frac ((f1.num * f2.denom )+ (f2.num * f1.denom)) (f1.denom * f2.denom)

let neg_frac f = 
      let s = simp f  in
      {num = (-1)*s.num; denom =s.denom}

let sub_frac f1 f2 = 
   frac ((f1.num * f2.denom )- (f2.num * f1.denom)) (f1.denom * f2.denom)
   (*add_frac f1 (neg_frac f2)*)

let mul_frac f1 f2 = 
   frac (f1.num * f2.num) (f1.denom*f2.denom)

let inv_frac f = 
   {num = f.denom; denom = f.num}
   (*frac f.denom f.num*)

let div_frac f1 f2 = 
   frac (f1.num * f2.denom) (f1.denom * f2.num)
   (*mul_frac (f1.num * f2.denom) (inv_frac f2)*)

type frac_float = {numf:float; denomf:float}

let float_of_frac f =
   let f = simp f in 
    {numf = float_of_int f.num; denomf= float_of_int f.denom}

let () = 
   let fra1 = {num = 1;denom = 2} in
   let fra2 = {num = 2;denom = 1} in
   let inv = float_of_frac fra1 in
   Printf.printf "%f / %f\n" inv.numf inv.denomf

(*exercice 08*)
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


(*1.2 nombres*)
type num = Int of int | Float of float | Frac of frac

let string_of_num n =
   match n with 
   |Int i -> string_of_int i
   |Float fl -> string_of_float fl
   |Frac fr -> string_of_frac fr



let () = 
   let test1 = Int 5 in
   Printf.printf "%s"  (string_of_num test1)

let float_of_fr f = 
   float_of_int f.num /. float_of_int f.denom

   
let exec_op n1 n2 op_i op_fr op_fl =
      match n1 , n2 with
      | Float fl1 , Float fl2 -> Float (op_fl fl1 fl2) 
      | Float fl1 , Frac fr2 -> Float ( op_fl fl1 (float_of_fr fr2))
      | Float fl1 , Int i2 -> Float (op_fl fl1 (float i2))
      | Frac fr1 , Float fl2 -> Float (op_fl (float_of_fr fr1) fl2)
      | Frac fr1 , Int i2 -> Frac (op_fr fr1 (frac i2 1))
      | Frac fr1, Frac fr2 -> Frac (op_fr fr1 fr2)
      | Int i1, Int i2 -> Int (op_i i1 i2)
      | Int i1 , Float fl2 -> Float (op_fl (float i1) fl2 )
      | Int i1 , Frac fr2 -> Frac (op_fr (frac i1 1) fr2)

let add_num n1 n2 = exec_op n1 n2 (+) add_frac (+.)
let sub_num n1 n2 = exec_op n1 n2 (-) sub_frac (-.)
let mul_num n1 n2 = exec_op n1 n2 ( * ) mul_frac ( *. )
let div_num n1 n2 = exec_op n1 n2 ( / ) div_frac ( /. )


let test_operations()=   
   Printf.printf "%s\n" (string_of_num (add_num (Int 2) (Float 3.2)));
   Printf.printf "%s\n" (string_of_num (sub_num (Int 2) (Int 1)));
   Printf.printf "%s\n" (string_of_num (mul_num(Int 2) (Int 2)));
   Printf.printf "%s\n" (string_of_num (div_num(Int 2) (Int 2)))

let () = 
   test_operations()

(* 1.3 Calculatrice*)

let all_of_num n = 
   match n with 
   |Float fl -> (int_of_float fl, frac_of_float fl, fl) (* c'est un tuple *)
   |Int i -> i , {num = i; denom = 1}, float_of_int i
   |Frac fr -> int_of_float(float_of_fr fr), fr , float_of_fr fr

(* Un teste de all_of_num*)
let test_all_of_num() = 
   let (int_part, frac_part, float_part) = all_of_num (Int 5) in 
   Printf.printf "Float: %f; Frac: %s; Int: %d \n"  float_part (string_of_frac frac_part) int_part 

let () = test_all_of_num()

(*Convertire une chaine de caractere en num *)


(*La fonction : *)
let num_of_string s =
   match String.index_opt s '.' with
   |Some _ -> Float (float_of_string s)
   |None -> begin
      match String.index_opt s '/' with
      |Some i -> 
         let fh = String.sub s 0 i in
         let sh = String.sub s (i+1) ((String.length s)-i+1) in
         Frac (frac (int_of_string fh) (int_of_string sh))
      |None -> Int (int_of_string s)
      end


      







      









