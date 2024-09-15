open Tree (* on « importe » les définitions de tree.ml *)

let dump filename t = Draw.draw (fun i -> Printf.sprintf "%d" i) filename t

let test1 () =
  let t1 = add 1 (add 2 (add 3 empty)) (* = Leaf *) in
  let () = dump "img/test_1_1.svg" t1 in
  let t2 = add 42 (add 43 (add 44 t1)) in
  dump "img/test_1_2.svg" t2


let () = test1 ()