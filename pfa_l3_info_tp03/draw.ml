(*
  Tidier Drawings of Trees, Reingold & Tilford 1981, IEEE Transactions on Soft. Eng.

  On traduit le code Pascal original…
*)

type 'a node =
  | NIL
  | NODE of {
      info : 'a;
      mutable left : 'a node;
      mutable right : 'a node;
      mutable xcoord : int;
      mutable ycoord : int;
      mutable offset : int;
      mutable thread : bool;
    }

type 'a extr = { mutable addr : 'a node; mutable off : int; mutable lev : int }

let extr () = { addr = NIL; off = 0; lev = -1 }

let rec node_of_tree t =
  let open Tree in
  match t with
  | Leaf -> NIL
  | Node (info, l, v, r) ->
      let left = node_of_tree l in
      let right = node_of_tree r in
      NODE
        {
          info = (info, v);
          left;
          right;
          xcoord = 0;
          ycoord = 0;
          offset = 0;
          thread = false;
        }

(*
    fonction SETUP, Fig. 6 de l'article. Les paramètres VAR sont
    remplacés par des références.
*)
let minsep = 15

let rec setup t level rmost lmost =
  let l = ref NIL in
  let r = ref NIL in
  let lr = ref (extr ()) in
  let ll = ref (extr ()) in
  let rl = ref (extr ()) in
  let rr = ref (extr ()) in
  let cursep = ref 0 in
  let rootsep = ref 0 in
  let loffsum = ref 0 in
  let roffsum = ref 0 in
  match t with
  | NIL ->
      !lmost.lev <- -1;
      !rmost.lev <- -1
  | NODE rt -> begin
      rt.ycoord <- level * minsep;
      l := rt.left;
      r := rt.right;
      setup !l (level + 1) lr ll;
      setup !r (level + 1) rr rl;
      match (!l, !r) with
      | NIL, NIL ->
          (*leaf*)
          !rmost.addr <- t;
          !lmost.addr <- t;
          !rmost.lev <- 0;
          !lmost.lev <- 0;
          rt.offset <- 0
      | _ -> begin
          cursep := minsep;
          rootsep := minsep;
          loffsum := 0;
          roffsum := 0;
          let rec loop () =
            match (!l, !r) with
            | NIL, _ | _, NIL -> ()
            | NODE lrec, NODE rrec ->
                if !cursep < minsep then begin
                  rootsep := !rootsep + (minsep - !cursep);
                  cursep := minsep
                end;
                if lrec.right <> NIL then begin
                  loffsum := !loffsum + lrec.offset;
                  cursep := !cursep - lrec.offset;
                  l := lrec.right
                end
                else begin
                  loffsum := !loffsum - lrec.offset;
                  cursep := !cursep + lrec.offset;
                  l := lrec.left
                end;
                if rrec.left <> NIL then begin
                  roffsum := !roffsum - rrec.offset;
                  cursep := !cursep - rrec.offset;
                  r := rrec.left
                end
                else begin
                  roffsum := !roffsum + rrec.offset;
                  cursep := !cursep + rrec.offset;
                  r := rrec.right
                end;
                loop ()
          in
          loop ();
          rt.offset <- (!rootsep + 1) / 2;
          loffsum := !loffsum - rt.offset;
          roffsum := !roffsum + rt.offset;
          if !rl.lev > !ll.lev || rt.left = NIL then begin
            lmost := !rl;
            !lmost.off <- !lmost.off + rt.offset
          end
          else begin
            lmost := !ll;
            !lmost.off <- !lmost.off - rt.offset
          end;
          if !lr.lev > !rr.lev || rt.right = NIL then begin
            rmost := !lr;
            !rmost.off <- !rmost.off - rt.offset
          end
          else begin
            rmost := !rr;
            !rmost.off <- !rmost.off + rt.offset
          end;
          let () =
            match (!l, !rr) with
            | NODE _, { addr = NODE rrec; off; _ } when !l <> rt.left -> begin
                rrec.thread <- true;
                rrec.offset <- abs (off + rt.offset - !loffsum);
                if !loffsum - rt.offset <= off then rrec.left <- !l
                else rrec.right <- !l
              end
            | _ -> ()
          in
          let () =
            match (!r, !ll) with
            | NODE _, { addr = NODE lrec; off; _ } when !r <> rt.right -> begin
                lrec.thread <- true;
                lrec.offset <- abs (off + rt.offset - !roffsum);
                if !roffsum + rt.offset >= off then lrec.right <- !r
                else lrec.left <- !r
              end
            | _ -> ()
          in
          ()
        end
    end

let rec petrify t xpos =
  match t with
  | NIL -> ()
  | NODE rt ->
      rt.xcoord <- xpos;
      if rt.thread then begin
        rt.left <- NIL;
        rt.right <- NIL;
        rt.thread <- false
      end;
      petrify rt.left (xpos - rt.offset);
      petrify rt.right (xpos + rt.offset)

open Gg
open Vg

let gray = I.const (Color.gray 0.90)
let black = I.const Color.black
let red = I.const Color.red
let fminsep = float minsep
let node_rad = fminsep /. 4.0
let border_width = node_rad /. 8.0

let mk_circle ?(color = gray) p =
  let circle = P.empty |> P.circle p node_rad in
  let disk = I.cut circle color in
  let border =
    let area = `O { P.o with P.width = border_width } in
    I.cut ~area circle black
  in
  I.blend border disk

let line ?(color = black) p1 p2 =
  let p = P.empty |> P.sub p1 |> P.line p2 in
  let area = `O { P.o with P.width = border_width } in
  I.cut ~area p color

let text ?(color = black) text p =
  let x = V2.x p in
  let y = V2.y p in
  let fh = node_rad in
  let fw = 0.6 *. fh in
  let y = y -. (fh *. 0.25) in
  let x = x -. (fw *. float_of_int (String.length text) *. 0.5) in
  I.move (V2.v x y)
    (I.cut_glyphs ~text
       Font.
         {
           name = "monospace";
           size = node_rad;
           weight = `W400;
           slant = `Normal;
         }
       [] color)

let draw pr_elem filename t =
  let n = node_of_tree t in
  let () = setup n 0 (ref (extr ())) (ref (extr ())) in
  let () = petrify n 0 in
  let min_x = ref infinity in
  let max_x = ref (-.infinity) in
  let min_y = ref infinity in
  let max_y = ref (-.infinity) in
  let rec loop n img parent =
    match n with
    | NIL -> img
    | NODE { left; right; xcoord; ycoord; info; _ } ->
        let x = float_of_int xcoord in
        let y = float_of_int (-ycoord) in
        let pxy = V2.v x y in
        let img =
          match parent with None -> img | Some p -> I.blend (line p pxy) img
        in
        let circle = mk_circle pxy in
        let txt = pr_elem (snd info) in
        let itxt = text txt pxy in
        let circle = I.blend itxt circle in
        min_x := min x !min_x;
        max_x := max x !max_x;
        min_y := min y !min_y;
        max_y := max y !max_y;
        let img = loop left img (Some pxy) in
        let img = loop right img (Some pxy) in
        I.blend circle img
  in
  let img = loop n (I.const Color.white) None in
  min_x := !min_x -. float minsep;
  max_x := !max_x +. float minsep;
  min_y := !min_y -. float minsep;
  max_y := !max_y +. float minsep;

  let sx = !max_x -. !min_x in
  let sy = !max_y -. !min_y in
  let size = Size2.v (3.0 *. sx) (3.0 *. sy) in


  let view = Box2.v (V2.v !min_x !min_y) (V2.v sx sy) in
  let oc = open_out filename in
  let r = Vgr.create (Vgr_svg.target ()) (`Channel oc) in
  try
    ignore (Vgr.render r (`Image (size, view, img)));
    ignore (Vgr.render r `End);
    close_out oc
  with e ->
    close_out oc;
    raise e


let start_html_log filename =
  let oc = open_out filename in
  output_string oc "<!DOCTYPE html>
  <html>
  <head><style> .img { text-align:center; }</style></head><body>\n";
  close_out oc


let stop_html_log filename =
  let oc = open_out filename in
  output_string oc "</body></html>\n";
  close_out oc