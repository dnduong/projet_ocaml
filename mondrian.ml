open Graphics

type label = { coord : int; colored : bool; }
type bsp = R of color option | L of label * bsp * bsp
						    
let width = 500
let height = 500

let random_bsp width height max_depth =
  let padding = 20 in
  let rand a b =
    Random.int (b - a) + a in
  let rec aux amin amax bmin bmax depth =
    if depth = 0 || amax - amin <= 2 * padding then
      R (Some (if Random.bool () then blue else red))
    else
      let coord = rand (amin + padding) (amax - padding) in
      let left = aux bmin bmax amin coord (rand (depth / 2) depth) in
      let right = aux bmin bmax coord amax (rand (depth / 2) depth) in
      L ({coord = coord; colored = Random.bool ()}, left, right) in
  aux 0 width 0 height max_depth

let rectangles_from_bsp bsp =
  let rec loop bsp x0 y0 x1 y1 p =
    match bsp with
    | R c -> [((x0, y0, x1 - x0, y1 - y0), c)]
    | L (l, g, d) ->
       if p then
	 loop g x0 y0 l.coord y1 false @ loop d l.coord y0 x1 y1 false
       else
	 loop g x0 y0 x1 l.coord true @ loop d x0 l.coord x1 y1 true in
  loop bsp 0 0 width height true

let adjacent_rects (x0, y0, x1, y1) rlist =
  let rec is_adjacent ((x, y, w, h), _) =
    (x = x0 || x + w = x0) && y >= y0 && y + h <= y1
    || (y = y0 || y + h = y0) && x >= x0 && x + w <= x1 in
  List.filter is_adjacent rlist
	      
let line_color line rlist =
  let adj = adjacent_rects line rlist in
  let b = List.length (List.filter (fun (_, c) -> c = Some blue) adj) in
  let r = List.length (List.filter (fun (_, c) -> c = Some red) adj) in
  if b > r then blue
  else if b < r  then red
  else magenta

let lines_from_bsp bsp rlist =
  let rec aux bsp amin amax bmin bmax vertical =
    match bsp with
    | R c -> []
    | L (l, g, d) ->
       let line =
	 if vertical then (l.coord, bmin, l.coord, bmax)
	 else (bmin, l.coord, bmax, l.coord) in
       let color =
	 if l.colored then line_color line rlist else black in
       (line, color) :: aux g bmin bmax amin l.coord (not vertical)
       @ aux d bmin bmax l.coord amax (not vertical) in
  aux bsp 0 width 0 height true

let rec draw_current_bsp rlist llist =
  let draw_line ((x0, y0, x1, y1), c) =
    if c = blue then set_color 0x0055FF
    else if c = red then set_color 0xFF5500
    else set_color c;
    moveto x0 y0;
    lineto x1 y1 in
  let draw_rectangle ((x, y, w, h), c) =
    (match c with
     | None -> set_color white
     | Some col -> set_color col);
    fill_rect x y w h in
  clear_graph ();
  set_line_width 4;
  List.iter draw_rectangle rlist;
  List.iter draw_line llist;
  set_color black;
  draw_rect 0 0 width height;
  synchronize ()

let change_color bsp x y =
  let rec aux bsp p =
    match bsp with
    | L (l, g, d) ->
       let coord = if p then x else y in
       if coord < l.coord then L (l, aux g (not p), d) else L (l, g, aux d (not p))
    | R c ->
       let col =
	 if c = None then Some blue
	 else if c = Some blue then Some red
	 else None in
       R col in
  if x < 0 || x > width || y < 0 || y > height then bsp else aux bsp true

let rec rlist_to_fnc rlist =
  let f (rect, c) =
    match c with
    | None -> [(true, rect); (false, rect)]
    | Some col -> [(col = blue, rect)] in
  List.map f rlist
	   
let parmi k rlist bool =
  let rec aux k rlist acc =
    match rlist with
    | [] -> if k = 0 then [acc] else []
    | (rect, c) :: tl ->
       if k = 0 then [acc] else
	 aux (k - 1) tl ((bool, rect) :: acc) @ aux k tl acc in
  aux k rlist []
      
let rec llist_to_fnc llist rlist =
  match llist with
  | [] -> []
  | (l, c) :: tl ->
     let adj = adjacent_rects l rlist in
     let n = List.length adj in
     if c = magenta then
       parmi (n / 2 + 1) adj true @ parmi (n / 2 + 1) adj false @ llist_to_fnc tl rlist
     else if c = black then llist_to_fnc tl rlist
     else
       parmi (n / 2 + n mod 2) adj (c = blue) @ llist_to_fnc tl rlist

let dvv_to_rlist dvv =
  List.map (fun (b, rect) -> (rect, Some (if b then blue else red))) dvv
	   
module Variables =
  struct
    type t = int * int * int * int
    let compare = compare
  end
    
module Sat = Sat_solver.Make(Variables)
			    
let complete llist rlist_end rlist =
  match Sat.solve (llist_to_fnc llist rlist_end @ rlist_to_fnc rlist) with
  | None -> None
  | Some dvv -> Some (dvv_to_rlist dvv)
		     
let check_current llist rlist_end rlist =
  List.for_all (fun (_, c) -> c <> None) rlist &&
    complete llist rlist_end rlist <> None

let rec game () =
  let bsp_end = random_bsp width height 6 in
  let rec cpy = function
    | R c -> R None
    | L (l, g, d) -> L (l, cpy g, cpy d) in
  let bsp = cpy bsp_end in
  let rlist_end = rectangles_from_bsp bsp_end in
  let llist = lines_from_bsp bsp_end rlist_end in
  let rec loop bsp =
    let rlist = rectangles_from_bsp bsp in
    draw_current_bsp rlist llist;
    moveto 550 400;
    draw_string "Appuyez sur e pour chercher une extension";
    synchronize ();
    if not (check_current llist rlist_end rlist) then   
      let e = wait_next_event [Button_down; Key_pressed] in
      if e.button then
	loop (change_color bsp e.mouse_x e.mouse_y)
      else if e.key = 'e' then
	match complete llist rlist_end rlist with
	| None ->
	   moveto 550 250;
	   draw_string "Il n'existe pas d'extension.";
	   synchronize ();
	   ignore (wait_next_event [Button_down; Key_pressed]);
	   loop bsp
	| Some l ->
	   moveto 550 250;
	   draw_string "Une extension existe, l'afficher ? (o/n)";
	   synchronize ();
	   let rec while' () =
	     match read_key () with
	     | 'o' -> draw_current_bsp l llist
	     | 'n' -> loop bsp
	     | _ -> while' () in
	   while' ()
      else loop bsp in
  loop bsp;
  moveto 550 250;
  draw_string "Partie terminee, rejouer ? (o/n)";
  synchronize ();
  let rec while' () =
    let key = read_key () in
    if key = 'o' then game ()
    else if key <> 'n' then while' () in
  while' ()
    
let main () =
  open_graph " 800x540";
  set_window_title "Mondrian";
  auto_synchronize false;
  Random.self_init ();
  try game () with
  | Graphic_failure "fatal I/O error" -> ()
       
let () = main ()
