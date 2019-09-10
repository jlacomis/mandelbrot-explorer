open Graphics

(* Coordinate in the bottom-left *)
let draw_string_with_box string x y =
  let orig_x, orig_y = current_point () in
  moveto (x + 5) y;
  let width, height = text_size string in
  set_color black;
  fill_rect x y (width + 10) height;
  set_color white;
  draw_string string;
  moveto orig_x orig_y;
  synchronize ()

(* Coordinate in the bottom-right *)
let draw_string_with_box_br string x y =
  let width, _ = text_size string in
  let x = x - width - 10 in
  draw_string_with_box string x y

(* Coordinate in the top-right *)
let draw_string_with_box_tr string x y =
  let width, height = text_size string in
  let x = x - width - 10 in
  let y = y - height in
  draw_string_with_box string x y

(* Coordinate in the top-left *)
let draw_string_with_box_tl string x y =
  let _, height = text_size string in
  let y = y - height in
  draw_string_with_box string x y

(* Coordinate in the center *)
let draw_string_with_box_c string x y =
  let width, height = text_size string in
  let x = x - (width / 2) in
  let y = y - (height / 2) in
  draw_string_with_box string x y
