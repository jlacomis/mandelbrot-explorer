open Drawing

type point =
  { x : float
  ; y : float
  }

let string_of_point p = Format.sprintf "%.20f %.20f" p.x p.y

type range =
  { min : point
  ; max : point
  }

let init () =
  let open Graphics in
  open_graph "";
  set_window_title "Mandelbrot";
  auto_synchronize false

let default_iterations = 100
let start_range = { min = { x = (-.2.5) ; y = (-.1.0) }
                  ; max = { x = 1.0 ; y = 1.0 }
                  }

let center () =
  let x = (Graphics.size_x ()) / 2 in
  let y = (Graphics.size_y ()) / 2 in
  x, y

let rescale pixel_max min max value =
  let range = max -. min in
  let offset = 0.0 +. min in
  let ratio = 1.0 /. (float_of_int pixel_max) in
  ((float_of_int value) *. ratio) *. range +. offset

let get_iterations iterations x y range =
  let max_x = Graphics.size_x () in
  let max_y = Graphics.size_y () in
  let x0 = rescale max_x range.min.x range.max.x x in
  let y0 = rescale max_y range.min.y range.max.y y in
  let rec iterate iteration rsquare isquare zsquare =
    if iteration >= iterations || rsquare +. isquare > 4.0 then
      iteration
    else
      begin
        let x = rsquare -. isquare +. x0 in
        let y = zsquare -. rsquare -. isquare +. y0 in
        let rsquare = x *. x in
        let isquare = y *. y in
        let zsquare = (x +. y) *. (x +. y) in
        iterate (iteration + 1) rsquare isquare zsquare
      end
  in
  iterate 0 0.0 0.0 0.0

let hsv_to_rgb ~h ~s ~v =
  let c = v *. s in
  let h' = Core.Float.mod_float (h /. 60.0) 2.0 in
  let x = c *. (1.0 -. (Float.abs (h' -. 1.0))) in
  let m = v -. c in
  let r', g', b' =
    match h with
    | h when 0.0 <= h && h < 60.0 -> c, x, 0.0
    | h when 60.0 <= h && h < 120.0 -> x, c, 0.0
    | h when 120.0 <= h && h < 180.0 -> 0.0, c, x
    | h when 180.0 <= h && h < 240.0 -> 0.0, x, c
    | h when 240.0 <= h && h < 300.0 -> x, 0.0, c
    | h when 300.0 <= h && h <= 360.0 -> c, 0.0, x
    | _ -> 0.0, 0.0, 0.0
  in
  let decode value = int_of_float ((value +. m) *. 255.0) in
  decode r', decode g', decode b'

let rec draw_mandelbrot ?(x=0) ?(y=0) ?(iterations=default_iterations) range =
  let max_x_pixels = Graphics.size_x () in
  let max_y_pixels = Graphics.size_y () in
  let x, y =
    match x with
    | x when x >= max_x_pixels -> 0, y + 1
    | _ -> x + 1, y
  in
  if y >= max_y_pixels then
    ()
  else
    let color =
      match get_iterations iterations x y range with
      | x when x >= iterations -> Graphics.black
      | i ->
         let scaled = (float_of_int i) /. (float_of_int iterations) in
         let h = 360.0 *. scaled in
         let r, g, b = hsv_to_rgb ~h ~s:1.0 ~v:1.0 in
         Graphics.rgb r g b
    in
    Graphics.set_color color;
    Graphics.plot x y;
    draw_mandelbrot ~x ~y ~iterations range

let get_rectangle (fst_event : Graphics.status) range =
  let snd_event = Graphics.wait_next_event [ Button_up; ] in
  let max_pixel_x = Graphics.size_x () in
  let max_pixel_y = Graphics.size_y () in
  let rescale_x = rescale max_pixel_x range.min.x range.max.x in
  let rescale_y = rescale max_pixel_y range.min.y range.max.y in
  let new_max_x = max fst_event.mouse_x snd_event.mouse_x in
  let new_min_x = min fst_event.mouse_x snd_event.mouse_x in
  let new_max_y = max fst_event.mouse_y snd_event.mouse_y in
  let new_min_y = min fst_event.mouse_y snd_event.mouse_y in
  let min = { x = rescale_x new_min_x; y = rescale_y new_min_y } in
  let max = { x = rescale_x new_max_x; y = rescale_y new_max_y } in
  { min; max }

let draw_scale range =
  let open Graphics in
  let min_string = string_of_point range.min in
  draw_string_with_box min_string 0 0;
  let max_string = string_of_point range.max in
  draw_string_with_box_tr max_string (size_x ()) (size_y ())

let draw_message () =
  let s = "Drawing..." in
  let x, y = center () in
  draw_string_with_box_c s x y

let draw_iterations iterations =
  let s = Format.sprintf "Iterations: %d" iterations in
  draw_string_with_box_tl s 0 (Graphics.size_y ())

let draw iterations range =
  draw_message ();
  draw_mandelbrot ~iterations range;
  draw_scale range;
  draw_iterations iterations;
  Graphics.synchronize ()

let () =
  init ();
  ignore (Graphics.wait_next_event [ Key_pressed; ]);
  draw default_iterations start_range;
  let rec loop iterations range =
    let redraw iterations range =
      draw iterations range;
      loop iterations range
    in
    let fst_event = Graphics.wait_next_event [ Button_down; Key_pressed; ] in
    match fst_event.keypressed with
    | true ->
      begin
        match fst_event.key with
        (* Redraw *)
        | 'd' ->
          redraw iterations range
        (* Zoom out *)
        | 'z' ->
          redraw iterations start_range
        (* Reset *)
        | 'r' ->
          redraw default_iterations start_range
        (* Increase iterations *)
        | '+' ->
          let increment = max (iterations / 5) 1 in
          let iterations = iterations + increment in
          redraw iterations range
        (* Decrease iterations *)
        | '-' ->
          let decrement = max (iterations / 5) 1 in
          let iterations = iterations - decrement in
          redraw iterations range
        (* Quit *)
        | 'q' | _ -> ()
      end
    | false ->
       let new_range = get_rectangle fst_event range in
       redraw iterations new_range
  in
  loop default_iterations start_range
