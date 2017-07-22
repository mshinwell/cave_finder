open Base
open Float.O_dot

type t = {
  x_min : float;
  x_max : float;
  y_min : float;
  y_max : float;
  z : float;
  x_fix : float;
  y_fix : float;
  other_vertices : (float * float) list;
  length : float;
  num_vertices : int;
}

let create ~vertices ~z =
  match vertices with
  | [] | [_] -> None
  | (x_fix, y_fix)::rest ->
    let x_last, y_last =
      match List.rev rest with
      | last::_ -> last
      | [] -> assert false
    in
    if Float.(<>) x_fix x_last || Float.(<>) y_fix y_last then
      None  (* contour isn't closed *)
    else
      let x_min, x_max, y_min, y_max =
        List.fold_left rest
          ~init:(x_fix, x_fix, y_fix, y_fix)
          ~f:(fun (x_min, x_max, y_min, y_max) (x, y) ->
            Float.min x_min x, Float.max x_max x,
              Float.min y_min y, Float.max y_max y)
      in
      let rec calc_length ~length ~vertices ~prev_x ~prev_y =
        match vertices with
        | [] -> length
        | (x, y)::rest ->
          let dx = x -. prev_x in
          let dy = y -. prev_y in
          let length = (Float.sqrt (dx*.dx +. dy*.dy)) +. length in
          calc_length ~length ~vertices:rest ~prev_x:x ~prev_y:y
      in
      let length =
        calc_length ~length:0.0 ~prev_x:x_fix ~prev_y:y_fix ~vertices
      in
      let t =
        { x_min;
          x_max;
          y_min;
          y_max;
          z;
          x_fix;
          y_fix;
          other_vertices = rest;
          length;
          num_vertices = 1 + List.length rest;
        }
      in
      Some t

let compare_by_length_inverted t1 t2 =
  Int.neg (Float.compare t1.length t2.length)

let shorter t1 ~than:t2 =
  Float.(<) t1.length t2.length

let inside_bounding_box t1 ~of_:t2 =
  Float.(>=) t1.x_min t2.x_min
    && Float.(<=) t1.x_max t2.x_max
    && Float.(>=) t1.y_min t2.y_min
    && Float.(<=) t1.y_max t2.y_max

let approx_inside t1 ~of_:t2 =
  inside_bounding_box t1 ~of_:t2
    && shorter t1 ~than:t2
    && Float.(<) t1.z t2.z

let fixed_point t = t.x_fix, t.y_fix
let altitude t = t.z
let length t = t.length

let longest_axis t =
  let x_range = t.x_max -. t.x_min in
  let y_range = t.y_max -. t.y_min in
  Float.max x_range y_range

let fold_edges t ~init ~f =
  let _x_prev, _y_prev, acc =
    List.fold_left t.other_vertices
      ~init:(t.x_fix, t.y_fix, init)
      ~f:(fun (x_prev, y_prev, acc) (x, y) ->
        let dx = x -. x_prev in
        let dy = y -. y_prev in
        let acc = f acc ~dx ~dy in
        x, y, acc)
  in
  acc

let centroid t =
  let num_vertices = Float.of_int t.num_vertices in
  let x_sum =
    List.fold t.other_vertices
      ~init:t.x_fix
      ~f:(fun x_sum (x, _) -> x +. x_sum)
  in
  let y_sum =
    List.fold t.other_vertices
      ~init:t.y_fix
      ~f:(fun y_sum (_, y) -> y +. y_sum)
  in
  x_sum /. num_vertices, y_sum /. num_vertices

let slope_from_bounding_box t ~to_:(x, y, z) =
  let slope x' y' =
    let z' = t.z in
    let dx = x -. x' in
    let dy = y -. y' in
    let dist_in_xy_plane = Float.sqrt (dx*.dx +. dy*.dy) in
    let dz = Float.abs (z -. z') in
    let angle = Float.atan (dz /. dist_in_xy_plane) in
    angle /. Float.pi *. 360.0
  in
  let slope1 = slope t.x_min t.y_min in
  let slope2 = slope t.x_min t.y_max in
  let slope3 = slope t.x_max t.y_min in
  let slope4 = slope t.x_max t.y_max in
  (slope1 +. slope2 +. slope3 +. slope4) /. 4.0
