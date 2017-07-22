open Base
open Float.O_dot

type t = {
  contours : Contour.t list;  (* longest one first *)
}

let create contour =
  { contours = [contour];
  }

let size t = List.length t.contours

let longest t =
  match t.contours with
  | longest::_ -> longest
  | [] -> assert false

let shortest t =
  match List.rev t.contours with
  | shortest::_ -> shortest
  | [] -> assert false

let longest_length t = Contour.length (longest t)
let longest_axis t = Contour.longest_axis (longest t)

let depth t =
  let longest = longest t in
  let shortest = shortest t in
  let highest = Contour.altitude longest in
  let lowest = Contour.altitude shortest in
  let depth = highest -. lowest in
  Float.max 0.0 depth

let union t1 t2 =
 let contours = t1.contours @ t2.contours in
 { contours = List.sort contours ~cmp:Contour.compare_by_length_inverted;
 }

let approx_inside t1 ~of_:t2 =
  Contour.approx_inside (longest t1) ~of_:(longest t2)

let centroid t =
  let centroids = List.map t.contours ~f:Contour.centroid in
  let num_centroids = Float.of_int (List.length centroids) in
  let x_sum, y_sum =
    List.fold centroids
      ~init:(0.0, 0.0)
      ~f:(fun (x_sum, y_sum) (x, y) -> x +. x_sum, y +. y_sum)
  in
  x_sum /. num_centroids, y_sum /. num_centroids

let slope t =
  let x, y = centroid t in
  let z = Contour.altitude (shortest t) in
  Contour.slope_from_bounding_box (longest t) ~to_:(x, y, z)

let fold t ~init ~f =
  List.fold t.contours ~init ~f
