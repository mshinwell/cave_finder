open Base
open Float.O_dot

type t = {
  longest : Contour.t;
  others_deepest_first : Contour.t list;
}

let create contour =
  { longest = contour;
    others_deepest_first = [];
  }

let size t = 1 + List.length t.others_deepest_first

let longest t = t.longest

let highest t = t.longest

let highest_altitude t = Contour.altitude (highest t)

let deepest t =
  match t.others_deepest_first with
  | deepest::_ -> deepest
  | [] -> t.longest

let longest_length t = Contour.length (longest t)
let longest_axis t = Contour.longest_axis (longest t)

let invariant t =
  assert (List.for_all t.others_deepest_first ~f:(fun contour ->
    Float.(>=) (longest_length t) (Contour.length contour)));
  (* This next one is a bit dubious, only holds by virtue of
     [Cave_finder.try_to_fit] *)
  assert (List.for_all t.others_deepest_first ~f:(fun contour ->
    Float.(>=) (Contour.altitude t.longest)
      (Contour.altitude contour)))

let depth t =
  let highest = highest t in
  let deepest = deepest t in
  let highest = Contour.altitude highest in
  let lowest = Contour.altitude deepest in
  assert (Float.(>=) highest lowest);
  highest -. lowest

let union t1 t2 =
  let longest, other =
    if Float.(>) (longest_length t1) (longest_length t2) then
      t1.longest, t2.longest
    else
      t2.longest, t1.longest
  in
  let others_deepest_first =
    List.sort (other :: t1.others_deepest_first @ t2.others_deepest_first)
      ~cmp:Contour.compare_lowest_altitude_first
  in
  let t =
    { longest;
      others_deepest_first;
    }
  in
  invariant t;
  t

let approx_inside t1 ~of_:t2 =
  Contour.approx_inside (longest t1) ~of_:(longest t2)

let all_contours t =
  t.others_deepest_first @ [t.longest]

let centroid t =
  let centroids = List.map (all_contours t) ~f:Contour.centroid in
  let num_centroids = Float.of_int (List.length centroids) in
  let x_sum, y_sum =
    List.fold centroids
      ~init:(0.0, 0.0)
      ~f:(fun (x_sum, y_sum) (x, y) -> x +. x_sum, y +. y_sum)
  in
  x_sum /. num_centroids, y_sum /. num_centroids

let slope t =
  let x, y = centroid t in
  let z = Contour.altitude (deepest t) in
  Contour.slope_from_bounding_box (longest t) ~to_:(x, y, z)

let fold t ~init ~f =
  List.fold (all_contours t) ~init ~f

let bounding_box t = Contour.bounding_box (longest t)
