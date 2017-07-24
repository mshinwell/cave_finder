type t

val create
   : vertices:(float * float) list
  -> z:float
  -> t option

val compare_lowest_altitude_first : t -> t -> int

val fixed_point : t -> float * float

val altitude : t -> float

val length : t -> float
val longest_axis : t -> float

val slope_from_bounding_box : t -> to_:(float * float * float) -> float

val fold_edges
   : t
  -> init:'a
  -> f:('a -> dx:float -> dy:float -> 'a)
  -> 'a

val approx_inside : t -> of_:t -> bool

val centroid : t -> float * float

val bounding_box : t -> float * float * float * float
