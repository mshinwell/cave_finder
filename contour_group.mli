type t

val create : Contour.t -> t

val size : t -> int

val longest_length : t -> float
val longest_axis : t -> float

val highest_altitude : t -> float

val depth : t -> float
val slope : t -> float

val approx_inside : t -> of_:t -> bool

val centroid : t -> float * float

val union : t -> t -> t

val fold
   : t
  -> init:'a
  -> f:('a -> Contour.t -> 'a)
  -> 'a

val bounding_box : t -> float * float * float * float
