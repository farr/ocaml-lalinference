(** Statistical Routines *)

(** [draw_uniform a b] returns a random number uniformly distributed
    between a and b. *)
val draw_uniform : float -> float -> float

(** Draws an angle isotropically (i.e. flat in cosine). *)
val draw_iso_angle : unit -> float

(** [draw_from_dist ?xmin ?xmax ?ymin ?ymax f] draws from the PDF
    defined by [f] using von Neumann rejection sampling.  The
    region between [(xmin, ymin)] and [(xmax, ymax)] in the plane
    should enclose the curve [f x]. *)
val draw_from_dist : ?xmin : float -> ?xmax : float -> ?ymin : float -> ?ymax : float -> (float -> float) -> float

(** [draw_radius_by_volume] returns a radius chosen uniformly in the
    volume of the unit sphere. *)
val draw_radius_by_volume : unit -> float
