(** Global options for the code. *)

(** Minimum component mass (solar masses). *)
val mmin : float ref

(** Maximum component mass. *)
val mmax : float ref 

(** Minimum total system mass. *)
val mtotmin : float ref

(** Maxmium total system mass. *)
val mtotmax : float ref

(** Minimum and maximum distances. *)
val dmin : float ref
val dmax : float ref

(** Trigger time and prior window around it for coalescence time. *)
val trigtime : float ref
val one_sided_dt : float ref

(** Optional string pointing at the injection XML file. *)
val injxml : string option ref

(** PSD estimation parameters. *)
val psdstart : float ref
val psdlen : float ref
val seglen : float ref

(** Intereferometer parameters.  For example, [ifos] can be set to
    [\["H1"; "L1"; "V1"\]].  A corresponding [cache] could be
    [\["LALLIGO"; "LALLIGO"; "LALVirgo"\]], or three paths to the
    corresponding cache files. *)
val cache : string list ref 
val ifos : string list ref
val flow : float list ref
val fhigh : float list ref

(** Sample rate, in Hz. *)
val srate : float ref

(** The seed to use for generating random detector noise, if the
    caches are ["LALLIGO"] or ["LALVirgo"]. *)
val dataseed : int ref

(** An option list suitable for {!Arg.parse} *)
val options : (string * Arg.spec * string) list
