(** Parameter structures. *)

(** Non-spinning parameters. *)
type non_spin_params = {
  m1 : float; (** Mass 1 in solar masses (always > m2). *)
  m2 : float; (** Mass 2 in solar masses (always < m1). *)
  dist : float; (** Distance in Mpc. *)
  cos_i : float; (** Cos(inclination). *)
  psi : float; (** Detector polarization angle. *)
  phi : float; (** Waveform phase at reference point in cycle. *)
  time : float; (** Coalescence time. *)
  ra : float; (** Right-ascension in radians (i.e. [0, 2pi]). *)
  sin_dec : float; (** sin(declination) *)
}

(** Spin parameters. *)
type spin_params = {
  a1 : float; (** Dimensionless spin magnitude. *)
  a2 : float; (** Dimensionless spin magnitude. *)
  cos_tilt1 : float; (** Lhat dot Shat.  Angle between ang. momentum and spin. *)
  phi1 : float; (** Azimuthal angle in frame where zhat = Lhat, xhat = LOS. *)
  cos_tilt2 : float; (** See cos_tilt1. *)
  phi2 : float; (** See phi1. *)
}

(** Parameters are either spinning or non-spinning. *)
type params = 
  | NonSpinning of non_spin_params
  | Spinning of non_spin_params * spin_params

(** The Student-t log(L) depends on the number of segments used to
    estimate the PSD. *)
type logl_options = {
  nseg : int
}

(** Draw from the non-spinning prior. *)
val draw_non_spinning_prior : unit -> non_spin_params

(** Draw from the prior on the spinning parameters. *)
val draw_spinning_prior : unit -> spin_params

(** Log of the non-spinning part of the prior. *)
val log_p_nonspin : non_spin_params -> float

(** Log of the spinning part of the prior. *)
val log_p_spin : spin_params -> float

(** Log prior handling both spinning and non-spinning cases. *)
val logp : params -> float

(** The likelihood of a given set of parameters. *)
val log_likelihood : logl_options -> Read_data.li_ifo_data -> params -> float

(** Convert params to an array. *)
val to_array : params -> float array

(** Convert a float array to params. *)
val from_array : float array -> params
