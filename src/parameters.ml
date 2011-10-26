open Options
open Oli_stats

type non_spin_params = {
  m1 : float;
  m2 : float;
  dist : float;
  cos_i : float;
  psi : float;
  phi : float;
  time : float;
  ra : float;
  sin_dec : float;
}

type spin_params = {
  a1 : float;
  a2 : float;
  cos_tilt1 : float;
  phi1 : float;
  cos_tilt2 : float;
  phi2 : float;
}

type params = 
  | NonSpinning of non_spin_params
  | Spinning of non_spin_params * spin_params

type logl_options = {
  nseg : int
}

let pi = 3.1415926535897932385

let rec draw_mass () = 
  let m1 = draw_uniform !mmin !mmax and 
      m2 = draw_uniform !mmin !mmax in 
  let mtot = m1+.m2 in 
    if m2 > m1 || mtot < !mtotmin || mtot > !mtotmax then 
      draw_mass ()
    else
      (m1, m2)

let rec draw_dist () = 
  let d = draw_radius_by_volume () in 
  let dist = !dmax *. d in 
    if dist < !dmin then 
      draw_dist ()
    else
      dist

let time_bounds () = 
  (~-. !one_sided_dt, !one_sided_dt)

let draw_non_spinning_prior () =
  let (tmin,tmax) = time_bounds () in 
  let (m1,m2) = draw_mass () and 
      dist = draw_dist () and 
      cos_i = draw_uniform (-1.0) 1.0 and 
      psi = draw_uniform 0.0 pi and 
      phi = draw_uniform 0.0 (2.0*.pi) and 
      time = draw_uniform tmin tmax and 
      ra = draw_uniform 0.0 (2.0*.pi) and 
      sin_dec = draw_uniform (-1.0) 1.0 in 
    {m1 = m1; m2 = m2; dist = dist; cos_i = cos_i; psi = psi; phi = phi; time = time; ra = ra; sin_dec = sin_dec}

let draw_spinning_prior () = 
  let a1 = draw_uniform 0.0 1.0 and 
      a2 = draw_uniform 0.0 1.0 and 
      cos_t1 = draw_uniform (-1.0) 1.0 and 
      cos_t2 = draw_uniform (-1.0) 1.0 and 
      phi1 = draw_uniform 0.0 (2.0*.pi) and 
      phi2 = draw_uniform 0.0 (2.0*.pi) in 
    {a1 = a1; a2 = a2; cos_tilt1 = cos_t1; cos_tilt2 = cos_t2; phi1 = phi1; phi2 = phi2}

let log_p_nonspin {m1=m1; m2=m2; dist=dist; cos_i=cos_i; psi=psi; phi=phi; time=time; ra=ra; sin_dec=sin_dec} = 
(* Just check bounds for most things; only non-flat part is
   dist**2.0. *)
  let mtot = m1+.m2 and 
      (tmin,tmax) = time_bounds () in 
    if m1 < !mmin || m1 > !mmax ||
      m2 < !mmin || m2 > !mmax || 
      m2 > m1 ||
      mtot < !mtotmin || mtot > !mtotmax ||
      dist < !dmin || dist > !dmax ||
      cos_i < -1.0 || cos_i > 1.0 ||
      psi < 0.0 || psi > pi ||
      phi < 0.0 || phi > 2.0*.pi ||
      time < tmin || time > tmax ||
      ra < 0.0 || ra > 2.0*.pi || 
      sin_dec < -1.0 || sin_dec > 1.0 then 
      neg_infinity
    else
      2.0*.(log dist)

let log_p_spin {a1=a1; a2=a2; cos_tilt1 = ct1; cos_tilt2 = ct2; phi1 = phi1; phi2 = phi2} = 
(* Again, check within bounds.  Otherwise flat. *)
  if a1 < 0.0 || a1 > 1.0 ||
    a2 < 0.0 || a2 > 1.0 ||
    ct1 < -1.0 || ct1 > 1.0 ||
    ct2 < -1.0 || ct2 > 1.0 ||
    phi1 < 0.0 || phi1 > 2.0*.pi ||
    phi2 < 0.0 || phi2 > 2.0*.pi then 
    neg_infinity
  else
    0.0

let logp = function 
  | NonSpinning ns -> log_p_nonspin ns
  | Spinning (ns, s) -> (log_p_nonspin ns) +. (log_p_spin s)
  
external ext_log_likelihood : logl_options -> Read_data.li_ifo_data -> params -> float = "wrapLALInferenceFreqDomainStudentTLogLikelihood"

let log_likelihood opts data = function 
  | NonSpinning ns -> 
    ext_log_likelihood opts data (NonSpinning {ns with time = ns.time +. !trigtime})
  | Spinning (ns, s) -> 
    ext_log_likelihood opts data (Spinning ({ns with time = ns.time +. !trigtime}, s))
