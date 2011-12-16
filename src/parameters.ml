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

let nonspin_to_array {m1=m1; m2=m2; dist=dist; cos_i=cos_i; psi=psi; phi=phi; time=time; ra=ra; sin_dec=sin_dec} = 
  let a = Array.make 9 0.0 in 
    a.(0) <- m1;
    a.(1) <- m2;
    a.(2) <- dist;
    a.(3) <- cos_i;
    a.(4) <- psi;
    a.(5) <- phi;
    a.(6) <- time;
    a.(7) <- ra;
    a.(8) <- sin_dec;
    a

let array_to_nonspin a = 
  {m1=a.(0); m2=a.(1); dist=a.(2); cos_i=a.(3); psi=a.(4); phi=a.(5); time=a.(6); ra=a.(7); sin_dec=a.(8)}

let spin_to_array {a1=a1; a2=a2; cos_tilt1=cos_tilt1; phi1=phi1; cos_tilt2=cos_tilt2; phi2=phi2} = 
  let a = Array.make 6 0.0 in 
    a.(0) <- a1;
    a.(1) <- a2;
    a.(2) <- cos_tilt1;
    a.(3) <- phi1;
    a.(4) <- cos_tilt2;
    a.(5) <- phi2;
    a

let array_to_spin a = 
  {a1=a.(0); a2=a.(1); cos_tilt1=a.(2); phi1=a.(3); cos_tilt2=a.(4); phi2=a.(5)}

let to_array = function 
  | NonSpinning nospin -> 
    nonspin_to_array nospin
  | Spinning(nospin,spin) -> 
    Array.append (nonspin_to_array nospin) (spin_to_array spin)

let from_array a = 
  if Array.length a = 9 then 
    NonSpinning (array_to_nonspin a)
  else if Array.length a = 15 then 
    let ns = array_to_nonspin (Array.sub a 0 9) and 
        s = array_to_spin (Array.sub a 9 6) in 
      Spinning(ns,s)
  else
    raise (Invalid_argument "Parameters.from_array")

let spin_to_observer_angles = function 
  | NonSpinning _ -> 
    raise (Invalid_argument "spin_to_observer_angles: non-spinning parameters.")
  | Spinning({cos_i = cos_i}, {cos_tilt1 = ct1; cos_tilt2 = ct2; phi1 = phi1; phi2 = phi2}) -> 
    let sin_i = sqrt (1.0 -. cos_i*.cos_i) in 
    let zhat = [|sin_i; 0.0; cos_i|] in (* zhat is along l *)
    let xhat = [| ~-.cos_i; 0.0; sin_i|] in (* N = [|0.0; 0.0; 1.0]; xhat = N - (N*z)z *)
    let yhat = [|zhat.(1)*.xhat.(2) -. zhat.(2)*.xhat.(1);
                 zhat.(2)*.xhat.(0) -. zhat.(0)*.xhat.(2);
                 zhat.(0)*.xhat.(1) -. zhat.(1)*.xhat.(0)|] in 
    let cp1 = cos phi1 and sp1 = sin phi1 and 
        cp2 = cos phi2 and sp2 = sin phi2 in 
    let st1 = sqrt (1.0 -. ct1*.ct1) and st2 = sqrt (1.0 -. ct2*.ct2) in 
    let s1 = Array.make 3 0.0 and s2 = Array.make 3 0.0 in 
      for i = 0 to 2 do 
        s1.(i) <- cp1*.st1*.xhat.(i) +. sp1*.st1*.yhat.(i) +. ct1*.zhat.(i);
        s2.(i) <- cp2*.st2*.xhat.(i) +. sp2*.st2*.yhat.(i) +. ct2*.zhat.(i)
      done;
      let theta1 = acos (s1.(2) /. sqrt (s1.(0)*.s1.(0) +. s1.(1)*.s1.(1) +. s1.(2)*.s1.(2))) and 
          theta2 = acos (s2.(2) /. sqrt (s2.(0)*.s2.(0) +. s2.(1)*.s2.(1) +. s2.(2)*.s2.(2))) and 
          phi1 = atan2 s1.(1) s1.(0) and 
          phi2 = atan2 s2.(1) s2.(0) in 
        ([|theta1; phi1|], [|theta2; phi2|])
