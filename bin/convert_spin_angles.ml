open Mcmc
open Parameters
open Read_write

let converted_header = "m1 m2 dist cosiota psi phi_orb time ra sindec a1 a2 theta1 theta2 phi1 phi2 logl logprior"

let _ = 
  ignore(read_line ());
  print_string converted_header;
  print_newline ();
  try 
    let rec loop () = 
      let {value = params;
           like_prior = {log_likelihood = ll; log_prior = lp}} = read_sample from_array stdin in 
      match params, spin_to_observer_angles params with 
        | Spinning(ns, s), ([|theta1; phi1|], [|theta2; phi2|]) -> 
          Printf.printf "%g %g %g %g %g %g %g %g %g %g %g %g %g %g %g %g %g\n"
            ns.m1 ns.m2
            ns.dist ns.cos_i
            ns.psi ns.phi
            ns.time
            ns.ra ns.sin_dec
            s.a1 s.a2
            theta1 theta2
            phi1 phi2
            ll lp;
          loop ()
        | _ -> raise (Failure "convert_spin_angles: bad params") in 
      loop ()
  with 
    | End_of_file -> 
      exit 0
   
            
