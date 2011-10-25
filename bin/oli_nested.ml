open Parameters

let spinning = ref false
let npost = ref 10000

let options = 
  Arg.align
    (Options.options @
       [("-spinning", Arg.Set spinning, " run with spinning templates");
        ("-seed", Arg.Int (fun s -> Random.init s), "S seed the OCaml RNG");
        ("-npost", Arg.Set_int npost, "N number of posterior samples to output")])

let draw_prior () = 
  let nonspin = draw_non_spinning_prior () in 
    if !spinning then 
      let spin = draw_spinning_prior () in 
        Spinning(nonspin, spin)
    else
      NonSpinning nonspin

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
    raise (Invalid_argument "Oli_nested.from_array")

let nseg () = 
  int_of_float (!Options.psdlen /. !Options.seglen +. 0.5)

let logl data = 
  let zero_logl = Parameters.log_likelihood {nseg = nseg ()} data 
    (NonSpinning 
       {m1 = 5.0; m2 = 4.0; dist = infinity; cos_i = 0.0; ra = 3.14; sin_dec = 0.0; phi = 3.14; psi = 1.5; 
        time = !Options.trigtime}) in 
    Printf.fprintf stderr "Zero Log(L) = %g\n%!" zero_logl;
    fun params -> 
      if logp params > neg_infinity then 
        (Parameters.log_likelihood {nseg = nseg ()} data params) -. zero_logl
      else
        neg_infinity
        
let observer = 
  let last_logl = ref neg_infinity in 
    fun dead_pt -> 
      let logl = dead_pt.Mcmc.like_prior.Mcmc.log_likelihood in 
        if logl -. !last_logl > 1.0 then begin
          last_logl := logl;
          Read_write.write_sample to_array stdout dead_pt;
          flush stdout            
        end
          

let _ = 
  Random.self_init ();
  Arg.parse options (fun _ -> ()) "oli_nested.native OPTION ...";
  let data = Read_data.read_data (Read_data.make_read_data_options ()) in 
  let logl = logl data in 
  let nested_out = Nested.nested_evidence ~observer:observer to_array from_array draw_prior logl logp in 
  let post = Nested.posterior_samples !npost nested_out in 
  let nout = open_out "nested.dat" and 
      pout = open_out "posterior.dat" in 
    Read_write.write_nested to_array nout nested_out;
    Read_write.write to_array pout post;
    close_out nout;
    close_out pout
    
