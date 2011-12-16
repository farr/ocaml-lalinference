open Parameters

let spinning = ref false
let npost = ref 10000
let nmcmc = ref 100
let nlive = ref 1000

let options = 
  Arg.align
    (Options.options @
       [("-spinning", Arg.Set spinning, " run with spinning templates");
        ("-seed", Arg.Int (fun s -> Random.init s), "S seed the OCaml RNG");
        ("-npost", Arg.Set_int npost, "N number of posterior samples to output");
        ("-nmcmc", Arg.Set_int nmcmc, "N number of MCMC steps to choose next live point");
        ("-nlive", Arg.Set_int nlive, "N number of live points")])

let draw_prior () = 
  let nonspin = draw_non_spinning_prior () in 
    if !spinning then 
      let spin = draw_spinning_prior () in 
        Spinning(nonspin, spin)
    else
      NonSpinning nonspin

let nonspin_header = "m1 m2 dist cosiota psi phi_orb time ra sindec"
let spin_header = "a1 a2 costilt1 costilt2 phiL1 phiL2"
let logl_header = "logl logprior"

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
          let (nacc,nrej) = Mcmc.get_counters () in 
          let ntot = nacc + nrej in 
          let accept_rate = (float_of_int nacc)/.(float_of_int ntot) in 
          last_logl := logl;
          Read_write.write_sample to_array stdout dead_pt;
          Printf.fprintf stderr "Current accept rate = %g\n" accept_rate;
          flush stdout;
          flush stderr
        end
          

let _ = 
  Random.self_init ();
  Arg.parse options (fun _ -> ()) "oli_nested.native OPTION ...";
  let data = Read_data.read_data (Read_data.make_read_data_options ()) in 
  let logl = logl data in 
    Mcmc.reset_counters ();
  let nested_out = Nested.nested_evidence ~observer:observer ~nlive:(!nlive) ~nmcmc:(!nmcmc) to_array from_array draw_prior logl logp in 
  let (nacc,nrej) = Mcmc.get_counters () in 
  let ntot = nacc + nrej in 
    Printf.fprintf stderr "Nacc = %d, nrej = %d, for acceptance ratio of %g\n%!" 
      nacc nrej ((float_of_int nacc) /. (float_of_int ntot));
  let post = Nested.posterior_samples !npost nested_out in 
  let nout = open_out "nested.dat" and 
      pout = open_out "posterior.dat" in 
    Printf.fprintf pout "%s " nonspin_header;
    if !spinning then Printf.fprintf pout "%s " spin_header;
    Printf.fprintf pout "%s\n" logl_header;
    Read_write.write_nested to_array nout nested_out;
    Read_write.write to_array pout post;
    close_out nout;
    close_out pout
