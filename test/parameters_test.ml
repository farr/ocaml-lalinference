open OUnit
open Options
open Parameters
open Read_data

let test_basic_logl () = 
  let p = NonSpinning({m1 = 5.0; m2 = 4.0; dist = 50.0; cos_i = -0.25;
                       psi = 2.6; phi = 5.8; time = 0.0; ra = 2.9;
                       sin_dec = 0.2}) in 
    trigtime := 968654558.0;
    psdstart := 968654560.0;
    cache := ["LALLIGO"; "LALLIGO"; "LALVirgo"];
    ifos := ["H1"; "L1"; "V1"];
    flow := [40.0; 40.0; 40.0];
    dataseed := 123;
    let data = read_data (make_read_data_options ()) in
    let logL = log_likelihood {nseg = int_of_float ((!psdlen /. !seglen) +. 0.5)} data p in 
      assert_bool "log(L) too small" (logL > -49000.0);
      assert_bool "log(L) too big" (logL < -48000.0)

let test_positive_mass () = 
  for i = 1 to 100000 do 
    let p = draw_non_spinning_prior () in 
      assert_bool "prior draw outside prior bounds" (log_p_nonspin p > neg_infinity)
  done

let tests = "parameters.ml tests" >:::
  ["log(L) with known parameters test" >:: test_basic_logl;
   "positive mass prior draws" >:: test_positive_mass]
