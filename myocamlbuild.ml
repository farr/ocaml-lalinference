open Ocamlbuild_plugin
open Command

(* Configuration section *)
let laldir = "-L/Users/farr/lscsoft/master/lib"
let lalinc = "-I/Users/farr/lscsoft/master/include"

let oUnit_dir = "/Users/farr/Documents/code/oUnit"
let mcmc_dir = "/Users/farr/Documents/code/mcmc-ocaml/_build"
  
let _ = dispatch begin function
  | After_rules ->
    flag ["ocamlmklib"; "c"; "use_lalinference"]
      (S[A laldir; A "-llal"; A "-llalinference"]);
    
    flag ["c"; "compile"; "use_lalinference"]
      (S[A"-ccopt"; A lalinc; A"-ccopt"; A"-g"; A"-ccopt"; A"-std=c99"]);
    
    flag ["link"; "ocaml"; "use_ocamllalinference"]
      (S[A"-ccopt"; A laldir; A"-cclib"; A "-llal"; A"-cclib"; A"-llalinference"]);
    
    ocaml_lib "ocamllalinference";
    ocaml_lib ~extern:true ~dir:oUnit_dir "oUnit";
    ocaml_lib ~extern:true ~dir:mcmc_dir "mcmc";
    
    dep  ["link"; "ocaml"; "use_ocamllalinference"] ["libwraplalinference.a"];
  | _ -> ()
end
