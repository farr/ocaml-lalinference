open Ocamlbuild_plugin
open Command

(* Configuration section *)
let laldir = "-L/Users/farr/lscsoft/master/lib"
let lalinc = "-I/Users/farr/lscsoft/master/include"

let oUnit_dir = "/Users/farr/Documents/code/oUnit"
  
let _ = dispatch begin function
  | After_rules ->
    flag ["ocamlmklib"; "c"; "use_lalinference"]
      (S[A laldir; A "-llal"; A "-llalinference"]);
    
    flag ["c"; "compile"; "use_lalinference"]
      (S[A"-ccopt"; A lalinc; A"-ccopt"; A"-g"]);
    
    flag ["link"; "ocaml"; "use_ocamllalinference"]
      (S[A"-ccopt"; A laldir; A"-cclib"; A "-llal"; A"-cclib"; A"-llalinference"]);
    
    ocaml_lib "ocamllalinference";
    ocaml_lib ~extern:true ~dir:oUnit_dir "oUnit";
    
    dep  ["link"; "ocaml"; "use_ocamllalinference"] ["libwraplalinference.a"];
  | _ -> ()
end
