open Options

type li_read_data_options = {
  srate : string;
  flows : string;
  fhighs : string;
  caches : string;
  ifos : string;
  dataseed : string;
  psdstart : string;
  trigtime : string;
  psdlen : string;
  seglen : string;
  inj_xml : string option
}

type li_ifo_data

let rec process_option_list_to_commas to_string = function 
  | [] -> ""
  | [x] -> to_string x
  | x :: xs -> to_string x ^ "," ^ (process_option_list_to_commas to_string xs)

let process_option_list to_string xs = 
  "[" ^ (process_option_list_to_commas to_string xs) ^ "]"

let make_read_data_options () = 
  {flows = (match !flow with 
     | [] -> ""
     | _ -> process_option_list string_of_float !flow);
   fhighs = (match !fhigh with 
     | [] -> ""
     | _ -> process_option_list string_of_float !fhigh);
   srate = string_of_float !srate;
   caches = process_option_list (fun x -> x) !cache;
   ifos = process_option_list (fun x -> x) !ifos;
   dataseed = string_of_int !dataseed;
   psdstart = string_of_float !psdstart;
   trigtime = string_of_float !trigtime;
   psdlen = string_of_float !psdlen;
   seglen = string_of_float !seglen;
   inj_xml = !injxml}

external read_data : li_read_data_options -> li_ifo_data = "wrapLALInferenceIFOData"
