let mmin = ref 1.0
let mmax = ref 34.0
let mtotmin = ref 2.0 
let mtotmax = ref 35.0

let dmin = ref 1.0
let dmax = ref 100.0

let trigtime = ref 0.0
let one_sided_dt = ref 0.1

let injxml : string option ref = ref None

let psdstart = ref 0.0
let psdlen = ref 256.0
let seglen = ref 8.0

let cache : string list ref = ref []
let ifos : string list ref = ref []
let flow : float list ref = ref []
let fhigh : float list ref = ref []

let srate = ref 4096.0

let dataseed = ref 0

let options = 
  [("-mmin", Arg.Set_float mmin, "M minimum component mass");
   ("-mmax", Arg.Set_float mmax, "M maximum component mass");
   ("-mtotmin", Arg.Set_float mtotmin, "M minimum total mass");
   ("-mtotmax", Arg.Set_float mtotmax, "M maximum total mass");
   ("-dmin", Arg.Set_float dmin, "D minimum distance");
   ("-dmax", Arg.Set_float dmax, "D maximum distance");
   ("-trigtime", Arg.Set_float trigtime, "T trigger time");
   ("-dt", Arg.Set_float one_sided_dt, "dT one-sided window about trigger");
   ("-injxml", Arg.String (fun s -> injxml := Some s), "FILE injection XML file");
   ("-psdstart", Arg.Set_float psdstart, "T start time for PSD samples");
   ("-psdlen", Arg.Set_float psdlen, "dT length of PSD estimation stretch");
   ("-seglen", Arg.Set_float seglen, "dT length of each PSD estimation segment");
   ("-cache", Arg.String (fun s -> cache := s :: !cache), "CFILE cache file for each IFO");
   ("-ifo", Arg.String (fun s -> ifos := s :: !ifos), "INAME name of IFO");
   ("-flow", Arg.Float (fun fl -> flow := fl :: !flow), "F low frequency of IFO data");
   ("-fhigh", Arg.Float (fun fh -> fhigh := fh :: !fhigh), "F high frequency of IFO data");
   ("-srate", Arg.Set_float srate, "F sampling frequency");
   ("-dataseed", Arg.Set_int dataseed, "DS seed for generating detector noise")]
