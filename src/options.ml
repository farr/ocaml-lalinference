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
