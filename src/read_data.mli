(** Wrapper for LALInferenceReadData.c.  WARNING: we do not attempt to
    de-allocate the li_ifo_data structure that is returned by
    LALInferenceReadData, so each call to [read_data] allocates a
    significant amount of un-reclaimable memory. *)

(** Options for read_data. *)
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

(** Opaque type for LALInferenceIFOData. *)
type li_ifo_data

(** Reads the current values of the reference cells in the [Options]
    module and constructs the corresponding [li_read_data_options]
    structure. *)
val make_read_data_options : unit -> li_read_data_options

(** Read the detector data and initialize a LALInferenceIFOData
    structure.  Wrapper for LALInferenceReadData(). *)
val read_data : li_read_data_options -> li_ifo_data
