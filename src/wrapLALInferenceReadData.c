#include <lal/LALInferenceReadData.h>
#include <lal/LALInferenceTemplate.h>
#include <lal/FrequencySeries.h>
#include <lal/TimeSeries.h>
#include <lal/Units.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/custom.h>

/* Don't even try to finalize the IFOData; major memory leak if you
   construct too many of these! */
static struct custom_operations IFODataOps = {
  "LALInferenceIFOData",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

#define IFOData_val(v) (* (LALInferenceIFOData **)Data_custom_val(v))

static value alloc_ifo_data(LALInferenceIFOData *d) {
  value v = alloc_custom(&IFODataOps, sizeof(LALInferenceIFOData *), 0, 1);
  IFOData_val(v) = d;
  return v;
}

static ProcessParamsTable *addCommandLineOption(ProcessParamsTable *ppt, char *oName, char *oValue) {
  ProcessParamsTable *current = NULL;

  current = calloc(1, sizeof(ProcessParamsTable));

  XLALStringCopy(current->program, "OCamlLALInference", LIGOMETA_PROGRAM_MAX);
  XLALStringCopy(current->param, oName, LIGOMETA_PARAM_MAX);
  XLALStringCopy(current->type, "string", LIGOMETA_TYPE_MAX);
  if (oValue != NULL) {
    XLALStringCopy(current->value, oValue, LIGOMETA_VALUE_MAX);
  }
  current->next = ppt;

  return current;
}

static void deletePPT(ProcessParamsTable *ppt) {
  if (ppt == NULL) {
    /* Done. */
  } else if (ppt->next == NULL) {
    free(ppt);
    /* Done. */
  } else {
    deletePPT(ppt->next);
    free(ppt);
    /* Done. */
  }
}

CAMLprim value wrapLALInferenceIFOData(value options) {
  CAMLparam1(options);
  CAMLlocal2(data, option);

  LALInferenceIFOData *d = NULL;
  ProcessParamsTable *ppt = NULL;

  /* Set srate. */
  option = Field(options, 0);
  ppt = addCommandLineOption(ppt, "--srate", String_val(option));

  /* Flow's */
  option = Field(options, 1);
  if (caml_string_length(option) == 0) {
    /* Do nothing. */
  } else {
    ppt = addCommandLineOption(ppt, "--flow", String_val(option));
  }

  /* Fhigh's */
  option = Field(options, 2);
  if (caml_string_length(option) == 0) {
    /* Do nothing. */
  } else {
    ppt = addCommandLineOption(ppt, "--fhigh", String_val(option));
  }

  option = Field(options, 3);
  ppt = addCommandLineOption(ppt, "--cache", String_val(option));
  
  option = Field(options, 4);
  ppt = addCommandLineOption(ppt, "--IFO", String_val(option));

  option = Field(options, 5);
  ppt = addCommandLineOption(ppt, "--dataseed", String_val(option));
  
  option = Field(options, 6);
  ppt = addCommandLineOption(ppt, "--PSDstart", String_val(option));
  
  option = Field(options, 7);
  ppt = addCommandLineOption(ppt, "--trigtime", String_val(option));
  
  option = Field(options, 8);
  ppt = addCommandLineOption(ppt, "--PSDlength", String_val(option));

  option = Field(options, 9);
  ppt = addCommandLineOption(ppt, "--seglen", String_val(option));

  option = Field(options, 10);
  if (Is_block(option)) {
    ppt = addCommandLineOption(ppt, "--injXML", String_val(Field(option,0)));
  }

  d = LALInferenceReadData(ppt);
  LALInferenceInjectInspiralSignal(d,ppt);
		
  LALInferenceIFOData *dElt = d;
  while (dElt != NULL) {
    /*If two IFOs have the same sampling rate, they should have the
      same timeModelh*, freqModelh*, and modelParams variables to
      avoid excess computation in model waveform generation in the
      future*/
    LALInferenceIFOData * dEltCompare=d;
    int foundIFOwithSameSampleRate=0;
    while (dEltCompare != NULL && dEltCompare!=dElt) {
      if(dEltCompare->timeData->deltaT == dElt->timeData->deltaT){
        dElt->timeModelhPlus=dEltCompare->timeModelhPlus;
        dElt->freqModelhPlus=dEltCompare->freqModelhPlus;
        dElt->timeModelhCross=dEltCompare->timeModelhCross;				
        dElt->freqModelhCross=dEltCompare->freqModelhCross;				
        dElt->modelParams=dEltCompare->modelParams;	
        foundIFOwithSameSampleRate=1;	
        break;
      }
      dEltCompare = dEltCompare->next;
    }
    if(!foundIFOwithSameSampleRate){
      dElt->timeModelhPlus  = XLALCreateREAL8TimeSeries("timeModelhPlus",
                                                        &(dElt->timeData->epoch),
                                                        0.0,
                                                        dElt->timeData->deltaT,
                                                        &lalDimensionlessUnit,
                                                        dElt->timeData->data->length);
      dElt->timeModelhCross = XLALCreateREAL8TimeSeries("timeModelhCross",
                                                        &(dElt->timeData->epoch),
                                                        0.0,
                                                        dElt->timeData->deltaT,
                                                        &lalDimensionlessUnit,
                                                        dElt->timeData->data->length);
      dElt->freqModelhPlus = XLALCreateCOMPLEX16FrequencySeries("freqModelhPlus",
                                                                &(dElt->freqData->epoch),
                                                                0.0,
                                                                dElt->freqData->deltaF,
                                                                &lalDimensionlessUnit,
                                                                dElt->freqData->data->length);
      dElt->freqModelhCross = XLALCreateCOMPLEX16FrequencySeries("freqModelhCross",
                                                                 &(dElt->freqData->epoch),
                                                                 0.0,
                                                                 dElt->freqData->deltaF,
                                                                 &lalDimensionlessUnit,
                                                                 dElt->freqData->data->length);
      dElt->modelParams = calloc(1, sizeof(LALInferenceVariables));
    }
    dElt = dElt->next;
  }

  deletePPT(ppt);

  data = alloc_ifo_data(d);

  CAMLreturn(data);
}
