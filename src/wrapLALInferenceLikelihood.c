#include <lal/LALInference.h>
#include <lal/LALInferenceLikelihood.h>
#include <lal/LALInferenceTemplate.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/custom.h>

CAMLprim value wrapLALInferenceFreqDomainStudentTLogLikelihood(value options, value IFOData, value params) {
  CAMLparam3(options, IFOData, params);
  CAMLlocal3(option, nsparams, vlogL);

  LALInferenceVariables LIparams;
  double Mc, eta, m1, m2;
  double distance;
  double inclination, dec;
  double polarization, phase, t, ra;
  double logL = 0.0;
  long nseg;
  double nu;

  LALInferenceIFOData * data = (*(LALInferenceIFOData **)Data_custom_val(IFOData));
  LALInferenceIFOData *currentData = data;

  LIparams.dimension = 0;
  LIparams.head = NULL;

  option = Field(options, 0);
  nseg = Long_val(option);

  nu = 4.0 / M_PI * nseg;
  currentData = data;
  while (currentData != NULL) {
    const size_t LEN = 64; /* Comes from LALInferenceLikelihood.c */
    char dofname[LEN];
    snprintf(dofname, LEN, "df_%s", currentData->name);
    LALInferenceAddVariable(&LIparams, dofname, &nu, LALINFERENCE_REAL8_t, LALINFERENCE_PARAM_FIXED);
    currentData = currentData->next;
  }

  /* Extract the non-spinning parameters. */
  nsparams = Field(params, 0);

  /* Masses. */
  m1 = Double_field(nsparams, 0);
  m2 = Double_field(nsparams, 1);
  eta = m1*m2/(m1+m2)/(m1+m2);
  Mc = (m1+m2)*pow(eta, 3.0/5.0);

  LALInferenceAddVariable(&LIparams, "chirpmass", &Mc, LALINFERENCE_REAL8_t, LALINFERENCE_PARAM_LINEAR);
  LALInferenceAddVariable(&LIparams, "massratio", &eta, LALINFERENCE_REAL8_t, LALINFERENCE_PARAM_LINEAR);

  distance = Double_field(nsparams, 2);
  LALInferenceAddVariable(&LIparams, "distance", &distance, LALINFERENCE_REAL8_t, LALINFERENCE_PARAM_LINEAR);
  
  inclination = acos(Double_field(nsparams,3));
  LALInferenceAddVariable(&LIparams, "inclination", &inclination, LALINFERENCE_REAL8_t, LALINFERENCE_PARAM_LINEAR);

  polarization = Double_field(nsparams, 4);
  LALInferenceAddVariable(&LIparams, "polarisation", &polarization, LALINFERENCE_REAL8_t, LALINFERENCE_PARAM_CIRCULAR);

  phase = Double_field(nsparams, 5);
  LALInferenceAddVariable(&LIparams, "phase", &phase, LALINFERENCE_REAL8_t, LALINFERENCE_PARAM_CIRCULAR);

  t = Double_field(nsparams, 6);
  LALInferenceAddVariable(&LIparams, "time", &t, LALINFERENCE_REAL8_t, LALINFERENCE_PARAM_LINEAR);

  ra = Double_field(nsparams, 7);
  LALInferenceAddVariable(&LIparams, "rightascension", &ra, LALINFERENCE_REAL8_t, LALINFERENCE_PARAM_CIRCULAR);
  
  dec = asin(Double_field(nsparams, 8));
  LALInferenceAddVariable(&LIparams, "declination", &dec, LALINFERENCE_REAL8_t, LALINFERENCE_PARAM_LINEAR);

  if (Tag_val(params) == 0) {
    /* Non-spinning parameters.  Run with TaylorF2 template. */
    LALPNOrder PhaseOrder=LAL_PNORDER_THREE_POINT_FIVE;
    Approximant approx = TaylorF2;

    LALInferenceAddVariable(&LIparams, "LAL_PNORDER", &PhaseOrder, LALINFERENCE_UINT4_t, LALINFERENCE_PARAM_FIXED);
    LALInferenceAddVariable(&LIparams, "LAL_APPROXIMANT", &approx, LALINFERENCE_UINT4_t, LALINFERENCE_PARAM_FIXED);

    logL = LALInferenceFreqDomainStudentTLogLikelihood(&LIparams, data, &LALInferenceTemplateLAL);
  } else {
    fprintf(stderr, "ERROR: spinning templates not implemented yet!\n");
    exit(1);
  }

  vlogL = caml_copy_double(logL);

  LALInferenceDestroyVariables(&LIparams);

  CAMLreturn(vlogL);
}
