#include <lal/LALInference.h>
#include <lal/LALInferenceLikelihood.h>
#include <lal/LALInferenceTemplate.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/custom.h>

static double dot(const double v[3], const double w[3]) {
  int i;
  double sum = 0.0;
  for (i = 0; i < 3; i++) {
    sum += v[i]*w[i];
  }

  return sum;
}

static double norm(const double v[3]) {
  return sqrt(dot(v,v));
}

static void scale(double vs[3], const double v[3], const double s) {
  int i;
  for (i = 0; i < 3; i++) {
    vs[i] = v[i]*s;
  }
}

static void sub(double s[3], const double v[3], const double w[3]) {
  int i;
  for (i = 0; i < 3; i++) {
    s[i] = v[i] - w[i];
  }
}

static void unit_vector(double hat[3], const double v[3]) {
  double n = norm(v);
  
  if (n == 0.0) {
    fprintf(stderr, "Trying to make a unit vector out of the zero-vector!\n");
    exit(1);
  }

  scale(hat, v, 1.0/n);
}

static void cross(double c[3], const double v[3], const double w[3]) {
  c[0] = v[1]*w[2] - v[2]*w[1];
  c[1] = v[2]*w[0] - v[0]*w[2];
  c[2] = v[0]*w[1] - v[1]*w[0];
}

static void project_along(double v[3], const double w[3], const double u[3]) {
  double uhat[3];
  double dp;

  unit_vector(uhat, u);
  dp = dot(w, uhat);

  scale(v, uhat, dp);  
}

static void perp_part(double v[3], const double w[3], const double u[3]) {
  double walongu[3];

  project_along(walongu, w, u);

  sub(v, w, walongu);
}

static void theta_phi_template(double *theta, double *phi, const double cos_i, const double cos_tilt, const double my_phi) {
  double L[3], N[3], myxhat[3], myyhat[3], myzhat[3], Nperp[3], s[3];
  double sin_i = sqrt(1.0 - cos_i*cos_i);
  double sin_tilt = sqrt(1.0 - cos_tilt*cos_tilt);
  int i;

  L[0] = sin_i;
  L[1] = 0.0;
  L[2] = cos_i;

  N[0] = 0.0;
  N[1] = 0.0;
  N[2] = 1.0;

  unit_vector(myzhat, L);
  
  perp_part(Nperp, N, L);
  unit_vector(myxhat, Nperp);

  cross(myyhat, myzhat, myxhat);

  for (i = 0; i < 3; i++) {
    s[i] = cos(my_phi)*sin_tilt*myxhat[i] + sin(my_phi)*sin_tilt*myyhat[i] + cos_tilt*myzhat[i];
  }

  *theta = atan2(sqrt(s[0]*s[0]+s[1]*s[1]), s[2]);
  *phi = atan2(s[1], s[0]);
}

CAMLprim value wrapLALInferenceFreqDomainStudentTLogLikelihood(value options, value IFOData, value params) {
  CAMLparam3(options, IFOData, params);
  CAMLlocal4(option, nsparams, sparams, vlogL);

  LALPNOrder PhaseOrder=LAL_PNORDER_THREE_POINT_FIVE;

  LALInferenceVariables LIparams;
  double Mc, eta, m1, m2;
  double distance;
  double inclination, cos_i, dec;
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
  
  cos_i = Double_field(nsparams, 3);
  inclination = acos(cos_i);
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

  LALInferenceAddVariable(&LIparams, "LAL_PNORDER", &PhaseOrder, LALINFERENCE_UINT4_t, LALINFERENCE_PARAM_FIXED);

  if (Tag_val(params) == 0) {
    /* Non-spinning parameters.  Run with TaylorF2 template. */
    Approximant approx = TaylorF2;

    LALInferenceAddVariable(&LIparams, "LAL_APPROXIMANT", &approx, LALINFERENCE_UINT4_t, LALINFERENCE_PARAM_FIXED);

    logL = LALInferenceFreqDomainStudentTLogLikelihood(&LIparams, data, &LALInferenceTemplateLAL);
  } else {
    double a1, a2, costilt1, costilt2, myphi1, myphi2, theta1, theta2, phi1, phi2;
    Approximant approx = SpinTaylorFrameless;

    LALInferenceAddVariable(&LIparams, "LAL_APPROXIMANT", &approx, LALINFERENCE_UINT4_t, LALINFERENCE_PARAM_FIXED);
    
    sparams = Field(params, 1);

    a1 = Double_field(sparams, 0);
    LALInferenceAddVariable(&LIparams, "a_spin1", &a1, LALINFERENCE_REAL8_t, LALINFERENCE_PARAM_LINEAR);

    a2 = Double_field(sparams, 1);
    LALInferenceAddVariable(&LIparams, "a_spin2", &a2, LALINFERENCE_REAL8_t, LALINFERENCE_PARAM_LINEAR);

    costilt1 = Double_field(sparams, 2);
    myphi1 = Double_field(sparams, 3);

    theta_phi_template(&theta1, &phi1, cos_i, costilt1, myphi1);
    LALInferenceAddVariable(&LIparams, "theta_spin1", &theta1, LALINFERENCE_REAL8_t, LALINFERENCE_PARAM_LINEAR);
    LALInferenceAddVariable(&LIparams, "phi_spin1", &phi1, LALINFERENCE_REAL8_t, LALINFERENCE_PARAM_CIRCULAR);

    costilt2 = Double_field(sparams, 4);
    myphi2 = Double_field(sparams, 5);
    
    theta_phi_template(&theta2, &phi2, cos_i, costilt2, myphi2);
    LALInferenceAddVariable(&LIparams, "theta_spin2", &theta2, LALINFERENCE_REAL8_t, LALINFERENCE_PARAM_LINEAR);
    LALInferenceAddVariable(&LIparams, "phi_spin2", &phi2, LALINFERENCE_REAL8_t, LALINFERENCE_PARAM_CIRCULAR);

    logL = LALInferenceFreqDomainStudentTLogLikelihood(&LIparams, data, &LALInferenceTemplateLALGenerateInspiral);
  }

  vlogL = caml_copy_double(logL);

  LALInferenceDestroyVariables(&LIparams);

  CAMLreturn(vlogL);
}
