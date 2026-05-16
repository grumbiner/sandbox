#include "mvector.h"
// Robert Grumbine

// Forecast vs. Observed:
// mean, rms, ...
extern "C" float iagree_(float *r, float *x, int &n);
extern "C" float correl_(float *r, float *x, int &n, float &r2, float &xmean,
                        float &ymean, float &sig2x, float &sig2y);
extern "C" float sumx_(float *r, int &n);
extern "C" float sumx2_(float *r, int &n);
// add threats based on level (concentration, e.g.)
void threat_scores(mvector<float> &x, mvector<float> &y, int &a11, int &a12, int &a21, int &a22);
void derived_threats(int a11, int a12, int a21, int a22, float &pod, float &far, float &fcr, float &correct);
//
// Forecast vs. Observed, with masking of points
//
//
// Generating null forecasters
// reading sea ice full climatology -- regular and conditional
void get_climo(FILE *fin, date, regular, conditional, );  // nulls =  

//
//
// Forecast vs. Observed, with null forecaster
extern "C" float murphy(float *f, float *obs, float *ref, int n) ;

// data:
// land masks
// outside model domain masks
// 'skip' masks -- e.g., always too warm
// scoring domain boundaries, e.g., NH, SH, Alaska, Labrador, ...
//
