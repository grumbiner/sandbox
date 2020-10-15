//summary of scoring program elements, aimed at scoring ice models
//  variant to go searching for analogs to present grid
//Robert Grumbine 14 June 2002


#include "ncepgrids.h"

float murphy(float *f, float *obs, float *ref, int n) ;

extern "C" float iagree_(float *r, float *x, int &n);
extern "C" float correl_(float *r, float *x, int &n, float &r2, float &xmean,
                        float &ymean, float &sig2x, float &sig2y);
extern "C" float sumx_(float *r, int &n);
extern "C" float sumx2_(float *r, int &n);

int main(int argc, char *argv[]) {
  global_ice<float> climo, fnull, forecast, obsd, land;
  FILE *cin, *nin, *fin, *oin, *lin;

  mvector<float> x(climo.xpoints()*climo.ypoints()), y(climo.xpoints()*climo.ypoints());
  mvector<float> ref(climo.xpoints()*climo.ypoints());
  float *r, *x1, *x2;
  float *delta, *delta1, *delta2;

  int a11 = 0, a12 = 0, a21 = 0, a22 = 0;
  float r2, xmean, ymean, sig2x, sig2y;

  float level = 0.15;
  int i, count;
  mvector<float> score(10);


// Read in climatology file
// Read in null forecaster
// Read in forecast field
// Read in observed ice concentrations
// Read in land mask
  cin = fopen(argv[1], "r");
  nin = fopen(argv[2], "r");
  fin = fopen(argv[3], "r");
  oin = fopen(argv[4], "r");
  lin = fopen(argv[5], "r");
  climo.binin(cin);
  fnull.binin(nin);
  forecast.binin(fin);
  obsd.binin(oin);
  land.binin(lin);

// Q: area-weighting (esp for llgrids)

  
// transfer to vectors:
  for (i = 0; i < x.xpoints(); i++) {
    x[i] = forecast[i];
    y[i] = obsd[i];
    ref[i] = climo[i];
  }
  delta  = new float[x.xpoints()];
  delta1 = new float[x.xpoints()];
  delta2 = new float[x.xpoints()];
  r      = new float[x.xpoints()];
  x1     = new float[x.xpoints()];
  x2     = new float[x.xpoints()];


// Rescale inputs to 0-1, and find deltas 
  for (i = 0; i < x.xpoints() ; i++) {
     if (( x[i] <= 128/100.) &&
         ( y[i] <= 128/100.) &&
         ( ref[i] <= 128/100.) ) {
       x1[count] = min(1.,x[i]);
       x2[count] = min(1.,y[i]);
       r[count]  = min(1.,ref[i]);
       delta[count] = x[i] - y[i];
       delta1[count] = x[i] - ref[i];
       delta2[count] = y[i] - ref[i];
       count += 1;
     }
  }


// Scoring:
// Score 0-3 are threat type scores: FAR, POD, Threat, Q
  for (i = 0 ; i < count; i++) { 
     if (x1[i] > level) {
       if (x2[i] > level) {
         a11 += 1;
       }
       else {
         a12 += 1;
       }
     }
     else {
       if (x2[i] > level) {
         a21 += 1;
       }
       else {
         a22 += 1;
       }
     }
  }
     score[0] = (float) a11 / (float) (a11 + a21 + a12);
     score[1] = (float) (a11*a22 - a21*a12)/ (float) (a11*a22 + 1);
     score[2] = (float) (a11+a22) - sqrt((a11+a22)*(a11+a22)-
                                             4.*(a11*a22-a12*a22)) /
                (float) (a11+a22) + sqrt((a11+a22)*(a11+a22)-
                                             4.*(a11*a22-a12*a22));
     score[3] = (float) (a11+a22-a12-a21)/(float) (a11+a12+a21+a22);

//Score 4 is the index of agreement:
     score[4] = iagree_(delta1, delta2, count);

// Score 5, 6, 7 are delta (fcst - obs) statistics:
   score[5] = sumx_(delta, count);
   score[6] = sumx2_(delta, count);
   score[7] = (count*score[6] - score[5]*score[5]) / (count*count - count);
   score[5] /= (float) count;
   score[6] = sqrt(score[6]/(float) count);
   score[7] = sqrt(score[7]); 

// score 8 is correlation (not squared):
   correl_(delta1, delta2, count, r2, xmean, ymean, sig2x, sig2y);
   score[8] = r2;

// score 9 is murphy skill score:
   score[9] = murphy(x1, x2, r, count);

}
// score 9 is murphy skill score:
float murphy(float *f, float *obs, float *ref, int n) {
  int i;
  float s1 = 0, s2 = 0;
  for (i = 0; i < n; i++) {
     s1 += (f[i] - obs[i])*(f[i]-obs[i]);
     s2 += (ref[i] - obs[i])*(ref[i]-obs[i]);
  }
  return 1 - s1 / s2;
}

