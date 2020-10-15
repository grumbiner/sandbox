// Program to score sea ice forecasts

#include "ncepgrids.h"

void scorer(grid2<float> &fcst, grid2<float> &obs, grid2<float> &ref, 
            grid2<float> &land, mvector<float> &score) ;
float murphy(float *f, float *obs, float *ref, int n) ;

extern "C" float iagree_(float *r, float *x, int &n);
extern "C" float correl_(float *r, float *x, int &n, float &r2, float &xmean,
                        float &ymean, float &sig2x, float &sig2y);
extern "C" float sumx_(float *r, int &n);
extern "C" float sumx2_(float *r, int &n);


int main(int argc, char *argv[]) {
  southgrid<float> fcst, ref, obs, land;
  grid2<float> tmp(fcst.xpoints()/5, fcst.ypoints()/5);
  grid2<float> tmp2(fcst.xpoints(), fcst.ypoints());
  mvector<float> scores(10);
  FILE *ffin, *frefin, *fobsin, *flandin;
  int i, j, k;
  palette<unsigned char> gg(19,65);

  ffin = fopen(argv[1], "r");
  if (ffin == (FILE *) NULL ) {
    printf("Failed to open the fcst file\n");
    return 1;
  }
  fobsin = fopen(argv[2], "r");
  if (fobsin == (FILE *) NULL ) {
    printf("Failed to open the obs file\n");
    return 2;
  }
  frefin = fopen(argv[3], "r");
  if (frefin == (FILE *) NULL ) {
    printf("Failed to open the reference file\n");
    return 1;
  }
  flandin = fopen(argv[4],"r");
  if (flandin == (FILE *) NULL) {
    printf("Failed to open the reference file\n");
    return 1;
  }
  land.binin(flandin);


  tmp.binin(ffin);
  tmp2 = tmp.magnify(5);
  tmp2 *= 100.;
  //tmp2.xpm("fcst.xpm",14,gg);
  fcst.set(tmp2);

  obs.binin(fobsin);
  //if (obs.average() < 3.0) obs *= 100.;
  //obs.xpm("obs.xpm",14,gg);
  
  ref.binin(frefin);
  //if (ref.average() < 3.0) ref *= 100.;
  //ref.xpm("ref.xpm",14,gg);
  
  if (fcst.average() > 3.0) fcst /= 100.;
  if ( obs.average() > 3.0)  obs /= 100.;
  if ( ref.average() > 3.0)  ref /= 100.;

  scorer(fcst, obs, ref, land, scores);
  for (k = 0; k < scores.xpoints(); k++) {
    printf("%f ",scores[k]);
  }
  printf("\n");

  return 0;
}

void scorer(grid2<float> &fcst, grid2<float> &obs, grid2<float> &ref, 
            grid2<float> &land, mvector<float> &score) {
  float f[obs.xpoints()*obs.ypoints() ];
  float o[obs.xpoints()*obs.ypoints() ];
  float r[obs.xpoints()*obs.xpoints() ];
  float delta[obs.xpoints()*obs.ypoints() ];
  float delta1[obs.xpoints()*obs.ypoints() ];
  float delta2[obs.xpoints()*obs.ypoints() ];
  int count=0;
  int i; 
  int a11=0, a12=0, a21=0, a22=0;
  float level = 50./100.;
  float r2, xmean, ymean, sig2x, sig2y;

  for (i = 0; i < obs.xpoints()*obs.ypoints() ; i++) {
     if (( fcst[i] <= 128/100.) && 
         ( obs[i] <= 128/100.) &&
         ( ref[i] <= 128/100.) &&
         ( land[i] <= 1.0 )        )   {
       f[count]      = min(1.,fcst[i]);
       o[count]      = min(1.,obs[i]);
       r[count]      = min(1.,ref[i]);
       delta[count]  = fcst[i] - obs[i];
       delta1[count] = fcst[i] - ref[i];
       delta2[count] =  obs[i] - ref[i];
       count += 1;
     }
  }
  printf("count = %d\n",count);

// Score 0-3 are threat type scores.
  for (i = 0 ; i < count; i++) {
     if (f[i] > level) {
       if (o[i] > level) {
         a11 += 1;
       }
       else {
         a12 += 1;
       }
     }
     else {
       if (o[i] > level) {
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
// For a referenced forecast, compute fration of residual explained.
   score[4] = (iagree_(f,o,count) - iagree_(r,o,count) )/
                         iagree_(r,o,count) ;

// Score 5, 6, 7 are delta statistics:
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
   score[9] = murphy(f, o, r, count);

  return;
}
float murphy(float *f, float *obs, float *ref, int n) {
  int i;
  float s1 = 0, s2 = 0;
  for (i = 0; i < n; i++) {
     s1 += (f[i] - obs[i])*(f[i]-obs[i]);
     s2 += (ref[i] - obs[i])*(ref[i]-obs[i]);
  }
  return 1 - s1 / s2;
}
