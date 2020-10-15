#include "ncepgrids.h"

#define MODEL gaussian

// do the verification on the analysis grid:
void scoring(global_12th<float> &obs, global_12th<float> &model, 
              float &mean, float &rms, double &a11, double &a12, double &a21, double &a22) ;

void scoring(global_12th<float> &obs, global_12th<float> &model, global_12th<unsigned char> &skip,
              float &mean, float &rms, double &a11, double &a12, double &a21, double &a22) ;

int main(int argc, char *argv[]) {
  FILE *fobs, *fmodel, *fskip;
  global_12th<float> obsd, model;
  global_12th<unsigned char> skip;
  MODEL<float> mask, ice_conc, ice_thick, ice_temp, snow_temp, snow_thick;
  MODEL<float> ice_u, ice_v, tmix, smix;

  MODEL<float> tmp, conc;
  MODEL<float> e11, e12, e22;
  float mean, rms, pod, far, fcr, correct;
  double a11, a12, a21, a22;

  latpt ll;
  ijpt loc;
  int i, step, base;

// Get the observed ice concentrations:
  fobs = fopen(argv[1], "r");
  if (fobs == (FILE *) NULL) {
    printf("failed to open observation input %s\n",argv[1]);
    return 1;
  }

  obsd.binin(fobs);
  fclose(fobs);

// Open up the file with running model output:
  fmodel = fopen(argv[2], "r");
  if (fmodel == (FILE *) NULL) {
    printf("failed to open model input %s\n",argv[2]);
    return 1;
  }


// Read in the file of flagged points to skip:
  fskip = fopen(argv[3], "r");
  if (fskip == (FILE *) NULL) {
    printf("failed to open skipfile input %s\n",argv[3]);
    return 1;
  }
  skip.binin(fskip);
  fclose(fskip);

  step = atoi(argv[4]);
  base = atoi(argv[5]);

  for (i = 0; i < step; i++) {
    ice_conc.ftnin(fmodel);
    ice_thick.ftnin(fmodel);
    ice_temp.ftnin(fmodel);
    snow_temp.ftnin(fmodel);
    snow_thick.ftnin(fmodel);
    ice_u.ftnin(fmodel);
    ice_v.ftnin(fmodel);
    tmix.ftnin(fmodel);
    smix.ftnin(fmodel);
    e11.ftnin(fmodel);
    e12.ftnin(fmodel);
    e22.ftnin(fmodel);
  }

  // interpolate from model grid to analysis grid:
  float landval = 900., nonval = -1.0;
  for (loc.j = 0; loc.j < ice_conc.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < ice_conc.xpoints(); loc.i++) {
    if (fabs(ice_conc[loc]) > landval) {
      mask[loc] = landval;
    }
    else {
      mask[loc] = -9.0;
    }
  }
  }

  model.fromall(ice_conc, mask, landval, nonval);

  // Score for whole globe:
  scoring(obsd, model, mean, rms, a11, a12, a21, a22);
  pod = a11 / (a11+a21);
  far = a12 / (a11+a12);
  fcr = a21 / (a22+a21);
  correct = (a11 + a22) / (a11+a12+a21+a22);
  printf("day %02d scores %f %f  %f %f %f %f  %f %f %f %f  ",step/4+base, mean, rms, (float) a11, (float) a12, (float) a21, (float) a22,
     pod, far, fcr, correct);

  // Score only points not flagged to be 'skipped'
  scoring(obsd, model, skip, mean, rms, a11, a12, a21, a22);
  pod = a11 / (a11+a21);
  far = a12 / (a11+a12);
  fcr = a21 / (a22+a21);
  correct = (a11 + a22) / (a11+a12+a21+a22);
  printf(" %f %f  %f %f %f %f  %f %f %f %f\n",mean, rms, (float) a11,(float)  a12,(float)  a21,(float)  a22,
     pod, far, fcr, correct);

  return 0;
}
void scoring(global_12th<float> &obs, global_12th<float> &model, 
              float &mean, float &rms, double &a11, double &a12, double &a21, double &a22) {
  global_12th<float> delta;
  int i;
  int count = 0;
  ijpt loc;
  double globe = 0.0, level = 0.0;

  a11 = 0; a12 = 0; a21 = 0; a22 = 0;

  delta = obs;
  delta -= model;
  mean = 0.0;
  rms  = 0.0;
  for (loc.j = 0; loc.j < model.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < model.xpoints(); loc.i++) {
    globe += delta.cellarea(loc);
    mean += delta[loc]*delta.cellarea(loc);
    rms  += delta[loc]*delta[loc]*delta.cellarea(loc);
  }
  }
  mean /= globe;
  rms   = sqrt(rms/globe); 

  for (loc.j = 0; loc.j < model.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < model.xpoints(); loc.i++) {
    if (model[loc] > level ) {
      if (obs[loc] > level ) {
        a11 += model.cellarea(loc);
      }
      else {
        a12 += model.cellarea(loc);
      }
    }
    else {
      if (obs[loc] > level ) {
        a21 += model.cellarea(loc);
      }
      else {
        a22 += model.cellarea(loc);
      }
    }
  }
  }

  a11 /= globe;
  a12 /= globe;
  a21 /= globe;
  a22 /= globe;

// pod = probability of detection (said there was ice, and there is)
// far = false alarm rate (said there was ice, but there wasn't)
  double pod, far;
  pod = (double) a11 / (double) (a11 + a21 + a12);
  far = (double) a12 / (double) (a11 + a21 + a12);

  return;
}
void scoring(global_12th<float> &obs, global_12th<float> &model, global_12th<unsigned char> &skip, 
              float &mean, float &rms, double &a11, double &a12, double &a21, double &a22) {
  global_12th<float> delta;
  int i;
  int count = 0;
  ijpt loc;
  double globe = 0.0, level = 0.0;

  a11 = 0; a12 = 0; a21 = 0; a22 = 0;

  delta = obs;
  delta -= model;
  mean = 0.0;
  rms  = 0.0;
  for (loc.j = 0; loc.j < model.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < model.xpoints(); loc.i++) {
    if (skip[loc] == 0) {
      globe += delta.cellarea(loc);
      mean += delta[loc]*delta.cellarea(loc);
      rms  += delta[loc]*delta[loc]*delta.cellarea(loc);
    }
  }
  }
  //printf("after skip = %f  ",globe / 1e12);
  mean /= globe;
  rms   = sqrt(rms/globe); 

  for (loc.j = 0; loc.j < model.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < model.xpoints(); loc.i++) {
  if (skip[loc] == 0) {
    if (model[loc] > level ) {
      if (obs[loc] > level ) {
        a11 += model.cellarea(loc);
      }
      else {
        a12 += model.cellarea(loc);
      }
    }
    else {
      if (obs[loc] > level ) {
        a21 += model.cellarea(loc);
      }
      else {
        a22 += model.cellarea(loc);
      }
    }
  }
  }
  }

  a11 /= globe;
  a12 /= globe;
  a21 /= globe;
  a22 /= globe;

// pod = probability of detection (said there was ice, and there is)
// far = false alarm rate (said there was ice, but there wasn't)
  double pod, far;
  pod = (double) a11 / (double) (a11 + a21);
  far = (double) a12 / (double) (a11 + a12);

  return;
}
