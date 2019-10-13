#include "ncepgrids.h"


// version for gfs fake inputs

// do the verification on the analysis grid:
void scoring(global_12th<float> &obs, global_12th<float> &model, 
              float &mean, float &rms, double &a11, double &a12, double &a21, double &a22) ;

void scoring(global_12th<float> &obs, global_12th<float> &model, global_12th<unsigned char> &skip,
              float &mean, float &rms, double &a11, double &a12, double &a21, double &a22) ;

int main(int argc, char *argv[]) {
  FILE *fobs, *fmodel, *fskip;
  global_12th<float> obsd_conc_native, model;
  global_12th<unsigned char> skip;
  global_quarter<float> ice_conc, ice_thick, mask;

  float mean, rms, pod, far, fcr, correct;
  double a11, a12, a21, a22;

  latpt ll;
  ijpt loc;
  int i, step;
  float landval = 157., nonval = -1.0;

// Get the observed ice concentrations:
  fobs = fopen(argv[1], "r");
  obsd_conc_native.binin(fobs);
  fclose(fobs);
  //debug printf("observed conc max, min, avg %f %f %f %f\n",
  //debug obsd_conc_native.gridmax(), obsd_conc_native.gridmin(), 
  //debug       obsd_conc_native.average(), obsd_conc_native.rms() );

// Open up the file with running model output:
  fmodel = fopen(argv[2], "r");


// Read in the file of flagged points to skip:
  fskip = fopen(argv[3], "r");
  skip.binin(fskip);
  fclose(fskip);

  step = atoi(argv[4]);
  for (i = 0; i < step; i++) {
    ice_conc.binin(fmodel);
    //debug printf("gfs conc  stats %f %f %f %f\n",ice_conc.gridmax(), ice_conc.gridmin(), ice_conc.average(), ice_conc.rms() );
    ice_thick.binin(fmodel);
    //debug printf("gfs thick stats %f %f %f %f\n",ice_thick.gridmax(), ice_thick.gridmin(), ice_thick.average(), ice_thick.rms() );
  }
  
  // interpolate from model grid to analysis grid:
  for (loc.j = 0; loc.j < ice_conc.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < ice_conc.xpoints(); loc.i++) {
    if (ice_conc[loc] > 150.) {
      mask[loc] = nonval;
      ice_conc[loc] = 0.;
    }
    else {
      mask[loc] = 0.;
      //if (ice_conc[loc] > 0) { printf("%d %d  %f\n",loc.i, loc.j, ice_conc[loc] ); }
    }
  }
  }

  //debug printf("gfs input2 stats %f %f %f %f\n",ice_conc.gridmax(), ice_conc.gridmin(), ice_conc.average(), ice_conc.rms() );
  model.fromall(ice_conc, mask, landval, nonval);
  int count = 0;
  for (loc.j = 0; loc.j < model.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < model.xpoints(); loc.i++) {
    if (model[loc] == nonval) {
      skip[loc] = 1;
      count += 1;
    }
  }
  } 
  //printf("nonval skipping %d points\n",count);
  //debug printf("gfs interp stats %f %f %f %f\n",model.gridmax(), model.gridmin(), model.average(), model.rms() );

  // Score for whole globe:
  scoring(obsd_conc_native, model, mean, rms, a11, a12, a21, a22);
  pod = a11 / (a11+a21);
  far = a12 / (a11+a12);
  fcr = a21 / (a22+a21);
  correct = (a11 + a22) / (a11+a12+a21+a22);

  printf("day %02d scores %f %f  %f %f %f %f  %f %f %f %f  ",step/4, mean, rms, (float) a11, (float) a12, (float) a21, (float) a22, pod, far, fcr, correct);

  // Score only points not flagged to be 'skipped'
  scoring(obsd_conc_native, model, skip, mean, rms, a11, a12, a21, a22);
  pod = a11 / (a11+a21);
  far = a12 / (a11+a12);
  fcr = a21 / (a22+a21);
  correct = (a11 + a22) / (a11+a12+a21+a22);

  printf(" %f %f  %f %f %f %f  %f %f %f %f\n",mean, rms, (float) a11,(float)  a12,(float)  a21,(float)  a22, pod, far, fcr, correct);

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

  //Debug printf("obs model max min %f %f  %f %f\n",obs.gridmax(), obs.gridmin(),
  //Debug         model.gridmax(), model.gridmin() );
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

  //Debug printf("obs model max min %f %f  %f %f\n",obs.gridmax(), obs.gridmin(),
  //Debug         model.gridmax(), model.gridmin() );
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
  //printf("after skip = %f\n",globe / 1e12);
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
