#include "scoring_subs.C"

int main(int argc, char *argv[]) {
  FILE *fobs, *fmodel, *fskip;
  global_12th<float> obsd, model, edge;
  global_12th<unsigned char> skip, nh, sh, ak, near_edge;
  MODEL<float> ice_conc, ice_thick, ice_temp, snow_temp, snow_thick;
  MODEL<float> ice_u, ice_v, e11, e12, e22;

  MODEL<float> tmp, conc, obs_conc_model;
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
// Preskip in observations:
  preskip(obsd, skip);

  step = atoi(argv[4]);
  base = atoi(argv[5]);

  for (i = 0; i < step; i++) {
    ice_conc.ftnin(fmodel);
    //printf("kiss ice_conc stats %d %f %f %f %f\n",i,
    //       ice_conc.gridmax(), ice_conc.gridmin(), ice_conc.average(), ice_conc.rms() );
    ice_thick.ftnin(fmodel);
    ice_temp.ftnin(fmodel);
    snow_temp.ftnin(fmodel);
    snow_thick.ftnin(fmodel);
    ice_u.ftnin(fmodel);
    ice_v.ftnin(fmodel);
    e11.ftnin(fmodel);
    e12.ftnin(fmodel);
    e22.ftnin(fmodel);
  }

  // interpolate from model grid to analysis grid:
  float landval = 1.57, nonval = 0.0;
  for (loc.j = 0; loc.j < ice_conc.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < ice_conc.xpoints(); loc.i++) {
    if (ice_conc[loc] > 100) ice_conc[loc] = landval;
  }
  }
  model.fromall(ice_conc, landval, nonval);
  for (i = 0; i < model.xpoints()*model.ypoints(); i++) {
    if (model[i] < 0.15) model[i] = 0; // filter low concentrations as analysis does
  }


//Newer: Regionalized scoring (n.b: This is nearly general for all models)
  nh = skip;
  sh = skip;
  ak = skip;
  findedge(obsd, model, edge, near_edge);
  northern(nh);
  southern(sh);
  alaska(ak);

  scoring(obsd, model, skip, mean, rms, a11, a12, a21, a22, pod, far, correct);
  fcr = a21 / (a22+a21);
  printf("%02d scores %s %6.3f %6.3f %6.3f %6.3f %6.3f %6.3f\n",base+step/4, "glob", mean, rms, pod, far, fcr, correct);
  scoring(obsd, model, nh, mean, rms, a11, a12, a21, a22, pod, far, correct);
  fcr = a21 / (a22+a21);
  printf("%02d scores %s %6.3f %6.3f %6.3f %6.3f %6.3f %6.3f\n",base+step/4, "nh  ", mean, rms, pod, far, fcr, correct);
  scoring(obsd, model, sh, mean, rms, a11, a12, a21, a22, pod, far, correct);
  fcr = a21 / (a22+a21);
  printf("%02d scores %s %6.3f %6.3f %6.3f %6.3f %6.3f %6.3f\n",base+step/4, "sh  ", mean, rms, pod, far, fcr, correct);
  scoring(obsd, model, ak, mean, rms, a11, a12, a21, a22, pod, far, correct);
  fcr = a21 / (a22+a21);
  printf("%02d scores %s %6.3f %6.3f %6.3f %6.3f %6.3f %6.3f\n",base+step/4, "ak  ", mean, rms, pod, far, fcr, correct);
  scoring(obsd, model, near_edge, mean, rms, a11, a12, a21, a22, pod, far, correct);
  fcr = a21 / (a22+a21);
  printf("%02d scores %s %6.3f %6.3f %6.3f %6.3f %6.3f %6.3f\n",base+step/4, "edge", mean, rms, pod, far, fcr, correct);

  printf("\n");
  return 0;
}
