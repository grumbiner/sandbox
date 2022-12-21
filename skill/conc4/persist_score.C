#include "scoring_subs.C"

int main(int argc, char *argv[]) {
  FILE *fobs, *fmodel, *fskip;
  global_12th<float> obsd, model, edge;
  global_12th<unsigned char> skip, nh, sh, ak, near_edge;
  MODEL<float> ice_conc;

  float mean, rms, pod, far, fcr, correct;
  double a11, a12, a21, a22;

  ijpt loc;
  int i, j, step;

// Get the observed ice concentrations:
  fobs = fopen(argv[1], "r");
  obsd.binin(fobs);
  fclose(fobs);
  printf("obsvd stats %f %f %f %f\n",obsd.gridmax(), obsd.gridmin(), obsd.average(), obsd.rms() );

// Open up the file with running model output:
  fmodel = fopen(argv[2], "r");

// Read in the file of flagged points to skip:
  fskip = fopen(argv[3], "r");
  skip.binin(fskip);
  fclose(fskip);

// preskip points that won't be scored in limited area:
  preskip(obsd, skip);

// for the persistence, we've got everything in one input, so score each:
  float landval = 1.57;
    ice_conc.binin(fmodel);
    for (loc.j = 0; loc.j < ice_conc.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < ice_conc.xpoints(); loc.i++) {
      if (ice_conc[loc] > 1.28) ice_conc[loc] = landval;
      if (ice_conc[loc] < 1.28 && ice_conc[loc] > 1.00) ice_conc[loc] = 1.00;
    }
    }
  model = ice_conc; // observed grid === model grid for persistence


  for (j = 0; j < model.xpoints()*model.ypoints(); j++) {
    if (model[j] < 0.15) model[j] = 0.0; // filter low concentrations as analysis does
  }
  printf("model stats %f %f %f %f\n",
      (float)model.gridmax(), (float)model.gridmin(), (float)model.average(), (float)model.rms() );
  preskip(model, skip);

//Newer: Regionalized scoring (n.b: This is general for all models)
  step = atoi(argv[4]);

  nh = skip;
  sh = skip;
  ak = skip;
  findedge(obsd, model, edge, near_edge);
  northern(nh);
  southern(sh);
  alaska(ak);

  scoring(obsd, model, skip, mean, rms, a11, a12, a21, a22, pod, far, correct);
  fcr = a21 / (a22+a21);
  printf("%02d scores %s %6.3f %6.3f %6.3f %6.3f %6.3f %6.3f\n",step, "glob", mean, rms, pod, far, fcr, correct);
  scoring(obsd, model, nh, mean, rms, a11, a12, a21, a22, pod, far, correct);
  fcr = a21 / (a22+a21);
  printf("%02d scores %s %6.3f %6.3f %6.3f %6.3f %6.3f %6.3f\n",step, "nh  ", mean, rms, pod, far, fcr, correct);
  scoring(obsd, model, sh, mean, rms, a11, a12, a21, a22, pod, far, correct);
  fcr = a21 / (a22+a21);
  printf("%02d scores %s %6.3f %6.3f %6.3f %6.3f %6.3f %6.3f\n",step, "sh  ", mean, rms, pod, far, fcr, correct);
  scoring(obsd, model, ak, mean, rms, a11, a12, a21, a22, pod, far, correct);
  fcr = a21 / (a22+a21);
  printf("%02d scores %s %6.3f %6.3f %6.3f %6.3f %6.3f %6.3f\n",step, "ak  ", mean, rms, pod, far, fcr, correct);
  scoring(obsd, model, near_edge, mean, rms, a11, a12, a21, a22, pod, far, correct);
  fcr = a21 / (a22+a21);
  printf("%02d scores %s %6.3f %6.3f %6.3f %6.3f %6.3f %6.3f\n",step, "edge", mean, rms, pod, far, fcr, correct);

  printf("\n");

  return 0;
}
