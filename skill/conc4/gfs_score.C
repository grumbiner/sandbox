#include "scoring_subs.C"

int main(int argc, char *argv[]) {
  FILE *fobs, *fmodel, *fskip;
  global_12th<float> obsd, model, edge;
  global_12th<unsigned char> skip, nh, sh, ak, near_edge;
  MODEL<float> ice_conc, ice_thick, ice_temp, snow_temp, snow_thick;
  MODEL<float> ice_u, ice_v, tmix, smix;

  MODEL<float> tmp, conc, obs_conc_model;
  float mean, rms, pod, far, fcr, correct;
  double a11, a12, a21, a22;

  ijpt loc;
  int i, step;

// Get the observed ice concentrations:
  fobs = fopen(argv[1], "r");
  obsd.binin(fobs);
  fclose(fobs);

// Open up the file with running model output:
  fmodel = fopen(argv[2], "r");


// Read in the file of flagged points to skip:
  fskip = fopen(argv[3], "r");
  skip.binin(fskip);
  fclose(fskip);

// preskip points that won't be scored in limited area:
  preskip(obsd, skip);

  step = atoi(argv[4]);
  for (i = 0; i < step; i++) {
    ice_conc.binin(fmodel);
    ice_thick.binin(fmodel);
  }
  for (loc.j = 0; loc.j < ice_conc.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < ice_conc.xpoints(); loc.i++) {
    if (ice_conc[loc] > 100) ice_conc[loc] = 157.;
  }
  }

  // interpolate from model grid to analysis grid:
  float landval = 157., nonval = 0.0;
  model.fromall(ice_conc, landval, nonval);

  for (i = 0; i < model.xpoints()*model.ypoints(); i++) {
    if (model[i] < 0.15) model[i] = 0.0; // filter low concentrations as analysis does
  }

  // Score for whole globe:
  //scoring(obsd, model, mean, rms, a11, a12, a21, a22, pod, far, correct);
  //fcr = a21 / (a22+a21);
  //printf("%02d scores %f %f  %f %f %f %f  %f %f %f %f  ",step/4, mean, rms, (float) a11, (float) a12, (float) a21, (float) a22, pod, far, fcr, correct);

  // Score only points not flagged to be 'skipped'
  //scoring(obsd, model, skip, mean, rms, a11, a12, a21, a22, pod, far, correct);
  //fcr = a21 / (a22+a21);
  //printf(" %f %f  %f %f %f %f  %f %f %f %f\n",mean, rms, (float) a11,(float)  a12,(float)  a21,(float)  a22, pod, far, fcr, correct);


// New regionalized scoring:
  nh = skip;
  sh = skip;
  ak = skip;
  findedge(obsd, model, edge, near_edge);
  northern(nh);
  southern(sh);
  alaska(ak);

  scoring(obsd, model, skip, mean, rms, a11, a12, a21, a22, pod, far, correct);
  fcr = a21 / (a22+a21);
  printf("%02d scores %s %6.3f %6.3f %6.3f %6.3f %6.3f %6.3f\n",step/4, "glob", mean, rms, pod, far, fcr, correct);
  scoring(obsd, model, nh, mean, rms, a11, a12, a21, a22, pod, far, correct);
  fcr = a21 / (a22+a21);
  printf("%02d scores %s %6.3f %6.3f %6.3f %6.3f %6.3f %6.3f\n",step/4, "nh  ", mean, rms, pod, far, fcr, correct);
  scoring(obsd, model, sh, mean, rms, a11, a12, a21, a22, pod, far, correct);
  fcr = a21 / (a22+a21);
  printf("%02d scores %s %6.3f %6.3f %6.3f %6.3f %6.3f %6.3f\n",step/4, "sh  ", mean, rms, pod, far, fcr, correct);
  scoring(obsd, model, ak, mean, rms, a11, a12, a21, a22, pod, far, correct);
  fcr = a21 / (a22+a21);
  printf("%02d scores %s %6.3f %6.3f %6.3f %6.3f %6.3f %6.3f\n",step/4, "ak  ", mean, rms, pod, far, fcr, correct);
  scoring(obsd, model, near_edge, mean, rms, a11, a12, a21, a22, pod, far, correct);
  fcr = a21 / (a22+a21);
  printf("%02d scores %s %6.3f %6.3f %6.3f %6.3f %6.3f %6.3f\n",step/4, "edge", mean, rms, pod, far, fcr, correct);

  printf("\n");

  palette<unsigned char> h(2);
  h.set_color(0, 0, 0, 0);
  h.set_color(1, 255, 255, 255);
  nh.xpm("nh.xpm",1,h);
  sh.xpm("sh.xpm",1,h);
  skip.xpm("skip.xpm",1,h);
  ak.xpm("ak.xpm",1,h);
  near_edge.xpm("near_edge.xpm",1,h);

  palette<unsigned char> gg(19,65);
  obsd *= 100.;
  obsd.xpm("obsd.xpm",7,gg);
  model *= 100.;
  model.xpm("model.xpm",7,gg);

  return 0;
}
