#include "scoring_subs.C"

int main(int argc, char *argv[]) {
  FILE *fobs, *fmodel, *fskip;
  global_12th<float> obsd, model, edge;
  global_12th<unsigned char> skip, nh, sh, ak, near_edge;
  MODEL<float> aice, hi, hs, tice, u, v, sst, mask; 

  float mean, rms, pod, far, fcr, correct;
  double a11, a12, a21, a22;

  latpt ll;
  ijpt loc;
  int i, step;
  float landval , nonval = 0.0;

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

// Note that acnfs uses 1 day steps
  step = atoi(argv[4]);
  for (i = 0; i < step; i++) {
    aice.ftnin(fmodel);
    hi.ftnin(fmodel);
    hs.ftnin(fmodel);
    tice.ftnin(fmodel);
    u.ftnin(fmodel);
    v.ftnin(fmodel);
    sst.ftnin(fmodel);
  }

  landval = aice.gridmin();
  // interpolate from model grid to analysis grid:
  landval = 1.57;
  for (loc.j = 0; loc.j < aice.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < aice.xpoints(); loc.i++) {
    if (aice[loc] > 1.5 || aice[loc] < 0 ) {
      mask[loc] = landval;
      aice[loc] = landval;
    }
    else {
      mask[loc] = -0.1;
      if (aice[loc] < 0.15) aice[loc] = 0;
    }
  }
  }
  model.fromall(aice, mask, landval, nonval);

  // Score for whole globe:
  // // -- for the acnfs, score only the northern hemisphere
  for (loc.j = 0; loc.j < model.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < model.xpoints(); loc.i++) {
    ll = model.locate(loc);
    if (ll.lat < 40) {
      model[loc] = 0.0;
      obsd[loc]  = 0.0;
    }
  }
  }


// New, regionalized scoring:
  preskip(obsd,skip);
  preskip(model,skip);

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

// diagnostic output:
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
