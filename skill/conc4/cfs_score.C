#include "scoring_subs.C"

template<class T>
class cfs1deg : public llgrid<T> {
  public:
   cfs1deg(void); /* Construction creator */
   ~cfs1deg(void) { };
   cfs1deg(cfs1deg<T>&);
   cfs1deg<T> & operator=(cfs1deg<T> &);
};
template<class T> 
cfs1deg<T> & cfs1deg<T>::operator=(cfs1deg<T> &x) {
  int loc;
  this->nx = 360;
  this->ny = 181;
  this->cyclicx = (fabs(this->nx * this->dlon) >= 360.0);
  this->cyclicy = false;
  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) { cout << "Failed to new in cfs1deg()\n"; }
  this->dlat = 1.0;
  this->dlon = 1.0;
  this->firstlat = -90.0;
  this->firstlon =  0.0;
  this->pds.set_gridid(3);
//Future: copy the pds
  for (loc = 0; loc < this->ny*this->nx; loc++) {
     this->grid[loc] = x[loc];
  }
  return *this;
}
template<class T>
cfs1deg<T>::cfs1deg(cfs1deg<T> &x) {
  int loc;
  this->nx = 360;
  this->ny = 181;
  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) { cout << "Failed to new in cfs1deg()\n"; }
  this->dlat = 1.0;
  this->dlon = 1.0;
  this->firstlat = -90.0;
  this->firstlon =  0.0;
  this->pds.set_gridid(3);
//Future: copy the pds
  for (loc = 0; loc < this->ny*this->nx; loc++) {
     this->grid[loc] = x[loc];
  }
  this->cyclicx = (fabs(this->nx * this->dlon) >= 360.0);
  this->cyclicy = false;

}
template<class T>
cfs1deg<T>::cfs1deg(void) {
  this->nx = 360 ;
  this->ny = 181;
  this->grid = new T[this->nx*this->ny];
  if (this->grid == (T *) NULL) { cout << "Failed to new in cfs1deg()\n"; }
  this->dlat = 1.0;
  this->dlon = 1.0;
  this->firstlat = -90.0;
  this->firstlon =  0.0;
  this->pds.set_gridid(3);
  this->cyclicx = (fabs(this->nx * this->dlon) >= 360.0);
  this->cyclicy = false;
}


int main(int argc, char *argv[]) {
  FILE *fobs, *fmodel, *fskip;
  global_12th<float> obsd, model, edge;
  global_12th<unsigned char> skip, nh, sh, ak, near_edge;
  MODEL<float> ice_conc, ice_thick;
  MODEL<float> ice_u, ice_v;

  float mean, rms, pod, far, fcr, correct;
  double a11, a12, a21, a22;

  ijpt loc;
  int i, step;
  float concflag = 157, thickflag = 157;

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
// prefilter the observations
  preskip(obsd, skip);

  step = atoi(argv[4]);
  for (i = 0; i < step; i++) {
    ice_conc.ftnin(fmodel);
    concflag = ice_conc.gridmax();
    //printf("cfs conc maxes %f %f\n",concflag, ice_conc.gridmax(concflag));
    
    ice_thick.ftnin(fmodel);
    ice_u.ftnin(fmodel);
    ice_v.ftnin(fmodel);
  }

  if (ice_conc.gridmax() > 100)   concflag = ice_conc.gridmax();
  printf("input conc stats %f %f %f %f\n",ice_conc.gridmax(concflag), 
         ice_conc.gridmin(concflag), ice_conc.average(concflag), ice_conc.rms(concflag) );

  // interpolate from model grid to analysis grid:
  float landval = concflag, nonval = 0.0;

  model.fromall(ice_conc, landval, landval);
  for (i = 0; i < model.ypoints()*model.xpoints(); i++) {
    if (model[i] > 100) model[i] = nonval;
    if (model[i] < 0.15) model[i] = 0.0 ;  // filter out as observing system would
  }
  printf("intrp conc stats %f %f %f %f\n",model.gridmax(), model.gridmin(), model.average(), model.rms() );
  preskip(model,skip);

// Regional Scoring
  nh = skip;
  sh = skip;
  ak = skip;
  findedge(obsd, model, edge, near_edge);
  northern(nh);
  southern(sh);
  alaska(ak);

  scoring(obsd, model, skip, mean, rms, a11, a12, a21, a22, pod, far, correct);
  fcr = a21 / (a22+a21);
  printf("%02d scores %s %6.3f %6.3f %6.3f %6.3f %6.3f %6.3f\n",step/4, 
            "glob", mean, rms, pod, far, fcr, correct);
  scoring(obsd, model, nh, mean, rms, a11, a12, a21, a22, pod, far, correct);
  fcr = a21 / (a22+a21);
  printf("%02d scores %s %6.3f %6.3f %6.3f %6.3f %6.3f %6.3f\n",step/4, 
            "nh  ", mean, rms, pod, far, fcr, correct);
  scoring(obsd, model, sh, mean, rms, a11, a12, a21, a22, pod, far, correct);
  fcr = a21 / (a22+a21);
  printf("%02d scores %s %6.3f %6.3f %6.3f %6.3f %6.3f %6.3f\n",step/4, 
            "sh  ", mean, rms, pod, far, fcr, correct);
  scoring(obsd, model, ak, mean, rms, a11, a12, a21, a22, pod, far, correct);
  fcr = a21 / (a22+a21);
  printf("%02d scores %s %6.3f %6.3f %6.3f %6.3f %6.3f %6.3f\n",step/4, 
            "ak  ", mean, rms, pod, far, fcr, correct);
  scoring(obsd, model, near_edge, mean, rms, a11, a12, a21, a22, pod, far, correct);
  fcr = a21 / (a22+a21);
  printf("%02d scores %s %6.3f %6.3f %6.3f %6.3f %6.3f %6.3f\n",step/4, 
            "edge", mean, rms, pod, far, fcr, correct);

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
