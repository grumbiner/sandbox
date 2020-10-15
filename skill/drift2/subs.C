#include <cmath>
#include <cstdio>
#include <cstring>

// 2 April 2014  Robert Grumbine


#define nskile 207
#define ndays   16
#define STRMAX 900
void getfcst(int &date, FILE *fin, float *dir, float *dist, int &code) ;


void detrnd(float *fdist, const float b0, const float b1, const int n);
void rms(float *odist, float *odir, float *fdist, float *fdir, int npts,
         float &meandist, float &meandir, float &rmsdist, float &rmsdir,
         float &errad, float &erradrms) ;
float delta_direction(float x, float y) ;
void dirfix(float *x, float y, int n) ;


void detrnd(float *fdist, const float b0, const float b1, const int count) {
  int i;
  for (i = 0; i < count; i++) {
    fdist[i] = b0 + b1 * fdist[i];
  }
  return;
}
void rms(float *odist, float *odir, float *fdist, float *fdir, int npts,
         float &meandist, float &meandir, float &rmsdist, float &rmsdir, float &errad, float &erradrms) {
  int i;
  float deldir, dx, dy;

  meandist = 0;
  rmsdist  = 0;
  for (i = 0; i < npts; i++) {
    meandist += odist[i] - fdist[i];
    rmsdist  += (odist[i] - fdist[i])*(odist[i] - fdist[i]);
  }
  meandist /= (float) npts;
  rmsdist  = sqrt(rmsdist/(float) npts);

  meandir = 0.;
  rmsdir  = 0.;
  errad   = 0.;
  erradrms  = 0.;
  for (i = 0; i < npts; i++) {
     deldir = delta_direction(odir[i], fdir[i]);
     meandir += deldir;
     rmsdir  += deldir*deldir;
     dx = (odist[i] - fdist[i])*cos(deldir*M_PI/180.);
     dy = (odist[i] - fdist[i])*cos(deldir*M_PI/180.);
     errad += sqrt(dx*dx + dy*dy);
     erradrms += (dx*dx + dy*dy);
  }
  meandir /= (float) npts;
  rmsdir   = sqrt(rmsdir/(float) npts);
  errad   /= (float) npts;
  erradrms = sqrt(erradrms/(float) npts);

  return;
}
float delta_direction(float x, float y) {
  float tmp;
  tmp = x - y;
  if (tmp > 180) {
    tmp = tmp - 360.;
  }
  if (tmp < -180) {
    tmp = 360 + tmp;
  }
  //printf("x, y, tmp %f %f %f\n", x, y, tmp);
  return tmp;
}
void dirfix(float *x, float y, int n) {
  int i;
  for (i = 0; i < n; i++) {
    x[i] -= y;
    if (x[i] > 360.) x[i] -= 360.;
    if (x[i] < 0.)   x[i] += 360.;
  }
  return;
}

/* C++ language variant finally, 2005/03/07 */
void getfcst(int &date, FILE *fin, float *dir, float *dist, int &code) {

  int i, j, skpt2;
  float lat, longit, t1, t2;

  char header[STRMAX], trailer[STRMAX];

  for (i = 0; i < ndays; i++) {
    fgets(header,STRMAX,fin);
    fgets(header,STRMAX,fin);
    fgets(header,STRMAX,fin);
    fgets(header,STRMAX,fin);
    fgets(header,STRMAX,fin);

    for (j = 0; j < nskile; j++) {
      fscanf(fin,"%d %f %f",&skpt2, &dir[j+nskile*i], &dist[j+nskile*i]);
      //printf("day %d i %3d skpt %3d\n",i+1,j+1,skpt2);
    }
    fgets(header,STRMAX,fin);
    
    j = 0;
    do {
      fgets(header,STRMAX,fin);
      sscanf(header,"%d %f %f %f %f",&skpt2, &longit, &lat, &t1, &t2);
      //printf("edge %d %f %f %f %f\n",skpt2, longit, lat, t1, t2);
      j += 1;
    }
    while (!feof(fin) && strlen(header) > 28);

    fscanf(fin, "%s",trailer);
    fscanf(fin, "%s",trailer);
  }

  code = ndays;

  return;
}

