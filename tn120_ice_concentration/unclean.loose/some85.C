#include <stdio.h>
#include <math.h>

#include "ssmi.h"
#include "icessmi.h"
#include "ncepgrids.h"

#define MAXPTS 200000
#define MAXCONC 128
#define MINCONC  15

// Note that we're using the land to carry the mapping:
int adder(avgpoint *dat, metricgrid<unsigned char> &land, process_2 *x) ;
int candidacy(avgpoint *nconc, int nx, int ny, int nice, mvector<float> &cice,
       mvector<float>  *g85, mvector<float> *g37 );
int vects(unsigned int v, unsigned int h, mvector<float> &x) ;
void regress(mvector<float> &conc, mvector<float> *x, int nice, 
             ijpt *locations, GRIDTYPE<unsigned char> &outconc, int f) ;
void correl(mvector<float> &pred, mvector<float> &conc, float &r, float &a, float &b);
float dot(mvector<float> &x, mvector<float> &y) ;

int main(int argc, char *argv[]) {
  bufr_line line;
  GRIDTYPE<unsigned char> nland, outconc;
  avgpoint *nconc;
  mvector<float> g85[MAXPTS], g37[MAXPTS];
  ijpt locations[MAXPTS];
  mvector<float> cice, r, a, b;
  process_1  x;
  process_2  x2[4];
  FILE *fin, *fout;
  ijpt loc;
  int i, j, index, count=0;

  if (argc != 4 ) {
    printf("usage is prog data_file land_file output_file\n");
    return 1;
  }

  nconc = new avgpoint[nland.xpoints()*nland.ypoints() ];

  fin = fopen(argv[2],"r");
  if (fin == (FILE *) NULL) {
    printf("Failed to open the land file %s\n",argv[2]);
    return 2;
  }
  nland.binin(fin);
  fclose(fin);

  fout = fopen(argv[3],"w");
  if (fout == (FILE *) NULL) {
    printf("Failed to open the output file %s\n",argv[3]);
    return 4;
  }

  fin = fopen(argv[1], "r");
  if (fin == (FILE *) NULL) {
    printf("Failed to open the data file\n");
    return 3;
  }
  while (!feof(fin)) {
    fread(&line, sizeof(bufr_line), 1, fin);
    for (i = 0; i < NSCANS; i++) {
      toprocess_1(x, line.full[i]);
      toprocess_2(x2, x);
      count += adder(nconc, nland, x2);
    }
  }
  printf("Found %d points on grid\n",count);

// Initialize the output concentration field to NO_DATA
  outconc.set(224);
     
  for (loc.j = 0; loc.j < nland.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < nland.xpoints(); loc.i++) {
     index = loc.i+loc.j*nland.xpoints();
     nconc[index].average();
     outconc[loc] = nconc[index].aref;
  }
  }
  outconc.binout(fout);
  fclose(fout);
  
  count = 0;
  i = 0;
  for (loc.j = 0; loc.j < nland.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < nland.xpoints(); loc.i++) {
    i = loc.i + loc.j*nland.xpoints();
    if (nconc[i].aref >= (unsigned int) MINCONC && nconc[i].aref <= (unsigned int) MAXCONC) {
       locations[count].i = loc.i;
       locations[count].j = loc.j;
       count++;
    }
  }
  }
  printf("%d points on hi-res grid had ice\n",count); fflush(stdout);

  candidacy(nconc, nland.xpoints(), nland.ypoints(), count, cice, g85, g37 );
  r.resize(g85[0].xpoints());
  a.resize(g85[0].xpoints());
  b.resize(g85[0].xpoints());
  regress(cice, g85, count, locations, outconc, 85);
  regress(cice, g37, count, locations, outconc, 37);

  return 0;
}
int adder(avgpoint *dat, metricgrid<unsigned char> &land, process_2 *x) {
  ijpt loc;
  fijpt floc;
  latpt geoloc;
  int i, index, count=0;

  for (i = 0; i < 4; i++) {
    geoloc.lat = ((float) x[i].latitude/100. - 90.0);
    geoloc.lon = ((float) x[i].longitude )/100. - 180.;
    floc = land.locate(geoloc);
    if (land.in(floc) ) {
      loc.i = (int) (0.5 + floc.i);
      loc.j = (int) (0.5 + floc.j);
      // add to grid if not on land and not weather 
      // and not 'near' land -- to avoid regression on points that
      // are land-contaminated
      //if (land[loc] == 0 && (int) x[i].aref <= MAXCONC && 
      if (land.anyof(157, 5, loc) == 0 && (int) x[i].aref <= MAXCONC && 
                            (int) x[i].aref >= MINCONC) {
        count += 1;
        index = loc.i + loc.j*land.xpoints();
        dat[index].t19v += x[i].t19v;
        dat[index].t19h += x[i].t19h;
        dat[index].t22v += x[i].t22v;
        dat[index].t37v += x[i].t37v;
        dat[index].t37h += x[i].t37h;
        dat[index].t85v += x[i].t85v;
        dat[index].t85h += x[i].t85h;
        dat[index].count += 1;
        dat[index].aref  += x[i].aref ;
      }
    }
  }

  return count;
}
int candidacy(avgpoint *nconc, int nx, int ny, int nice, mvector<float> &cice,
       mvector<float>  *g85, mvector<float> *g37 ) {
  int i, index = 0;

  cice.resize(nice);

  for (i = 0; i < nx*ny; i++) {
    if (nconc[i].aref >= (unsigned int) MINCONC && nconc[i].aref <= (unsigned int) MAXCONC) {
      cice[index] = (float) nconc[i].aref / 100.;
      vects(nconc[i].t85v, nconc[i].t85h, g85[index]);
      vects(nconc[i].t37v, nconc[i].t37h, g37[index]);
      index += 1;
    }
  }
 
  return index;  
}
// Compute the parameters for a given pair of h, v values
int vects(unsigned int v, unsigned int h, mvector<float> &x) {
  float cross, pr;
  float rh, rv;

  rh = (float) h;
  rh /= 100.;
  rv = (float) v;
  rv /= 100.;

  pr = (rv - rh) / (rv + rh); 
  cross = (rv - rh)*(rv+rh);

  x.resize(15);  // Note the neat feature: the rest of the program can
                 // use the right sizes without a global declaration!
  x[0] = rh;
  x[1] = rv;
  x[2] = rv + rh;
  x[3] = rv - rh;
  x[4] = rh / rv;
  x[5] = cross;
  x[6] = pr;
  x[7] = x[3]*x[3];
  x[8] = x[4]*x[4];
  x[9] = cross*cross;
  x[10] = pr*pr;
  x[11] = sqrt(fabs(x[3]));
  x[12] = sqrt(fabs(x[4]));
  x[13] = sqrt(fabs(x[5]));
  x[14] = sqrt(fabs(x[6]));

  return (int) v;
}
void regress(mvector<float> &conc, mvector<float> *x, int nice, 
             ijpt *locations, GRIDTYPE<unsigned char> &outconc, int f) {
  mvector<float> pred(nice);
  int i, j;
  float a, b, r;
  FILE *fout;
  char fname[80];
  
  printf("nice, parms %d %d\n",nice, x[0].xpoints() );

  for (j = 0; j < x[0].xpoints(); j++) {
    for (i = 0; i < nice; i++) {
      pred[i] = (x[i])[j];
    }
    correl(pred, conc, r, a, b);
    printf("Parameter %2d correl %6.3f offset %f slope %f\n",j, r, a, b);
    if (fabs(r) >= 0.85 || j == 13 ) {
      // then print out the grid:
      outconc.set(NO_DATA);
      sprintf(fname,"parm%2d%02d",f, j);
      fout = fopen(fname, "w");
      for (i = 0; i < nice; i++) {
         // Note that we have to rescale for the binary grid:
         outconc[locations[i] ] = (unsigned char) (0.5 + a*100. + b*pred[i]*100.);
         if (outconc[locations[i] ] < (unsigned char) MINCONC) outconc[locations[i] ] = 0;
         if (outconc[locations[i] ] > (unsigned char) MAXCONC) outconc[locations[i] ] = 0;

      }
      outconc.binout(fout);
      fclose(fout);
    }
      
  }

  return ;
}
void correl(mvector<float> &pred, mvector<float> &conc, float &r, 
            float &a, float &b) {
  mvector<float> x(pred.xpoints()), y(pred.xpoints());
  float sx, sy, sxy, sxx;
  float det;
  float n;

  n = (float) pred.xpoints();
  x = pred;
  y = conc;

  if (x.xpoints() != y.xpoints() ) {
    printf("data volume mismatch in correl: %d %d\n",x.xpoints(), y.xpoints() );
  }
  sx = x.average()*n;
  sy = y.average()*n;
  sxy = dot(x, y);
  sxx = dot(x, x);

  det = n*sxx - sx*sx;
  if (det == 0.) {
    printf("Singular, cannot work with\n");
    a = 0;  b = 0; r = 0;
    return ;
  }
  
  b = (n * sxy  - sx*sy) / det;
  a = y.average() - b*x.average();

  r = (n * sxy  - sx*sy) / sqrt(det) / sqrt(n*dot(y,y) - sy*sy);
//  x -= x.average();
//  x /= sqrt(dot(x, x));
//  y -= y.average();
//  y /= sqrt(dot(y,y));
//  r = dot(x, y);

  return;

}

float dot(mvector<float> &x, mvector<float> &y) {
  double sum = 0.0;
  //should test for xpoints being same on both
  int i;
  for (i = 0; i < x.xpoints() ; i++) {
    sum += x[i] * y[i] ;
  }
  return (float) sum;
}
