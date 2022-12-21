#include <stdio.h>

// This is a bogus declaration to get around problems SP complex has
//  with the omb class library.

typedef double cmplx ;

//ESSL -- ibm library for linpack-like solution  #include <essl.h>

#include "ssmi85.h"
#include "icessmi85.h"
#include "ncepgrids.h"

#define MAXPTS 200000
#define MAXCONC 128
#define MINCONC  15

#define GRIDTYPE northhigh
#define TERMS 5

// Note that we're using the land to carry the mapping:
int adder(avgpoint *dat, metricgrid<unsigned char> &land, process_2 *x) ;
int candidacy(avgpoint *nconc, int nx, int ny, int nice, mvector<float> &cice,
       mvector<float>  *g85, mvector<float> *g37 );
int vects(unsigned int v, unsigned int h, mvector<float> &x) ;
void regress(mvector<float> &conc, mvector<float> *x, int nice, 
             ijpt *locations, GRIDTYPE<unsigned char> &outconc) ;
void correl(mvector<float> &pred, mvector<float> &conc, float &r, float &a, float &b);
float dot(mvector<float> &x, mvector<float> &y) ;

void leastsq(mvector<float> &pred, mvector<float> &conc, int terms, 
             mvector<float> &coeffs);
void evaluate(mvector<float> &pred, mvector<float> &coeffs) ;
float frob(float *a, int m, int n) ;

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

  fin = fopen("nland.new","r");
  nland.binin(fin);
  fclose(fin);

  nconc = new avgpoint[nland.xpoints()*nland.ypoints() ];

  fin = fopen(argv[1], "r");
#ifdef BUFR
  while (!feof(fin)) {
    fread(&line, sizeof(bufr_line), 1, fin);
    for (i = 0; i < NSCANS; i++) {
      toprocess_1(x, line.full[i]);
      toprocess_2(x2, x);
      count += adder(nconc, nland, x2);
    }
  }
#else
  while (!feof(fin)) {
    fread(&line, sizeof(bufr_line), 1, fin);
    for (i = 0; i < NSCANS; i++) {
      toprocess_1(x, line.full[i]);
      toprocess_2(x2, x);
      count += adder(nconc, nland, x2);
    }
  }
#endif

  printf("Found %d points on grid\n",count);
  if (count == 0) return 1;
     
  for (loc.j = 0; loc.j < nland.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < nland.xpoints(); loc.i++) {
     index = loc.i+loc.j*nland.xpoints();
     nconc[index].average();
     outconc[loc] = nconc[index].aref;
  }
  }
  fout = fopen("nconc.out","w");
  outconc.binout(fout);
  fclose(fout);
  
  count = 0;
  i = 0;
  for (loc.j = 0; loc.j < nland.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < nland.xpoints(); loc.i++) {
    i = loc.i + loc.j*nland.xpoints();
    if (nconc[i].aref >= (unsigned int) MINCONC && 
        nconc[i].aref <= (unsigned int) MAXCONC     ) {
       locations[count].i = loc.i;
       locations[count].j = loc.j;
       count++;
    }
  }
  }
  printf("%d points on hi-res grid had ice\n",count); fflush(stdout);

  candidacy(nconc, nland.xpoints(), nland.ypoints(), count, cice, g85, g37 );
  regress(cice, g85, count, locations, outconc);

//  regress(cice, g37, count, locations, outconc);

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
      // This is the point to extend the land masking zone for comparisons -
      //   anyof (range=5? (65 km))
      if (land[loc] == 0 && x[i].aref < 157) {
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
             ijpt *locations, GRIDTYPE<unsigned char> &outconc) {
  mvector<float> pred(nice);
  int i, j, terms = TERMS;
  mvector<float> coeffs(terms);
  float a, b, r;
  FILE *fout;
  char fname[80];
  
  for (j = 0; j < x[0].xpoints(); j++) {
    for (i = 0; i < nice; i++) {
      pred[i] = (x[i])[j];
    }

    leastsq(pred, conc, terms, coeffs); // least square solution
    evaluate(pred, coeffs); //destructive evaluation of the full prediction
    correl(pred, conc, r, a, b);
    printf("Param %2d correl %6.3f %e %e ",j, r, a, b);
    printf("\n");

    if (fabs(r) >= 0.0) {
      // then print out the grid:
      outconc.set(NO_DATA);
      sprintf(fname,"parm%02d",j);
      fout = fopen(fname, "w");
      for (i = 0; i < nice; i++) {
         // Note that we have to rescale for the binary grid:
         outconc[locations[i] ] = (unsigned char) (0.5 + 100.*pred[i]);
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

  sx = x.average()*x.xpoints();
  sy = y.average()*y.xpoints();
  sxy = dot(x, y);
  sxx = dot(x, x);

  det = x.xpoints()*sxx - sx*sx;
  if (det == 0.) {
    printf("Singular, cannot work with\n");
    a = 0;  b = 0; r = 0;
    return ;
  }

  
  b = (x.xpoints() * sxy  - sx*sy) / det;
  a = y.average() - b*x.average();

  x -= x.average();
  x /= sqrt(dot(x, x));
  y -= y.average();
  y /= sqrt(dot(y,y));
  r = dot(x, y);


  return;
}

float dot(mvector<float> &x, mvector<float> &y) {
  double sum = 0.0;
  //should text for xpoints being same on both
  int i;
  for (i = 0; i < x.xpoints() ; i++) {
    sum += x[i] * y[i] ;
  }
  return (float) sum;
}


// Find least squares polynomial order N (terms) in parameter 'pred' to
//   fit the concentration
void leastsq(mvector<float> &pred, mvector<float> &conc, int terms, 
             mvector<float> &coeffs) {
  //float *a, *b, *x, *rn, *aux;
  float a[MAXPTS][TERMS];
  float *b, *x, *rn, *aux;
  int k, lda = MAXPTS, ldb = pred.xpoints(), ldx = terms;
  float tau = 0.001 * pred.xpoints(); // concentration precision 
                                     // times estimated Frobenius norm of a 
  int m = pred.xpoints(), n = terms, nb = 1;
  int naux = 4*terms, iopt = 0;
  int i, j;

  //a = new float[m*n];
  b = new float[m];
  x = new float[n];
  rn = new float[n];
  aux = new float[naux];
  for (i = 0; i < pred.xpoints(); i++) {
    b[i] = conc[i];
    a[i][0] = 1.0;
    if (terms > 1) {
      for (j = 1; j < terms; j++) {
         a[i][j] = pow(pred[i], j*1.0);
      }
    }
  }
// ESSL library call
  tau = frob(&a[0][0], m, n);
  tau *= 0.00;
//ESSL  sgells(iopt, a, lda, b, ldb, x, ldx, rn, tau, m, n, nb, &k, aux, naux);
  coeffs = (float) 0.0;
  printf("%d terms, coeffs = %e",k, x[0]);
  for (j = 1; j < k; j++) {
    printf(" %10.7e ",x[j]);
  }
  for (j = 0; j < k; j++) {
    coeffs[j] = x[j];
  }
  //printf("data rms %e ",pred.rms() );
   
  return;
}

//Evaluate the polynomial in 'pred', return in place:
void evaluate(mvector<float> &pred, mvector<float> &coeffs) {
  int i, j;
  float tmp;
  
  for (i = 0; i < pred.xpoints(); i++) {
    tmp = coeffs[0];
    for (j = 1; j < coeffs.xpoints(); j++) {
      tmp += coeffs[j]*pow(pred[i], j*1.0) ;
    }
    pred[i] = tmp;
  }

  return;
}      


float frob(float *a, int m, int n) {
  int i;
  double sum = 0.;
  for (i = 0 ; i < m*n; i++) {
    sum += a[i]*a[i];
  }
  return ( (float) sqrt(sum) );
}
