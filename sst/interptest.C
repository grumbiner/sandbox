#include <ctime>
#include "points.h"
#include "ncepgrids.h"

// Assemble the estimate for current sst given the reference fields
//   and the time
// Input is the name of the reference fields file, then YYYY MM DD,
//   and finally, the name of the file that you want the output in. 

#define PERIOD 365.259636
#define NPER 6

#define TROPICAL    365.24219
#define SIDEREAL    365.256363
#define ANOMALISTIC 365.259635

#define lunar_sidereal 27.32166
#define lunar_perigee (8.85*SIDEREAL)
#define lunar_node    (18.6*SIDEREAL)

////////////////////////////////////////////////////////////////////////
template <class T>
void  read_reference(FILE *fin, global_quarter<T> &slope, global_quarter<T> &intercept,
                     mvector<global_quarter<T> > &ampl, mvector<global_quarter<T> > &phase) ;

template <class T>
void zero3six(global_quarter<T> &expectation, global_quarter<T> &slope, global_quarter<T> &intercept, mvector<global_quarter<T> > &ampl, mvector<global_quarter<T> > &phase, float &time, int &nharm, mvector<double> &omega) ;

template <class T>
void assemble1(grid2<T> &expectation, grid2<T> &slope, grid2<T> &intercept, float time) ;

// find number of days since 1 September 1981
void find_days(int yy, int mm, int dd, float &time) ;

//
void relaxation_fill(global_quarter<float> &expect_fill, global_quarter<float> &mask, float maskval) ;
float iterate(llgrid<float> &land, llgrid<float> &y, const float &flag) ;
////////////////////////////////////////////////////////////////////////

int main(int argc, char *argv[]) {
// For harmonics
  mvector<double> omega(NPER), period(NPER);
// From first pass
  global_quarter<float> slope, intercept;
  mvector<global_quarter<float> > ampl(NPER), phase(NPER);

// utility:
  FILE *fin;
  int i, j, nharm = NPER;
  ijpt loc;
  float time;
  global_quarter<float> expect;
  global_quarter<float> mask;
  global_quarter<unsigned char> umask;
  global_12th<float> expect_high;
  float landval = -9, maskval = -9;

///////////////////////////////////////////////////////////
// Start working
  fin = fopen(argv[1],"r");
  read_reference(fin, slope, intercept, ampl, phase);
  fclose(fin);

  //printf("slope stats %e %e %e %e\n",slope.gridmax(0.0), slope.gridmin(0.0), slope.average(0.0), slope.rms(0.0) );
  //printf("intercept stats %e %e %e %e\n",intercept.gridmax(0.0), intercept.gridmin(0.0), intercept.average(0.0), intercept.rms(0.0) );
//  for (i = 0; i < NPER; i++) {
//    printf("phase %1d stats %e %e %e %e\n",i,phase[i].gridmax(), phase[i].gridmin(), phase[i].average(), phase[i].rms() );
//  }

  // construct a mask file:
  int count = 0;
  for (i = 0; i < mask.xpoints()*mask.ypoints(); i++ ) {
    if (ampl[0][i] == 0) {
      mask[i] = landval;
      count++;
    }
    else {
      mask[i] = 0;
    }
  }
  //printf("land count = %d\n",count);
  conv(mask, umask);



  int yy, mm, dd;
  yy = atoi(argv[2]);
  mm = atoi(argv[3]);
  dd = atoi(argv[4]);
  find_days(yy, mm, dd, time);
//  #ifdef VERBOSE
  printf("%d %d %d days since 1 Sep 1981 = %f\n",yy, mm, dd, time);
  fflush(stdout);
//  #endif

// Initialize:
  fin = fopen("doodson","r");
  if (fin == (FILE*) NULL) {
    printf("failed to open the doodson # file\n");
    return 1;
  }
  int d1, d2, d3, d4, d5, d6;
  for (j = 0; j < NPER; j++) {
    fscanf(fin, "%d %d %d %d %d %d\n",&d1, &d2, &d3, &d4, &d5, &d6);
    //printf("%d %d %d %d %d %d  ",d1, d2, d3, d4, d5, d6);
    omega[j] = 
        1/(TROPICAL)*d1        +
        1/(SIDEREAL)*d2        + 
        1/(ANOMALISTIC)*d3     + 
        1/(lunar_sidereal)*d4  + 
        1/(lunar_node)*d5      + 
        1/(lunar_perigee)*d6 ;
     period[j] = 1./omega[j];
     omega[j] *= 2.*M_PI;
     //printf(" %e %e\n",period[j],omega[j]);
  }
  fflush(stdout);

////////////////////////////////////////////////////////////////////////////

  zero3six(expect, slope, intercept, ampl, phase, time, nharm, omega);

//global_quarter<float> expect_fill;
//expect_fill = expect; // a grid that we will fill in via relaxation to/of a laplacean
//relaxation_fill(expect_fill, mask, maskval);

//expect_high.fromall(expect_fill, mask, landval, maskval);
  expect_high.fromall(expect, mask, landval, maskval);
//expect_high.fromall(expect, landval, maskval);
  // fill in the masked points on high res grid with values from nearest 
  //   original (relaxation-filled) point
  i = 0;
  for (loc.j = 1; loc.j < 8; loc.j++) {
//for (loc.i = 1; loc.i < 8; loc.i++) {
  for (loc.i = expect_high.xpoints()-5; loc.i < expect_high.xpoints(); loc.i++) {
   
    printf("%d %d %6.2f \n",loc.i,loc.j,expect_high[loc] );

  }
  for (loc.i = 0; loc.i < 8; loc.i++) {
    printf("%d %d %6.2f \n",loc.i,loc.j,expect_high[loc] );
  }
  }
  //printf("filled in %d points\n",i);
    


//printf("h1 %6.2f %6.2f %6.2f %6.2f \n",expect_high.gridmax(), expect_high.gridmin(), expect_high.average(), expect_high.rms() );
//printf("h2 %6.2f %6.2f %6.2f %6.2f\n",expect_high.gridmax(landval), expect_high.gridmin(landval), expect_high.average(landval), expect_high.rms(landval) );

  FILE *fout;
  fout = fopen(argv[5],"w");
  expect_high.ftnout(fout);
  fclose(fout);
////////////////////////////////////////////////////////////////////////////

  return 0;
}
////////////////////
template <class T>
void assemble1(grid2<T> &expectation, grid2<T> &slope, grid2<T> &intercept, float time) {
// time is days since 1 September 1981, with that day being 0.
  expectation = slope;
  expectation *= time;
  expectation += intercept;
  return;
}

template <class T>
void zero3six(global_quarter<T> &expectation, global_quarter<T> &slope, global_quarter<T> &intercept, mvector<global_quarter<T> > &ampl, mvector<global_quarter<T> > &phase, float &time, int &nharm, mvector<double> &omega) {
// time is days since 1 September 1981, with that day being 0.

  ijpt loc;
  for (loc.j = 0; loc.j < expectation.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < expectation.xpoints(); loc.i++) {
    if(loc.i%2 == 0) {
      if(loc.j%2 == 0) {
        expectation[loc] = 0;
      }
      else {
        expectation[loc] = 3;
      }
    }
    else {
      if(loc.j%2 == 0) {
        expectation[loc] = 3;
      }
      else {
        expectation[loc] = 6;
      }
    }
    if(loc.i < 7 || loc.i > expectation.xpoints() - 8) {
      if(loc.j < 7 || loc.j > expectation.ypoints() - 8) {
        printf("%d %d %6.2f \n",loc.i,loc.j,expectation[loc] );
      }
    }
  }
  }
//for (int index = 0; index < expectation.ypoints()*expectation.xpoints(); index++) {
//  if (index%2 == 0) {
//    expectation[index] = 0;
//  }
//  else {
//    expectation[index] = 3;
//  }
//}
  return;
}
template <class T>
void  read_reference(FILE *fin, global_quarter<T> &slope, global_quarter<T> &intercept, 
                  mvector<global_quarter<T> > &ampl, mvector<global_quarter<T> > &phase) {
  intercept.binin(fin);
  slope.binin(fin);

  for (int i = 0; i < NPER; i++) {
    ampl[i].binin(fin);
    phase[i].binin(fin);
  }

  return;
}
void find_days(int yy, int mm, int dd, float &time) {
  tm tmp;
  time_t secs;

  if (yy > 1900) {
    tmp.tm_year = yy - 1900;
  }
  else if (yy < 36) {
    tmp.tm_year = yy + 100;
  }
  else {
    tmp.tm_year = yy;
  }

  tmp.tm_mon  = mm - 1;
  tmp.tm_mday = dd;
  tmp.tm_hour = 0;
  tmp.tm_min  = 0;
  tmp.tm_sec  = 0;

  secs = mktime(&tmp);

  tmp.tm_year = 81;
  tmp.tm_mon  =  8;
  tmp.tm_mday  =  1;
  secs -= mktime(&tmp);
  time = secs / 86400.;

  return;
}
float iterate(llgrid<float> &land, llgrid<float> &y, const float &flag) {
  ijpt loc;
  float lim = 0;
  double del;
  double c1, c2, c3;
  ijpt ip, jp, im, jm;
  latpt ll;
  double dlat, dlon;
  double theta, divisor;


// precompute some variables


  dlat = y.dlat;
  dlon = y.dlon;
  dlat *= M_PI / 180.;
  dlon *= M_PI / 180.;

  c1 = 1./dlat/dlat;

  for (loc.j = 1; loc.j < land.ypoints()-1; loc.j++) {
    ip.j = loc.j;
    jp.j = loc.j+1;
    im.j = loc.j;
    jm.j = loc.j-1;
    ll = y.locate(loc);
    theta = ll.lat * M_PI/180.;
    divisor = 2./cos(theta)/cos(theta)/dlon/dlon + 2./dlat/dlat;
    c2 = 1./dlon/dlon/cos(theta)/cos(theta);
    c3 = -tan(theta)/2./dlat;

  for (loc.i = 0; loc.i < land.xpoints(); loc.i++) {
    if (land[loc] == flag) {
      jm.i = loc.i;
      jp.i = loc.i;
      ip.i = loc.i + 1;
      im.i = loc.i - 1;
      if (im.i < 0) im.i += land.xpoints();
      if (im.i > land.xpoints() - 1) im.i -= land.xpoints();

      del  = (y[jp] + y[jm])*c1 + (y[ip] + y[im])*c2;
      del -= c3*(y[jp]-y[jm]);
      y[loc] = del/divisor;

    }
  }
  }

  return lim;
}
void relaxation_fill(global_quarter<float> &expect_fill, global_quarter<float> &mask, float maskval) {
  int i = 0, itmax = 100000;
  float dmax, dmin, rms = 9.e9;
  float limit = 1.e-4;
  global_quarter<float> old, tmp;

  rms = expect_fill.rms();
  limit = rms / 1.e5;
  printf("input rms = %e limit = %e \n",rms, limit);
  
  old = expect_fill;
  while (rms > limit && i < itmax) {
    iterate(mask, expect_fill, maskval);
    tmp = expect_fill;
    tmp -= old;
    dmax = tmp.gridmax();
    dmin = tmp.gridmin();
    rms = max(dmax, -dmin);

    old = expect_fill;
    //printf("%3d iteration %f max delta\n",i,rms);
    i++;
  } 
    printf("%3d iterations %f max delta\n",i,rms);


  return;
}
