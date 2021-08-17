#include <stdio.h>
#include <time.h>
#include <math.h>

#include "metric.h"

#define NX 360
#define NY 180 

#define sinfn mag*sin(rpdg*lats.lon)

template<class T>
void gbase_sub(grid2_base<T> &x, grid2_base<T> &y, grid2_base<T> &z) ;
template<class T>
void gmath_sub(grid2<T> &x, grid2<T> &y, grid2<T> &z) ;

//Note that we cannot declare a metricgrid, although a pointer to
//  one is ok.  Metricgrids are pure virtuals.
int main(void) {
  metricgrid<float> *x;
  llgrid<float> ll1, ll2, ll3;
  llgrid<float> llhigh(360/.25, 180/.25, 0.25, 0.25, -89.875, .125);
  psgrid<float> ps(50, 90, 25.0, 45.0, 60.0, -10.0, 1.0, 125.e3, 125.e3);
  psgrid<float> ps2(250, 450, 125.0, 225.0, 60.0, -10.0, 1.0, 25.e3, 25.e3);
  grid2<float> dummy(50, 90);
  latpt lats;
  ijpt ij;
  fijpt fij;
  float mag, nonval, maskval, tmp;
  palette<unsigned char> gg(19,65);
  clock_t start, end;

//Test of the grid_base elements of metric grids:
  #ifdef VERBOSE
    printf("Calling gbase_sub\n"); fflush(stdout);
  #endif
  gbase_sub(dummy, ll1, ps);

//Test of the grid_math elements of metric grids:
  #ifdef VERBOSE
    printf("Calling gmath_sub\n"); fflush(stdout);
  #endif
  gmath_sub(ll1, ll2, ll3);


///////////////////////////////////////////////
  printf("Starting the testing of metricgrids\n");
  ij.j = 5;
  ij.i = 5;


//////////// Test locator of integers
  lats = ll1.locate(ij);
  printf("%d %d is at %f %f in ll grid \n",ij.i, ij.j, lats.lat, lats.lon);

  lats = ps.locate(ij);
  printf("%d %d is at %f %f in ps grid \n",ij.i, ij.j, lats.lat, lats.lon);
  
  x = &ll1;
  lats = x->locate(ij);
  printf("%d %d is at %f %f in ll grid by pointer\n",ij.i, ij.j, 
            lats.lat, lats.lon);

  x = &ps;
  lats = x->locate(ij);
  printf("%d %d is at %f %f in ps grid by pointer\n",ij.i, ij.j, 
            lats.lat, lats.lon);

/////////// Test locator of lat-long pts:
  lats.lat = 40.0;
  lats.lon = -89.0;
  fij = ll1.locate(lats);
  printf("%f %f is at %f %f in the ll grid\n",lats.lat, lats.lon, fij.i, fij.j);
  fij = ps.locate(lats);
  printf("%f %f is at %f %f in the ps grid\n",lats.lat, lats.lon, fij.i, fij.j);

  x = &ll1;
  fij = x->locate(lats);
  printf("%f %f is at %f %f in the ll grid\n",lats.lat, lats.lon, fij.i, fij.j);

  x = &ps;
  fij = x->locate(lats);
  printf("%f %f is at %f %f in the ps grid\n",lats.lat, lats.lon, fij.i, fij.j);

/////////// Test location of floating ijpts
  fij.i = (float) ij.i;
  fij.j = (float) ij.j;
  
  lats = ll1.locate(fij);
  printf("%f %f is at %f %f in ll grid \n",fij.i, fij.j, lats.lat, lats.lon);

  lats = ps.locate(fij);
  printf("%f %f is at %f %f in ps grid \n",fij.i, fij.j, lats.lat, lats.lon);
  
  x = &ll1;
  lats = x->locate(fij);
  printf("%f %f is at %f %f in ll grid by pointer\n",fij.i, fij.j, 
            lats.lat, lats.lon);

  x = &ps;
  lats = x->locate(fij);
  printf("%f %f is at %f %f in ps grid by pointer\n",fij.i, fij.j, 
            lats.lat, lats.lon);

///////// Test for cyclic nature:
  if (ll1.iscyclicx() ) {
    printf("The default lat-long grid claims to be cyclic (it is)\n");
  }
  else {
    printf("The default lat-long grid claims not to be cyclic (it is)\n");
    printf("%f %f %f\n",(float)ll1.xpoints(), ll1.dlon, ll1.xpoints()*ll1.dlon);
    printf("%f = 360 - size\n",360.0 - ll1.xpoints()*ll1.dlon);
    printf("%d %d\n",ll1.iscyclicx(), ll1.iscyclicy() );
    return 1;
  }
  if (ps.iscyclicx() ) {
    printf("The constructed ps grid claims to be cyclic (it is not)\n");
  }
  else {
    printf("The constructed ps grid claims not to be cyclic (it is not)\n");
  }

/////////////////////////////////////////////////
// Test the grid to grid interpolation.
  ll2.set(5.0);
  maskval = 0.0;
  nonval = -99.0;
  mag = 100.;

  for (ij.j = 0; ij.j < ll1.ypoints() ; ij.j++) {
  for (ij.i = 0; ij.i < ll1.xpoints() ; ij.i++) {
    lats = ll1.locate(ij);
    //ll1[ij] = sinfn ;
    if (lats.lon < 0.) lats.lon += 360.;
    ll1[ij] = lats.lon * mag;
  }
  }

// Interpolate to the low resolution polar stereographic grid
  start = clock();
  ps.fromall(ll1, ll2, maskval, nonval);
  end = clock();
  printf("time to interpolate 50x90 ps grid: %f\n",(end-start)/(float)CLOCKS_PER_SEC);
  for (ij.j = 0; ij.j < ps.ypoints() ; ij.j++) {
  for (ij.i = 0; ij.i < ps.xpoints() ; ij.i++) {
     lats = ps.locate(ij);
     if (lats.lon < 0.) lats.lon += 360.;
     // Tmp is the relative error
     if (ps[ij] != 0.) {
       tmp = (mag*lats.lon - ps[ij]) / ps[ij];
     }
     else {
       tmp = 0.0;
     }
     if (fabs(tmp) > 1.2e-7) {
     printf("lat lon, value %3d %3d  %6.3f %6.3f  %f %f %e\n",ij.i, ij.j,
        lats.lat, lats.lon, ps[ij], mag*lats.lon - ps[ij], tmp);
     }
  }
  }
  ps.scale();
  ps.xpm("ps.xpm", 7, gg);

// Interpolate to the higher resolution lat-long grid
  start = clock();
  llhigh.fromall(ll1, ll2, maskval, nonval);
  end = clock();
  printf("time to interpolate quarter degree ll grid: %f\n",(end-start)/(float)CLOCKS_PER_SEC);
  for (ij.j = 0; ij.j < llhigh.ypoints() ; ij.j++) {
  for (ij.i = 0; ij.i < llhigh.xpoints() ; ij.i++) {
     lats = llhigh.locate(ij);
     if (lats.lon < 0.) lats.lon += 360.;
     // Tmp is the relative error
     if (llhigh[ij] != 0.) {
       tmp = (mag*lats.lon - llhigh[ij]) / llhigh[ij];
     }
     else {
       tmp = 0.0;
     }
     if (fabs(tmp) > 1.2e-7) {
     printf("lat lon, value %3d %3d  %6.3f %6.3f  %f %f %e\n",ij.i, ij.j,
        lats.lat, lats.lon, llhigh[ij], mag*lats.lon - llhigh[ij], tmp);
     }
  }
  }
  llhigh.scale();
  llhigh.xpm("llhigh.xpm", 7, gg);

//Interpolate to the higher resolution version of ps grid:
  start = clock();
  ps2.fromall(ll1, ll2, maskval, nonval);
  end = clock();
  printf("time to interpolate 5x ps grid: %f\n",(end-start)/(float)CLOCKS_PER_SEC);
  for (ij.j = 0; ij.j < ps2.ypoints() ; ij.j++) {
  for (ij.i = 0; ij.i < ps2.xpoints() ; ij.i++) {
     lats = ps2.locate(ij);
     if (lats.lon < 0.) lats.lon += 360.;
     // Tmp is the relative error
     if (ps2[ij] != 0.) {
       tmp = (mag*lats.lon - ps2[ij]) / ps2[ij];
     }
     else {
       tmp = 0.0;
     }
     if (fabs(tmp) > 1.2e-7) {
     printf("lat lon, value %3d %3d  %6.3f %6.3f  %f %f %e\n",ij.i, ij.j,
        lats.lat, lats.lon, ps2[ij], mag*lats.lon - ps2[ij], tmp);
     }
  }
  }
  ps2.scale();
  ps2.xpm("ps2.xpm", 7, gg);
 
//Quick test of mercator grid
  x = new mercator<float>;
  x->fromall(ll1, ll2, maskval, nonval);
  x->scale();
  x->xpm("mer.xpm", 7, gg);


  return 0;
}

//Exercise the metric grids as if they were grid2_bases:
template<class T>
void gbase_sub(grid2_base<T> &x, grid2_base<T> &y, grid2_base<T> &z) {
  float tmpflt = 7.5;
  ijpt i1, i2;
  int i;

  printf("Entered gbase_sub\n"); fflush(stdout);
  x.resize(30, 20);
  printf("passed resize gbase_sub\n"); fflush(stdout);
  cout << "Size of x " << x.xpoints() << " " << x.ypoints() << "\n";
  cout << "Size of y " << y.xpoints() << " " << y.ypoints() << "\n";
  cout << "Size of z " << z.xpoints() << " " << z.ypoints() << "\n";
  x.resize(20, 20);
  cout << "After trying to resize x to 20x20, destroying any extant data in the process\n";
  cout << "Size of x " << x.xpoints() << " " << x.ypoints() << "\n";

  y.resize(20, 20);
  x.set(5.0);
  y.set(7.0);
  i1.i = 5;
  i1.j = 5;
  cout << y[i1] << " " << x[i1] << "\n";
  y.set(x);
  cout << y[i1] << " " << x[i1] << "\n";
  y[i1] = 23.0;
  cout << y[i1] << " " << x[i1] << "\n";
  i = 22;
  cout << y[i] << "\n";

  if (y.in(i1) ) {
    cout << "In is in grid\n";
  }
  else {
    cout << "In is not in grid\n";
  }
  y[i1] = 23.0;
  i1.j = 6;
  y[i1] = 23.0;
  cout << y[i1] << "\n" ;

  if (y.anyof(23.0, 3, i1)) {
    cout << "One or more points within 3 of i1 is indeed 23\n";
  }
  else {
    cout << "No points within 3 of i1 is indeed 23\n";
  }
  cout << "y.anyof " << y.anyof(23.0, 3, i1) << "\n";
  cout << y[i1] << "\n" ;

// Operator tests:
  y = tmpflt;
  cout << y[i1] << " Should be " << tmpflt << "\n" ;
  y = 2.*tmpflt;
  cout << y[i1] << " Should be " << 2.*tmpflt << "\n" ;
  if (y == x ) {
    cout << "Y and X are identical (should not be the case)\n";
  }
  else {
    cout << "Y and X are not identical (should be the case) \n";
  }
  y = x;
  if (y == x ) {
    cout << "Y and X are identical (should be the case)\n";
  }
  cout << "Leaving the grid_base test\n\n\n";

}

//Now to the grid_math tests
template<class T>
void gmath_sub(grid2<T> &x, grid2<T> &y, grid2<T> &z) {
  float val1=5., val2=6.;
  ijpt i1;
  clock_t start, end;
  int nbit;

  x.resize(NX, NY);
  y.resize(NX, NY);
  z.resize(NX, NY);
  i1.i = 5; i1.j = 7;
  x = val1;
  z = val2;
  y = val2;

  x += z;
  cout << x[i1] << " Should be " << val1+val2 << "\n";
  x -= z;
  cout << x[i1] << " Should be " << val1 << "\n";
  x *= z;
  cout << x[i1] << " Should be " << val1*val2 << "\n";
  x /= z;
  cout << x[i1] << " Should be " << val1 << "\n";
  x += val2;
  cout << x[i1] << " Should be " << val1+val2 << "\n";
  x -= val2;
  cout << x[i1] << " Should be " << val1 << "\n";
  x *= val2;
  cout << x[i1] << " Should be " << val1*val2 << "\n";
  x /= val2;
  cout << x[i1] << " Should be " << val1 << "\n";

//Now explicitly binary operators
  x = x + y;
  cout << x[i1] << " Should be " << val1+val2 << "\n";
  x = x - y;
  cout << x[i1] << " Should be " << val1 << "\n";
  x = x * y;
  cout << x[i1] << " Should be " << val1*val2 << "\n";
  x = x / y;
  cout << x[i1] << " Should be " << val1 << "\n";

//Test timing
  start = clock();
  printf("Loop over x+= z %8.4f\n ", (float)clock()/ (float)CLOCKS_PER_SEC );
  for (int j = 0; j < 100; j++) {
    x += z;
  }
  end = clock();
  printf("                %8.4f ", (float)clock()/ (float)CLOCKS_PER_SEC );
  printf("Approx mflops = %f\n",(float)(100*NY*NX/1024/1024)/
          ((float) (end - start)/(float)CLOCKS_PER_SEC)       );

// Start testing grid level operations:
  printf("Max in x is %f\n",x.gridmax() );
  printf("Min in x is %f\n",x.gridmin() );
  printf("average of x is %f\n",x.average());
  printf("average of x excluding val1's %f\n",x.average(val1) );
  z = x.laplace();
  printf("average of the laplacean of x %f\n",z.average() );
  x.laplace(z);
  printf("average of the laplacean of x %f\n",z.average() );
  z = x.gradsq();
  printf("average of the squared gradient of x %f\n",z.average() );
  x.gradsq(z);
  printf("average of the squared gradient of x %f\n",z.average() );
  printf("rms of x %f\n",x.rms() );
  printf("rescaling of x \n"); x.scale();
  printf("grib scaling of z\n"); z.grib_scale(3, nbit, x);

//Test the reduction operator:
  for (i1.j = 0; i1.j < x.ypoints() ; i1.j++) {
  for (i1.i = 0; i1.i < x.xpoints() ; i1.i++) {
    x[i1] = i1.i;
  }
  }
  z.resize(NX/2, NY/2);
  printf("reduce          %8.4f\n", (float)clock()/ (float)CLOCKS_PER_SEC );
  z.reduce(x);
  printf("                %8.4f\n", (float)clock()/ (float)CLOCKS_PER_SEC );
  printf("Leaving gmath_sub\n\n\n");
}

