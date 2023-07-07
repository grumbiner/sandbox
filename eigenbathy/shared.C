#include "ncepgrids.h"

void orthog(mvector<double> &x, mvector<mvector<double> > &y, int modemax) ;
double dot(mvector<double> &x, mvector<double> &y) ;

void random(mvector<double> &x) ;
void show(mvector<double> &eta) ;

void gradients(mvector<double> &eta, mvector<double> &dx, double &deltax);
void divergence(mvector<double> &dx, mvector<double> &div, double &deltax);

void d2dx2(mvector<double> &eta, mvector<double> &out, double &deltax);
void d2dx2(mvector<double> &c2, mvector<double> &eta, mvector<double> &out, double &deltax);

////////////////////

// orthonormalize x w.r.t. each of the first modemax members of y
void orthog(mvector<double> &x, mvector<mvector<double> > &y, int modemax) {
  int i, n;
  double rx, ry, tmp;
  mvector<double> tvec(x.xpoints());

  for (n = 0; n < modemax; n++) {
    tmp = dot(x,y[n]);
    rx  = dot(x,x);
    ry  = dot(y[n],y[n]);
    tvec = y[n];
    tvec *= tmp/ry;
    x -= tvec;
  }
  x /= sqrt(dot(x,x));

  return;
}
double dot(mvector<double> &x, mvector<double> &y) {
  double sum = 0.0;
  for (int i = 0; i < x.xpoints(); i++) {
    sum += x[i]*y[i];
  }
  return (double) sum;
}


void random(mvector<double> &x) {
  for (int loc = 0; loc < x.xpoints() ; loc++) {
    x[loc] = drand48();
  }
  return;
}


void gradients(mvector<double> &eta, mvector<double> &dx, double &deltax) {
  int i;
  dx = 0.0;
  for (i = 1; i < dx.xpoints() - 1 ; i++) {
    dx[i] = (eta[i+1]-eta[i-1])/2./deltax;
  }

  return;
}
void divergence(mvector<double> &dx, mvector<double> &div, double &deltax) {
  int i;
  div = 0.0;

  for (i = 1; i < dx.xpoints() - 1 ; i++) {
    div[i] = (dx[i+1]-dx[i-1])/2./deltax;
  }

  return;
}

void show(mvector<double> &eta) {
  int loc;
  for (loc = 0; loc < eta.xpoints(); loc++) {
      printf("%3d  %12.5e\n",loc, eta[loc]);
  }

  return ;
}

void d2dx2(mvector<double> &eta, mvector<double> &out, double &deltax) {
  int i;
  out = 0.;
  for (i = 1; i < eta.xpoints() - 1; i++) {
    out[i] = (eta[i+1]-2.*eta[i]+eta[i-1])/deltax/deltax;
  }
  return;
}
void d2dx2(mvector<double> &c2, mvector<double> &eta, mvector<double> &out, double &deltax) {
  int i;
  out = 0.;
  for (i = 1; i < eta.xpoints() - 1; i++) {
    out[i] = c2[i]*(eta[i+1]-2.*eta[i]+eta[i-1])/deltax/deltax;
    out[i] += (eta[i+1]-eta[i-1])*(c2[i+1]-c2[i-1])/4./deltax/deltax;
  }
  return;
}


/////////// For 2d grids //////////////////////////////////

void random(grid2<float> &x) ;
void show(llgrid<float> &eta) ;
void average(llgrid<short int> &bathy, llgrid<float> &averaged) ;


void show(llgrid<float> &eta) {
  ijpt loc;
  latpt ll;
  for (loc.j = 0; loc.j < eta.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < eta.xpoints(); loc.i++) {
    //if (fabs(eta[loc]) > 1.e-26) {
      ll = eta.locate(loc);
      printf("%3d %3d %.3f %.3f  %12.5e\n",loc.i, loc.j, ll.lat, ll.lon, eta[loc]);
    //}
  }
  }

  return ;
}
void average(llgrid<short int> &bathy, llgrid<float> &averaged) {
  grid2<int> count(averaged.xpoints(), averaged.ypoints() );
  ijpt loc1, loc2;
  int ratio, pts = 0;

  ratio = bathy.xpoints() / averaged.xpoints() ;
  printf("ratio = %d nx ny %d %d\n",ratio, averaged.xpoints(), averaged.ypoints() );

  count.set(0);
  averaged.set((float) 0.0);
  for (loc1.j = 0; loc1.j < bathy.ypoints(); loc1.j++) {
  loc2.j = loc1.j / ratio;
  for (loc1.i = 0; loc1.i < bathy.xpoints(); loc1.i++) {
    loc2.i = loc1.i / ratio;
    if (bathy[loc1] < 0.) {
      count[loc2] += 1;
      averaged[loc2] += bathy[loc1];
    }
  }
  }
  for (loc2.j = 0; loc2.j < averaged.ypoints(); loc2.j++) {
  for (loc2.i = 0; loc2.i < averaged.xpoints(); loc2.i++) {
    if (count[loc2] != 0) { 
      averaged[loc2] /= count[loc2]; 
      pts += 1;
    }
  }
  }
  averaged *= -1.;

  //printf("averaged: %f %f\n",averaged.gridmax(), averaged.gridmin() );
  //printf("count: %d %d\n",count.gridmax(), count.gridmin() );
  //printf("ocean points %d %d\n",pts, count.xpoints()*count.ypoints() );

  return;
}
void random(grid2<float> &x) {
  ijpt loc;
  for (loc.j = 0; loc.j < x.ypoints() ; loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints() ; loc.i++) {
    x[loc] = drand48();
  }
  }
  return;
}
