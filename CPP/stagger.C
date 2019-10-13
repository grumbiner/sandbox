#include "mvector.h"

void bathy(mvector<float> &h, float chi, float dx);
void surface(mvector<float> &h, float chi, float dx) ;
float sum(mvector<float> &x, float dx);
float sumsq(mvector<float> &x, float dx);
void advance(mvector<float> &h, mvector<float> *u, mvector<float> *eta, float dx, float dt, int i);

int main(void) {
  mvector<float> u[3], eta[3];
  mvector<float> h;
  float chi, sumsqu, sumsqe, sumu, sume;
  int i, nx = 101, nstep = 200;
  //float dx = 0.01, dt = dx / 5.;
  //float length = dx*(nx - 1);
  float length = 1.0;
  float dx = length/(nx-1), dt = dx / 5.;
  char fname[80];
  FILE *fout;

  for (i = 0; i < 3; i++) {
    u[i].resize(nx);
    u[i] = 0.0;
    eta[i].resize(nx);
  }
  h.resize(nx);


  for (chi = 0.0; chi < 1. ; chi += 1/2.) {
    sprintf(fname,"chi%f",chi);
    printf("fname = %s\n",fname);
    fout = fopen(fname,"w");
    bathy(h, chi, dx);
    //for (int j = 0; j < nx; j++) {
    //  printf("%d h %f\n",j,h[j]);
    //}
    surface(eta[0], chi, dx);
    surface(eta[1], chi, dx);
    surface(eta[2], chi, dx);
    u[0] = 0.0;
    u[1] = 0.0;
    u[2] = 0.0;
    printf("chi %f  %f %f  %f %f\n",chi, sum(u[0], dx), sum(eta[0], dx), sumsq(u[0], dx), sumsq(eta[0], dx) );
    for (i = 0; i <= nstep; i++) {
      advance(h, u, eta, dx, dt, i);   
      if ( (i%5) == 0 ) {
        sumu = sum(u[i%2], dx);
        sume = sum(eta[i%2], dx);
        sumsqu = sumsq(u[i%2], dx);
        sumsqe = sumsq(eta[i%2], dx);
        for (int j = 0; j < nx; j++) {
          fprintf(fout, "%d %d %f %f  %f %f  %f %f  %f\n",i, j, 
              u[i%2][j], eta[i%2][j], sumu, sume, sumsqu, sumsqe, sumsqu+sumsqe);
        }
      }
    }
    printf("chi %f  %f %f  %f %f\n",chi, sum(u[0], dx), sum(eta[0], dx), sumsq(u[0], dx), sumsq(eta[0], dx) );
    fclose(fout);

  }



  return 0;
}
float sumsq(mvector<float> &x, float dx) {
  double tsum = 0.0;
  for (int i = 0; i < x.xpoints(); i++) {
    tsum += x[i]*x[i]*dx;
  }
  return (float) tsum;
}
float sum(mvector<float> &x, float dx) {
  double tsum = 0.0;
  for (int i = 0; i < x.xpoints(); i++) {
    tsum += x[i]*dx;
  }
  return (float) tsum;
}
void bathy(mvector<float> &h, float chi, float dx) {
// Bathymetry is a gaussian hump between 0 and 1
  float sigma = 0.1, x0 = 0.532;
  int i;
  for (i = 0; i < h.xpoints(); i++) { 
    h[i] = (1.1 - exp(- (i*dx - x0 + chi*dx)*(i*dx - x0 + chi*dx) / 2. / sigma / sigma) );
    h[i] = 1.0;
  }

  return;
}
void surface(mvector<float> &h, float chi, float dx) {
// Bathymetry is a gaussian hump between 0 and 1
  float sigma = 0.05, x0 = 0.5 + dx/2.;
  for (int i = 0; i < h.xpoints(); i++) { 
    h[i] = 0.1*exp(- (i*dx - x0 + chi*dx)*(i*dx - x0 + chi*dx) / 2. / sigma / sigma) ;
  }

  return;
}
void advance(mvector<float> &h, mvector<float> *u, mvector<float> *eta, float dx, float dt, int step) {
  int i;
  int t, tp, nx = h.xpoints();
  t = step % 2;
  tp = (step+1)%2;

  for (i = 1; i < nx - 1; i++) {
    u[tp][i]   =   u[t][i] - (dt/2./dx) * (eta[t][i+1]      - eta[t][i-1]);
    eta[tp][i] = eta[t][i] - (dt/2./dx) * (h[i+1]*u[t][i+1] - h[i-1]*u[t][i-1]);
  } 

  u[tp][0]      = 0.;
  u[tp][nx - 1] = 0.;

  eta[tp][0]      = eta[tp][1];
  eta[tp][nx - 1] = eta[tp][nx - 2];

  return;
}
