#include "points.h"
#include "ncepgrids.h"

// Make a first past through a time and space continuous field
//   (e.g., SST) and compute statistics which can be done exactly
//   by accumulation on 1 pass.
// Work in long integers to avoid the problem of accumulating small 
//    numbers to large cumulants in reals (even doubles)
// Read from 2 byte integers of Reynolds QDOI sst, but formatted in my
//    standard orientation, and with land flags removed.  Some points,
//    therefore, will have 0 variance




template <class T>
void compute_moments(grid2<T> &sum, grid2<T> &sum2, grid2<T> &sum3, grid2<T> &sum4, grid2<float> &mean, grid2<float> &var, grid2<float> &skew, grid2<float> &kurtosis, int count) ;

template <class T>
void compute_trends(grid2<T> &sx, grid2<T> &sx2, grid2<T> &sxy, grid2<T> &sy, grid2<T> &sy2, grid2<float> &slope, grid2<float> &intercept, grid2<float> &correl, grid2<float> &tstatistic, int npts) ;

template <class T>
void findrtg(grid2<T> &mag, grid2<float> &rtg_scale) ;


template <class T>
void update_max(grid2<T> &x, grid2<T> &xmax) ;
template <class T>
void update_min(grid2<T> &x, grid2<T> &xmin) ;

void findres(global_quarter<float> &mag, global_quarter<float> &res, float toler) ;

int main(int argc, char *argv[]) {
// For reading and accumulating
  global_quarter<short int> tmp, tmax, tmin, trange;
  global_quarter<long long int> tmp2, stmp, sum, sum2, sum3, sum4;
// For trend
  global_quarter<long long int> sumx, sumx2, sumxy;  // sumy = sum, sumy2 = sum2;
  global_quarter<float> slope, intercept, correl, tstatistic;
// Resultant moment terms:
  global_quarter<float> mean, var, skew, kurtosis;

// utility:
  FILE *fin;
  int i, npts;

// Initialize:
  tmax.set((short int) -300);
  tmin.set((short int) 5000);
  sum.set((long long int) 0);
  sum2.set((long long int) 0);
  sum3.set((long long int) 0);
  sum4.set((long long int) 0);
  sumx.set((long long int) 0);
  sumxy.set((long long int) 0);
  sumx2.set((long long int) 0);
  #ifdef HARMONIC
  for (i = 1; i <= NPER; i++) {
    fsin[i-1] = sin(2.*M_PI*i*step / PERIOD) * 2./PERIOD;
    fcos[i-1] = cos(2.*M_PI*i*step / PERIOD) * 2./PERIOD;
  } 
  #endif
  npts = argc - 1;
  printf("npts = %d\n",npts); fflush(stdout);

// Start scan through observations:
  for (i = 1; i < argc; i++) {
    fin = fopen(argv[i], "r");
    tmp.binin(fin);
    fclose(fin);

    conv(tmp, tmp2);
    stmp = tmp2; 
    
    sum += stmp;
    stmp *= tmp2;
    sum2 += stmp;
    stmp *= tmp2;
    sum3 += stmp;
    stmp *= tmp2;
    sum4 += stmp;

    // for trends:
    sumx  += (i-1);
    sumx2 += (i-1)*(i-1);
    stmp = tmp2; stmp *= (i-1);
    sumxy += stmp;

    update_max(tmp, tmax);
    update_min(tmp, tmin);

      #ifdef HARMONIC
      for (i = 0; i < NPER; i++) {
        sum_sin[loc][i] += tmp*fsin[i];
        sum_cos[loc][i] += tmp*fcos[i];
      }
      #endif


  }

// Find things of interest and write them out:
  FILE *fout;
  global_quarter<float> ftmp;

  trange = tmax;
  trange -= tmin;
  compute_moments(sum, sum2, sum3, sum4, mean, var, skew, kurtosis, npts);
  compute_trends(sumx, sumx2, sumxy, sum, sum2, slope, intercept, correl, tstatistic, npts);

  fout = fopen("first_pass_out", "w");
  // convert to plain int for ease with grads et al.
  conv(tmax, ftmp);
    ftmp.binout(fout);
  conv(tmin, ftmp);
    ftmp.binout(fout);
  conv(trange, ftmp);
    ftmp.binout(fout);

  mean.binout(fout);
  var.binout(fout);
  skew.binout(fout);
  kurtosis.binout(fout);
  slope.binout(fout);
  intercept.binout(fout);
  correl.binout(fout);
  tstatistic.binout(fout);
  

// compute areal histogram for the means:
// loop
//      printf("histogram %3d  %7d ",i,hist[i]);
//      printf(" area %9.5f\n",flag.integrate()/1.e12);
//      sumarea += flag.integrate()/1.e12;
// endloop

// compute vector gradient, magnitude of gradient, laplacean of T
  float scale = 1.e5;
  global_quarter<float> sst, dx, dy, mag, lapl, resolution;
  float flag = -9999.;

  conv(mean, sst);
  sst /= 100;
  
  gradients(sst, dx, dy, mag, flag);
  laplacean(sst, lapl, flag);

// scaling to degrees per 100 km for gradients, and degrees per 10^4 km^2 for 
//    lapl
  dx *= scale;
  dy *= scale;
  mag *= scale;
  findres(mag, resolution, 0.5);
  lapl *= (scale*scale);
  dx.binout(fout);
  dy.binout(fout);
  mag.binout(fout);
  resolution.binout(fout);
  lapl.binout(fout);

  global_quarter<float> rtg_scale;
  findrtg(mag, rtg_scale);
  rtg_scale.binout(fout);

  fclose(fout);
  return 0;
}


template <class T>
void compute_moments(grid2<T> &sum, grid2<T> &sum2, grid2<T> &sum3, grid2<T> &sum4, grid2<float> &mean, grid2<float> &var, grid2<float> &skew, grid2<float> &kurtosis, int count)  {
  ijpt loc;
  grid2<float> var3(sum.xpoints(), sum.ypoints() );
  grid2<float> var4(sum.xpoints(), sum.ypoints() );

  mean.set((float) 0.0);
  var.set((float) 0.0);
  skew.set((float) 0.0);
  kurtosis.set((float) 0.0);

////
// Compute some higher order statistics -- not assuming mean == 0
  int n = count;
  conv(sum, mean);
  mean /= (float) n;
  for (loc.j = 0; loc.j < sum.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < sum.xpoints(); loc.i++) {
      var[loc] = (sum2[loc] - n*mean[loc]*mean[loc]) / n;
      var3[loc] = (sum3[loc] - 3*mean[loc]*sum2[loc] + 3*mean[loc]*mean[loc]*sum[loc]
                       - n*mean[loc]*mean[loc]*mean[loc]) / n ;
      var4[loc] = (sum4[loc] - 4*mean[loc]*sum3[loc] + 6*mean[loc]*mean[loc]*sum2[loc]
                       - 4*mean[loc]*mean[loc]*mean[loc]*sum[loc]
                       + n*mean[loc]*mean[loc]*mean[loc]*mean[loc] ) / n ;
      if (var[loc] > 1.e-2) {
        skew[loc]     = var3[loc] / pow((double) (var[loc]),1.5);
        kurtosis[loc] = var4[loc] / (var[loc]*var[loc])     - 3.0;
      }
      else {
        skew[loc] = 0.;
        kurtosis[loc] = 0.;
      }
    }
    }
////////////////////
// Compute fourier amplitude and phase:
//    #ifdef HARMONIC 
//    for (i = 0; i < NPER; i++) {
//       ampl[loc][i] = sqrt(sum_sin[loc][i]*sum_sin[loc][i] +
//                      sum_cos[loc][i]*sum_cos[loc][i]    );
//       phase[loc][i] = atan2(sum_sin[loc][i], sum_cos[loc][i] );
//    }
//    #endif


  return;
}
template <class T>
void update_max(grid2<T> &x, grid2<T> &xmax) {
  for (int i = 0; i < x.xpoints()*x.ypoints(); i++) {
    if (x[i] > xmax[i]) xmax[i] = x[i];
  }
  return;
}
template <class T>
void update_min(grid2<T> &x, grid2<T> &xmin) {
  for (int i = 0; i < x.xpoints()*x.ypoints(); i++) {
    if (x[i] < xmin[i]) xmin[i] = x[i];
  }
  return;
}
template <class T>
void findrtg(grid2<T> &mag, grid2<float> &rtg_scale) {
// implement rtg rule of scale fn(mag)
// note that mag is in K per 100 km
  for (int i = 0; i < mag.xpoints()*mag.ypoints(); i++) {
    if (mag[i] != 0) {
      rtg_scale[i] = min((float) 450, max(225./mag[i], 100.));
    }
  }
  return;
}
template <class T>
void compute_trends(grid2<T> &sx, grid2<T> &sx2, grid2<T> &sxy, grid2<T> &sy, grid2<T> &sy2, grid2<float> &slope, grid2<float> &intercept, grid2<float> &correl, grid2<float> &tstatistic, int npts) {
  ijpt loc;

// x = time, y = sst.
  for (loc.j = 0; loc.j < sx.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < sx.xpoints(); loc.i++) {
      slope[loc] = (float) ((double) (npts*sxy[loc]) - (double) (sx[loc]*sy[loc]) ) /
                   (float) ((double) (npts*sx2[loc]) - (double) (sx[loc]*sx[loc]) );
      intercept[loc] =(float)  ((double) sy[loc]/(double) npts - (slope[loc]*(double) sx[loc])/(double) npts);
      correl[loc] =(float)  ((double) npts*sxy[loc] - (double) sx[loc]*sy[loc]) /
                   sqrt((double) npts*sx2[loc] - (double) sx[loc]*sx[loc]) /
                   sqrt((double) npts*sy2[loc] - (double) sy[loc]*sy[loc]) ;
  
      // test statistic goes here
      tstatistic[loc] = correl[loc]*sqrt(npts)/(1. - correl[loc]*correl[loc]);
  }
  }
  return;
}
void findres(global_quarter<float> &mag, global_quarter<float> &res, float toler) {
  res.set((float) 0.0);
  for (int i = 0; i < mag.xpoints()*mag.ypoints(); i++) {
    if (mag[i] != 0) res[i] = (toler / (mag[i]/100.)) * (60 / 111.1);
  }
  printf("max min of mag and res %f %f  %f %f\n",mag.gridmax(0.0), mag.gridmin(0.0),
      res.gridmax(0.0), res.gridmin(0.0) ); fflush(stdout);
  return;
}
