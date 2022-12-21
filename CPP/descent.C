//15 July 1999
#include "mvector.h"
#include "time_series.h"
#include "points.h"
#include "ncepgrids.h"

template <class T>
void descent(mvector<time_series<T> > &regress, time_series<T> &target, mvector<ijpt> &points, int npoints, T &flag, int lag);


template <class T>
void descent(mvector<time_series<T> > &regress, time_series<T> &target,
                 mvector<ijpt> &points, int npoints, T &flag, int lag) {
  int open = 0; int i, k;
  float r2max, rtmp, netvar = 0., posvar = 1.0, delvar;
  time_series<T> local(target.xpoints() );
  int imax;
  latpt llat;
  northwalsh<T> s;

  //printf("Size of target = %d\n",target.xpoints() );

  if (lag != 0) {
    mvector<time_series<T> > x(MAXSERIES);
    time_series<T> y;
    y.resize(target.xpoints() - lag);
    for (i = 0; i < target.xpoints() - lag; i++) {
      y[i] = target[i+lag];
    }
    for (k = 0; k < npoints; k++) {
      x[k].resize(target.xpoints() - lag);
      for (i = 0; i < target.xpoints() - lag; i++) {
         x[k][i] = regress[k][i];
      }
    }
    k = 0; //Note the near-recursion.  Since k = 0, won't actually repeat this
    descent(x, y, points, npoints, flag, k);
    return ;
  }
  else {
    mvector<time_series<T> > x(MAXSERIES);
    for (k = 0; k < npoints; k++) {
      x[k].resize(target.xpoints() );
      for (i = 0; i < target.xpoints() ; i++) {
         x[k][i] = regress[k][i];
      }
    }

// Normalize
  local = target;
  local.normalize();
  local /= sqrt((float) target.xpoints() );
      // Note that the above covers an error in normalize!!
  //printf("normalized target %f\n",dot(local, local) );

  do {
    r2max = 0.;
    imax = open;
    for (i = open; i < npoints; i++) {
       x[i].normalize(flag);
       rtmp = dot(x[i], local, flag);
       if (rtmp*rtmp > r2max) {
         imax = i;
         r2max = rtmp*rtmp;
       }
       //printf("%3d %3d new norm %f dot w. target %6.3f\n",
       //      points[i].i, points[i].j,
       //      dot(x[i], x[i], flag), rtmp );
    }
    llat = s.locate(points[imax]);
    if (llat.lon < 0.) llat.lon += 360.;
    delvar = posvar*r2max;
    netvar += delvar;
    printf("pri %2d pt %3d %3d %6.2f %6.2f  r2 %7.4f net %7.4f %7.4f\n", open,
        points[imax].i, points[imax].j, llat.lon, llat.lat, r2max, netvar, delvar);
    posvar = posvar - delvar;
    swap(x, points, open, imax); // swap the series of max correl to orig
    open += 1;
    for (i = open; i < npoints; i++) {
       orthog(x[open-1], x[i], flag);
    }
    orthog(x[open-1], local, flag);

  } while (open < 60 && r2max > 0.01 && delvar > 0.005);
  } // end of else clause

  return;
}
