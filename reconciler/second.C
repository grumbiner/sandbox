#include "ncepgrids.h"


// read in land/coast/ocean/inland water mask file and produce a
//   distance to coast grid for use in navy/physical blending
// Second pass -- have already done the simple check and written out results
template <class T>
float distperim(metricgrid<T> &x, T flag, int range, ijpt loc) ;

int main(int argc, char *argv[]) {
  global_12th<unsigned char> mask;
  global_12th<int> idist;
  global_12th<float> dist, grad, gradients;
  FILE *fin, *fout;
  ijpt loc;
  bool clear = false;
  unsigned char land = 157, water = 0, coast = 195;
  float tmp, flag = -99.0;
  
  fin = fopen(argv[1],"r");
  mask.binin(fin);
  fclose(fin);
  fin = fopen(argv[2], "r");
  dist.binin(fin);
  grad.binin(fin);
  idist.binin(fin);
  fclose(fin);

  fout = fopen(argv[3], "w");

  for (loc.j = 0; loc.j < mask.ypoints(); loc.j++ ) {
  for (loc.i = 0; loc.i < mask.xpoints(); loc.i++ ) {
    if (mask[loc] == coast) mask[loc] = water;
  }
  }

// high latitudes are always an issue:
  for (loc.j = 0; loc.j < 12*10; loc.j++) {
  for (loc.i = 0; loc.i < mask.xpoints(); loc.i++ ) {
    tmp = distperim(mask, land, idist[loc], loc);
    if (tmp < dist[loc]) {
      printf("%4d %4d  %10.2f %10.2f\n",loc.i, loc.j, tmp, dist[loc]); fflush(stdout);
      dist[loc] = tmp; 
    }
  }
  }
  // update the gradients:
  gradsq(dist, gradients, flag);
  grad = gradients;

// now look for points 'near' land with 'high' gradients
  printf("gradient check \n"); fflush(stdout);
  clear = false;
  float gradlim = 1000, distlim = 200e3;
  int passno = 0, count = 0;
  while (!clear && passno < 10) {
    clear   = true;
    passno += 1;
    count   = 0;
    for (loc.j = 0; loc.j < mask.ypoints(); loc.j++ ) {
    for (loc.i = 0; loc.i < mask.xpoints(); loc.i++ ) {
      if (grad[loc] > gradlim && dist[loc] < distlim) {
        tmp = distperim(mask, land, idist[loc], loc);
        if (tmp < dist[loc] ) {
          clear = false;
          count++;
          printf("%4d %4d  %10.2f %10.2f\n",loc.i, loc.j, tmp, dist[loc]); fflush(stdout);
          dist[loc] = tmp; 
        }
      }
    }
    }
    printf("passno %d count = %d\n",passno, count);
    gradsq(dist, gradients, flag);
    grad = gradients;
  }

  dist.binout(fout);
  gradients.binout(fout);
  idist.binout(fout); 
  fclose(fout);

  int i;
  mvector<int> histo(1001);

  printf("max gradsq = %f\n",gradients.gridmax() );
  histo = 0; 
  for (i = 0; i < dist.xpoints()*dist.ypoints(); i++) {
    histo[min((int) (gradients[i]+0.5),1000) ] += 1;
  }
  for (i = 0; i < histo.xpoints(); i++) {
    if (histo[i] != 0) {
      printf("grads %4d %7d\n", i, histo[i]); fflush(stdout);
    }
  }

  return 0;
} 
// Find the minimum distance to a perimeter point that matches a flag
// second pass -- push out in longitude
template <class T>
float distperim(metricgrid<T> &x, T flag, int range, ijpt loc) {
  int imax, irange;
  ijpt tloc, rloc;
  latpt ll1, ll2;
  float dist = 1.e9, scale = 1.0, tmp;

// constant throughout
  ll1 = x.locate(loc);
  scale = max(scale, 1./cos(ll1.lat*3.1416/180) );
  imax  = (int) (0.5 + scale*range); 
  imax = imax % x.xpoints();
  //printf("scale = %f imax %d\n",scale, imax); fflush(stdout);

  for (irange = range-1; irange <= imax; irange++) {

    tloc.i = loc.i - irange;
    if (tloc.i < 0) tloc.i += x.xpoints();
    for (rloc.j = -range; rloc.j <= range; rloc.j++) {
      tloc.j = loc.j + rloc.j;
      tloc.j = min(x.ypoints() - 1, tloc.j);
      tloc.j = max(0, tloc.j);
      if (x[tloc] == flag) {
        ll2 = x.locate(tloc);
        tmp = ARCDIS(ll1.lon, ll1.lat, ll2.lon, ll2.lat);
        if (tmp >= 0) {
          dist = min(dist, tmp* 1e3);
        }
        else {
          printf("error %f %f  %f %f  %f\n",ll1.lon, ll1.lat, ll2.lon, ll2.lat, tmp);
        }
      }
    } 

    tloc.i = loc.i + irange;
    if (tloc.i > x.xpoints() - 1) tloc.i -= x.xpoints() - 1; 
    for (rloc.j = -range; rloc.j <= range; rloc.j++) {
      tloc.j = loc.j + rloc.j;
      tloc.j = min(x.ypoints() - 1, tloc.j);
      tloc.j = max(0, tloc.j);
      if (x[tloc] == flag) {
        ll2 = x.locate(tloc);
        tmp = ARCDIS(ll1.lon, ll1.lat, ll2.lon, ll2.lat);
        if (tmp >= 0) {
          dist = min(dist, tmp* 1e3);
        }
        else {
          printf("error %f %f  %f %f  %f\n",ll1.lon, ll1.lat, ll2.lon, ll2.lat, tmp);
        }
      }
    } 

  }

  return dist;
}
