#include <cstdio>
#include <cstdlib>
#include <cmath>

#include "amsr2.h"
#include "ncepgrids.h"

#define land 0

int main(int argc, char *argv[]) {
  amsr2head  x;
  amsr2_hrpt hr;
  amsr2_lrpt lr;
  amsr2_spot s[12];

  northhigh<float> nhh;
  southhigh<float> shh;
  grid2<amsr2_hrpt> nh_hr(nhh.xpoints(), nhh.ypoints());
  grid2<amsr2_hrpt> sh_hr(shh.xpoints(), shh.ypoints());
  grid2<amsr2_lrpt> nh_lr(nhh.xpoints(), nhh.ypoints());
  grid2<amsr2_lrpt> sh_lr(shh.xpoints(), shh.ypoints());

  FILE *fin, *fout;
  int i, nobs, gridskip = 0, suse = 0, nuse = 0, nread = 0;
  float sum;
  ijpt loc;
  latpt ll;

  fin = fopen(argv[1], "r");
  if (fin == (FILE*) NULL) {
    printf("failed to open input satellite data file %s\n",argv[1]);
    return 1;
  }
  fout = fopen(argv[2], "w");
  if (fout == (FILE*) NULL) {
    printf("failed to open output satellite data file %s\n",argv[2]);
    return 1;
  }

  while (!feof(fin)) {
    fread(&x, sizeof(x), 1, fin);
    nobs = x.nspots;

    fread(&s[0], sizeof(amsr2_spot), nobs, fin);
    if (feof(fin)) continue;

// print out land fractions where not all 1 or 0:
    sum = 0;
    for (i = 0; i < nobs; i++) {
      sum += s[i].alfr;
    }
    //if (sum == nobs) {
    //  printf("water %10.5f %10.5f\n",(float) x.clat, (float) x.clon);
    //}
    //else if (sum == land) {
    //  printf("land %10.5f %10.5f\n",(float) x.clat, (float) x.clon);
    //}
    //if (sum != 0 && sum != nobs) {
    //  printf("%10.5f %10.5f  ",(float) x.clat, (float) x.clon);
    //  for (i = 0; i < nobs; i++) {
    //    printf("%5.3f ",s[i].alfr);
    //  }
    //  printf("\n");
    //}

    nread += 1;
    if (x.clat < 25 && x.clat > -40.0) continue;
    if (sum == land) continue;

    if (nobs == 2) {
      hr.head = x;
      for (i = 0; i < nobs; i++) {
        hr.obs[i] = s[i];
      }
    }
    else {
      lr.head = x;
      for (i = 0; i < nobs; i++) {
        lr.obs[i] = s[i];
      }
    }

    if (x.clat < 0 ) {
      ll.lat = (float) x.clat;
      ll.lon = (float) x.clon;
      loc = shh.locate(ll);
//      printf("%f %f sloc %d %d\n",ll.lat, ll.lon, loc.i, loc.j); fflush(stdout);
      if (shh.in(loc)) {
        suse += 1;
        // do something useful
        fwrite(&x, sizeof(x), 1, fout);
        fwrite(&s[0], sizeof(amsr2_spot), nobs, fout);
        if (nobs == 2) {
          sh_hr[loc] = hr;
        }
        else {
          sh_lr[loc] = lr;
        }
      }
      else {
        gridskip += 1;
      }
    }
    else {
      ll.lat = (float) x.clat;
      ll.lon = (float) x.clon;
      loc = nhh.locate(ll);
//      printf("%f %f nloc %d %d\n",ll.lat, ll.lon, loc.i, loc.j); fflush(stdout);
      if (nhh.in(loc)) {
        nuse += 1;
        // do something useful
        fwrite(&x, sizeof(x), 1, fout);
        fwrite(&s[0], sizeof(amsr2_spot), nobs, fout);
        if (nobs == 2) {
          nh_hr[loc] = hr;
        }
        else {
          nh_lr[loc] = lr;
        }
      }
      else {
        gridskip += 1;
      }
    }
  }

  printf("nuse = %d suse = %d nread = %d gridskip = %d\n",nuse, suse, nread, gridskip);


  return 0;
}
