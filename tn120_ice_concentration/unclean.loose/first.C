#include <stdio.h>
#include <stdlib.h>

#include "icessmi.h"
#include "ncepgrids.h"

#define NPARMS 8
#define T19V 0
#define T19H 1
#define T22V 2
#define T37V 3
#define T37H 4
#define T85V 5
#define T85H 6
#define SURF 7
// better to use an enum

int main(int argc, char *argv[]) {
  bufr_line x;
  int i, j, hstart, hfinish;
  FILE *fin, *fout1, *fout2;
  mvector<northgrid<float> > nh(4*NPARMS);
  mvector<southgrid<float> > sh(4*NPARMS);
  northgrid<int> ncount[4];
  southgrid<int> scount[4];
  latpt ll;
  fijpt fij;
  ijpt loc;
  int base, trips = 0;
  float nonval = -999.;
  
  fin = fopen(argv[1], "r");
  hstart = atoi(argv[2]);
  hfinish = atoi(argv[3]);
  fout1 = fopen(argv[4],"w");
  fout2 = fopen(argv[5],"w");

  for (i = 0 ; i < 4; i++) {
    scount[i].set(0);
    ncount[i].set(0);
    for (j = 0 ; j < NPARMS; j++) {
      nh[i*NPARMS + j].set((float)0.0);
      sh[i*NPARMS + j].set((float)0.0);
    }
  }

  while (!feof(fin) ) {

    fread(&x, sizeof(bufr_line), 1, fin);

    printf("trips = %d hour = %d sat = %d\n", trips++, x.hour, x.satno-246);
    if (x.hour >= hstart && x.hour < hfinish) {

      base=(x.satno - 246);
      if (base < 0 || base > 3) {
	printf("wrong format\n");
	return 1;
      }

      for (i = 0; i < NSCANS; i++) {
        if (x.full[i].latitude > 20.0 && 
            x.full[i].latitude <= 90.0) {
          ll.lat = x.full[i].latitude;
          ll.lon = x.full[i].longitude;
          fij = ncount[base].locate(ll);
          loc = fij;
          if (ncount[base].in(loc) ) {
            ncount[base][loc] += 1;
            nh[base*NPARMS + T19V][loc] += x.full[i].t19v;
            nh[base*NPARMS + T19H][loc] += x.full[i].t19h;
            nh[base*NPARMS + T22V][loc] += x.full[i].t22v;
            nh[base*NPARMS + T37V][loc] += x.full[i].t37v;
            nh[base*NPARMS + T37V][loc] += x.full[i].t37v;
            nh[base*NPARMS + SURF][loc] += x.full[i].surface_type;
            nh[base*NPARMS + T85V][loc] += x.full[i].t85v;
            nh[base*NPARMS + T85H][loc] += x.full[i].t85h;
            for (j = 0; j < 3; j++ ) {
              nh[base*NPARMS + T85V][loc] += x.full[i].hires[j].t85v;
              nh[base*NPARMS + T85H][loc] += x.full[i].hires[j].t85h;
            }
          }
        }
        if (x.full[i].latitude < -20.0 &&
            x.full[i].latitude >= -85.0) {
          ll.lat = x.full[i].latitude;
          ll.lon = x.full[i].longitude;
          fij = scount[base].locate(ll);
          loc = fij;
          if (scount[base].in(loc) ) {
            scount[base][loc] += 1;
            sh[base*NPARMS + T19V][loc] += x.full[i].t19v;
            sh[base*NPARMS + T19H][loc] += x.full[i].t19h;
            sh[base*NPARMS + T22V][loc] += x.full[i].t22v;
            sh[base*NPARMS + T37V][loc] += x.full[i].t37v;
            sh[base*NPARMS + T37V][loc] += x.full[i].t37v;
            sh[base*NPARMS + SURF][loc] += x.full[i].surface_type;
            sh[base*NPARMS + T85V][loc] += x.full[i].t85v;
            sh[base*NPARMS + T85H][loc] += x.full[i].t85h;
            // note that we're not being precise about high freq locations
            for (j = 0; j < 3; j++ ) {
              sh[base*NPARMS + T85V][loc] += x.full[i].hires[j].t85v;
              sh[base*NPARMS + T85H][loc] += x.full[i].hires[j].t85h;
            }
          }
        }


      } // end of scan line

    } // end of being in data window
  } // done with data reading


  for (loc.j = 0; loc.j < ncount[0].ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < ncount[0].xpoints(); loc.i++) {
    for (j = 0; j < 4*NPARMS; j++) {
      if (ncount[j/NPARMS][loc] != 0) {
        if ( (j%NPARMS) == T85V || (j%NPARMS) == T85H ) {
          nh[j][loc] /= (ncount[j/NPARMS][loc]*4);
        }
        else {
          nh[j][loc] /= ncount[j/NPARMS][loc];
        }
      }
      else {
        nh[j][loc] = nonval;
      }
    }
  }
  }

  for (loc.j = 0; loc.j < scount[0].ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < scount[0].xpoints(); loc.i++) {
    for (j = 0; j < 4*NPARMS; j++) {
      if (scount[j/NPARMS][loc] != 0) {
        if ( (j%NPARMS) == T85V || (j%NPARMS) == T85H ) {
          sh[j][loc] /= (scount[j/NPARMS][loc]*4);
        }
        else {
          sh[j][loc] /= scount[j/NPARMS][loc];
        }
      }
      else {
        sh[j][loc] = nonval;
      }
    }
  }
  }


  for (i = 0; i < 4; i++) {
    printf("sat no %d nh max count = %d\n",i, ncount[i].gridmax() );
    printf("sat no %d sh max count = %d\n",i, scount[i].gridmax() );
  }
  for (i = 0; i < 4*NPARMS; i++) {
    nh[i].binout(fout1);
  }
  for (i = 0; i < 4*NPARMS; i++) {
    sh[i].binout(fout2);
  }

  return 0;
}
