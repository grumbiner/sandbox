#include <stdio.h>
#include "icessmi.h"

int cmc_check(bufr_line *b) {
// Perform qc in the vein of the CMC.  Assume that NCEP's check has already
//   been called and see how often CMC flags something that ncep doesn't

  int nerr = 0, i, n19v=0, n19h=0, n37v=0, n37h=0, del19=0, del37=0;

  for (i = 0; i < NSCANS; i++) {
    // if t19h is zero, NCEP flagged it as a bad point.
    if (b->full[i].t19h == 0) continue;
    if (b->full[i].t19v <= 151.0) {
      nerr += 1;
      n19v += 1;
      zero_bufr(b,i);
    }
    if (b->full[i].t19h <= 92.0) {
      nerr += 1;
      n19h += 1;
      zero_bufr(b,i);
    }
    if (b->full[i].t37v <= 171.0) {
      nerr += 1;
      n37v += 1;
      zero_bufr(b,i);
    }
    if (b->full[i].t37h <= 125.0) {
      nerr += 1;
      n37h += 1;
      zero_bufr(b,i);
    }
    if ( (b->full[i].t19v - b->full[i].t19h) >= 84.0) {
      nerr += 1;
      del19 += 1;
      zero_bufr(b,i);
    }
    if ( (b->full[i].t37v - b->full[i].t37h) >= 80.0) {
      nerr += 1;
      del37 += 1;
      zero_bufr(b,i);
    }

    if (nerr != 0) {
      printf("cmc caught %d in %d %d  %d %d  %d %d\n",nerr, n19v, n19h, n37v, n37h, del19, del37);
    }

  }

  return nerr;
}
