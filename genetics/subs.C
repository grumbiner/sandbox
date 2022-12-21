#include "ncepgrids.h"
#include "ssmipt.h"


void getin(grid2<ssmipt> &x) {
  FILE *fin;
  fin = fopen("n3ssmi.990507","r");
  x.binin(fin);
  return;
}

#include "points.h"

void cellprint(northgrid<float> &x, ijpt &loc, FILE *fout) {
    ijpt tloca, tlocx, delloc, base;
    grid2<unsigned char> a(3,3);

    base.j = 1;
    base.i = 1;
    for (delloc.j = -1; delloc.j <= +1; delloc.j++) {
    for (delloc.i = -1; delloc.i <= +1; delloc.i++) {
       tlocx = loc ; tlocx += delloc;
       tloca = base ; tloca += delloc;
       a[tloca] = (unsigned char) (0.5 + 100. * x[tlocx]);
    }
    }
    a.binout(fout);

    return;
}
