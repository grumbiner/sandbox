#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  mrf1deg<float> x, y, del;
  FILE *fops, *ftest;
  int i = 0, count = 0, bigcount = 0;
  float xavg, yavg, ref;
  ijpt loc;
  palette<unsigned char> gg(19, 65);
  char fname[90];

  fops = fopen(argv[1], "r");
  ftest = fopen(argv[2], "r");

  //while (!feof(fops) && i == 0 ) {
  while (!feof(fops) ) {
    count = 0;
    bigcount = 0;
    x.binin(fops);
    y.binin(ftest);
    xavg = x.rms(0.0);
    yavg = y.rms(0.0);

    if (fabs(yavg - xavg) > 0.25*min(xavg, yavg) ) {
      printf("major systematic difference between ref and test grids\n");
      printf("step %d rms is %e %e, respectively\n",i, xavg, yavg);
      //exit(1);
    }
      

    for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
      if (x[loc] != 0 || y[loc] != 0) count++;
      ref = fabs(x[loc]) + fabs(y[loc]);
      if (fabs(x[loc] - y[loc]) > 0.25*ref ) {
        //printf("step %d diff at %d %d  %e vs. %e on %e\n",i, 
        //            loc.i, loc.j, x[loc], y[loc], ref); 
        bigcount++;
      }
    }
    }
    
    del = x;
    del -= y;

    printf("step %d xrms %e yrms %e delta rms %e\n",i, x.rms(0.0), y.rms(0.0), del.rms(0.0) );
    printf("step %d allcount %d big count %d\n",i, count, bigcount);
    x.scale();
    y.scale();
    del.scale();
    sprintf(fname,"ops%d.xpm",i);
    x.xpm(fname,7,gg);
    sprintf(fname,"test%d.xpm",i);
    y.xpm(fname,7,gg);
    sprintf(fname,"del%d.xpm",i);
    del.xpm(fname,7,gg);

    i++;
  }
  
  return 0;
} 
