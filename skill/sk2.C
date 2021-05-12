#include "ncepgrids.h"
#include "skill.h"
// Robert Grumbine 10 December 2004

int main(int argc, char*argv[]) {
  skillfield<float> x(720, 360), y(720, 360), ever(720, 360);
  global_ice<float> count, avg;
  global_ice<unsigned char> land; 
  FILE *fin1, *fin2;
  float flagval = -99., nonval = -99.;
  int xcount = 0, ycount = 0;
  ijpt loc;
  
  fin1 = fopen(argv[1], "r");
  x.binin(fin1);
  fclose(fin1);
  fin2 = fopen(argv[2], "r");
  y.binin(fin2);
  fclose(fin2);
  fin1 = fopen(argv[3], "r");
  avg.binin(fin1);
  fclose(fin1);
  fin1 = fopen(argv[4], "r");
  count.binin(fin1);
  fclose(fin1);
  fin1 = fopen(argv[5], "r");
  land.binin(fin1);
  fclose(fin1);

  ever.set(0.0);
  for (loc.j = 0; loc.j < y.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < y.xpoints(); loc.i++) {
// Filter out flagged points of all kinds
    if (x[loc] > 1.28) x[loc] = 0;
    if (x[loc] > 1.00) x[loc] = 1.00;
    if (y[loc] > 1.28) y[loc] = 0;
    if (y[loc] > 1.00) y[loc] = 1.00;
    if (avg[loc] > 0.0) ever[loc] = 1.00;  
// Filter out land
    if (land[loc] > 1) {
      x[loc] = 0;
      y[loc] = 0;
    }
    if (x[loc] > 0.) xcount += 1;
    if (y[loc] > 0.) ycount += 1;
  }
  }
  
  printf("pod %f far %f ",x.pod(y), x.far(y) );

// Skip perennially covered points:
//  for (loc.j = 0; loc.j < y.ypoints(); loc.j++) {
//  for (loc.i = 0; loc.i < y.xpoints(); loc.i++) {
//    if (count[loc] > 363.) {
//      x[loc] = 0;
//      y[loc] = 0;
//      ever[loc] = 0;
//    }
//  }
//  }
//  printf(" no_per %f %f %f ",x.pod(y), x.far(y), ever.far(y) );

// Start with information:
  count /= count.gridmax();
  printf(" PID %f FIR %f  CI %f\n",x.pid(y, count), 
                  x.fir(y, count), x.correct_info(y, count)  );

  return 0;
}
