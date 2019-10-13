#include <stdio.h>

#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  northhigh<unsigned char> mask, flags;
  FILE *fin1, *fin2;
  ijpt loc;
  float f1, f2, f3;
  int i1, i2;
  palette<unsigned char> gg(19,65);
  float minlat = 23.91;
  float slope  = 0.5 / 40.0; // top is %, bottom is number of degrees to phase
  float mincut = 0.01;

  fin1 = fopen(argv[1],"r");
  mask.binin(fin1);
  fclose(fin1);
  fin2 = fopen(argv[2],"r");
  
  while (!feof(fin2) ) {
    fscanf(fin2, "%d %d %f %f %d %d %f\n",&loc.i ,&loc.j, &f1, &f2, 
                        &i1, &i2, &f3);
//  If the coverage fraction is too high for the latitude, and it is 
//     currently water
    if (f1 < 67.0 && f3 > slope*(f1 - minlat) + mincut && mask[loc] == 0 ) {
       printf("masking %3d %3d  %5.2f %7.2f at %5.3f percent\n",
                  loc.i, loc.j, f1, f2, f3);
       mask[loc] = 30;
    }
// Coast removal
    if (f1 < 67.0 && f3 < slope*(f1 - minlat)/2. && mask[loc] == 195 ) {
       printf("decoasting %3d %3d  %5.2f %7.2f at %5.3f percent\n",
                  loc.i, loc.j, f1, f2, f3);
       mask[loc] = 60;
    }
  }

  mask.xpm("new.xpm",11,gg);
 
  return 0;
}
// i   j     lat    lon    flag  N  %
//115 886   33.70  131.88    0   18 0.058
//108 891   32.94  132.26    0   18 0.058
// 94 871   33.79  134.83    0   18 0.058
// 85  24   31.36 -114.08    0   18 0.058
// 83 866   33.57  136.19    0   18 0.058
// 83 862   33.88  136.46    0   18 0.058
// 77   8   29.72 -113.84    0   18 0.058
// 68  51   32.49 -117.34    0   18 0.058
// 62  75   33.92 -119.56    0   18 0.058
// 58  93   34.99 -121.26    0   18 0.058
