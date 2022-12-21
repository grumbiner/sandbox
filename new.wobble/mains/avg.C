#include "grid_math.h"

#define NX 192
#define NY 94

int main(int argc, char *argv[]) {
  grid2<float> x(NX, NY);
  mvector<double> sx(NX * NY), sxx(NX * NY), sx3(NX*NY), sx4(NX*NY);
  mvector<double> var(NX*NY), avg(NX*NY), skew(NX*NY), kurt(NX*NY);

  FILE *fin;
  int i, j, count = 0;
  ijpt loc;

  fin = fopen("temps","r");
  sx = 0.0;

  count = 0;
  while (!feof(fin) && count < 90000) {
    x.binin(fin);
    if (!feof(fin)) {
      x -= 273.15; // remove this to aid the precision in the summations.
      count += 1;
      
      for (i = 0; i < sxx.xpoints(); i++) {
         if (x[i] < -115) {
           printf("count %d i %d x %f x+263 %f\n",count, i,x[i], x[i]+273.15); fflush(stdout);
         }
         sx[i] += (double) x[i];
      }
    }
  }
  printf("Found %d grids\n",count); fflush(stdout);

// Having found the average, re-run and subtract the average
  rewind(fin);
  avg = sx;
  avg /= (double) count;
  printf("first average of all files %f %f %f\n",avg.average(), avg.minimum(), avg.maximum() );
  fflush(stdout);
  sx = 0.0;
  sxx = 0.0;
  for (i = 0; i < count; i++) {
    x.binin(fin);
    x -= 273.15;  // have to re-subtract this;
    for (j = 0; j < sx.xpoints(); j++) {
       x[j]   -= avg[j];
       sx[j]  += (double) x[j];
       sxx[j] += x[j]*x[j];
       sx3[j] += x[j]*x[j]*x[j];
       sx4[j] += x[j]*x[j]*x[j]*x[j];
    }
  }
  fclose(fin);

  sx /= (double) count;
  printf("second average of all files %f %f %f\n",sx.average(), sx.minimum(), sx.maximum() );
  printf("average of all files %f %f %f\n",avg.average(), avg.minimum(), avg.maximum() );

// note that some of the following have 0 terms (written in) because the mean of x has been reset
//  to 0 by the double-round statistics
  for (i = 0; i < sx.xpoints(); i++) {
    var[i] = (sxx[i] ) / count;
    skew[i] = (sx3[i] - 3*0 + 3*0 - count*0 ) / count;
    kurt[i] = (sx4[i] - 4*0 + 6*0 - 4*0 + 0 ) / count; 
    if (var[i] > 1.e-2) {
      skew[i] = skew[i] / pow(fabs(var[i]),1.5);
      kurt[i] = kurt[i] / (var[i]*var[i])         - 3.0;
    }
    else {
      skew[i] = 0.0;
      kurt[i] = 0.0;
    }

    printf("%5d  %f %f %f %f\n",i, avg[i], var[i], skew[i], kurt[i]);
  }
 
  fin = fopen("statsout","w");
  avg.binout(fin);
  var.binout(fin);
  skew.binout(fin);
  kurt.binout(fin);
  fclose(fin);

  return 0;
}  
