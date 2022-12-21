#include "grid_math.h"

#define NX 94
#define NY 192

int main(int argc, char *argv[]) {
  grid2<float> x(NX, NY);
  mvector<double> sx(NX * NY), sxx(NX * NY);
// Note that y dimension _should_ be sx.xpoints.  Since it isn't,
//   we need to run a second time, and that time around be working with
//   the second half of the y range.
  grid2<double> sxy(sx.xpoints(), sx.xpoints()/2);

  FILE *fin;
  int i, j, count = 0;
  ijpt loc;

  fin = fopen("temps","r");
  sx = 0.0;
  sxy = 0.0;

  count = 0;
  while (!feof(fin) && count < 8) {
    x.binin(fin);
    if (!feof(fin)) {
      x -= 273.15; // remove this to aid the precision in the summations.
      count += 1;
      
      for (i = 0; i < sxx.xpoints(); i++) {
         sx[i] += (double) x[i];
        sxx[i] += x[i]*x[i];
      }

      
      for (loc.i = 0; loc.i < sxy.xpoints(); loc.i++) {
      // only do from i to ny, as the grids are symmetric (and this is slow)
      for (loc.j = loc.i; loc.j < sxy.ypoints(); loc.j++) {
        sxy[loc] += x[loc.i]*x[loc.j];
      }
      }

    }
  }
  fclose(fin);
  printf("Found %d grids\n",count); fflush(stdout);

  //return 0;


  sx /= (double) count;
  printf("average of all files %f %f %f\n",sx.average(), sx.minimum(), sx.maximum() );
  fflush(stdout);

// also output mean, variance, rms, terms used in divisions
  fin = fopen("stats","w");
  sx.binout(fin);
  sxx.binout(fin);
  sxy.binout(fin);
  fclose(fin);

  fin = fopen("stats2", "w");
  double a, b, r;
  ijpt xyloc;

// Note that we're only going up (in i) to ypoints, not the full xpoints -- get the
//   higher values from the symmetry -- next block.
  for (i = 0; i < sxy.ypoints(); i++) {
  for (j = i; j < sxy.ypoints(); j++) {
    xyloc.i = i; xyloc.j = j;
    b = (sxy[xyloc] - count * sx[i]*sx[j])/(sxx[i] - count*sx[i]*sx[i]);
    a = sx[j] - b*sx[i];
    r = (sxy[xyloc] - count * sx[i]*sx[j])/sqrt(sxx[i] - count*sx[i]*sx[i])/
                                           sqrt(sxx[j] - count*sx[j]*sx[j]);
    fwrite(&i, sizeof(int), 1, fin);
    fwrite(&j, sizeof(int), 1, fin);
    fwrite(&a, sizeof(double), 1, fin);
    fwrite(&b, sizeof(double), 1, fin);
    fwrite(&r, sizeof(double), 1, fin);

  }
  printf("i %5d j %5d  a b r %f %f %f\n",i, j, a, b, r);
  fflush(stdout);
  }

// Now take advantage of symmetry
  for (i = sxy.ypoints(); i < sxy.xpoints(); i++) {
  for (j = 0; j <= i; j++) {
    xyloc.i = i; xyloc.j = j;
    b = (sxy[xyloc] - count * sx[i]*sx[j])/(sxx[i] - count*sx[i]*sx[i]);
    a = sx[j] - b*sx[i];
    r = (sxy[xyloc] - count * sx[i]*sx[j])/sqrt(sxx[i] - count*sx[i]*sx[i])/
                                           sqrt(sxx[j] - count*sx[j]*sx[j]);
    fwrite(&i, sizeof(int), 1, fin);
    fwrite(&j, sizeof(int), 1, fin);
    fwrite(&a, sizeof(double), 1, fin);
    fwrite(&b, sizeof(double), 1, fin);
    fwrite(&r, sizeof(double), 1, fin);

  }
  printf("i %5d j %5d  a b r %f %f %f\n",i, j, a, b, r);
  fflush(stdout);
  }


  return 0;
}  
