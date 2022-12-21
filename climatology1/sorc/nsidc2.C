#include "ncepgrids.h"

#define hlen 300

int main(int argc, char*argv[]) {
  nsidcnorth<unsigned char> x;
  mvector<float> y(x.xpoints()*x.ypoints() );
  mvector<int> hist(256), sum(256);
  float toler = 1.0;
  int i, j;

  FILE *fin;

  sum = 0;

  for (i = 1; i < argc; i++) {
    fin = fopen(argv[i],"r");
    fseek(fin, hlen, SEEK_SET);
    x.binin(fin);
    fclose(fin);
  
    for (j = 0; j < y.xpoints(); j++) {
      y[j] = (float) x[j];
    }

    y.histogram(hist, toler);
    sum += hist;

  }

  long int accum = 0;
  for (i = 1; i <= 250; i++) {
    accum += sum[i];
    printf("%3d  %9d %ld\n",i,sum[i],accum);
  }

  return 0;
}
