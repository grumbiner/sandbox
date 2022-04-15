#include "ncepgrids.h"

#define hlen 300

int main(int argc, char*argv[]) {
  nsidcnorth<unsigned char> x;
  mvector<float> y(x.xpoints()*x.ypoints() );
  mvector<int> hist(256);
  unsigned char head[hlen];
  char header[300];
  float toler = 1.0;

  FILE *fin;

  fin = fopen(argv[1],"r");
  //fscanf(fin, "%s\n",header);
  //printf("%s\n",header);
  fseek(fin, 300, SEEK_SET);
  x.binin(fin);
  fclose(fin);
  //printf("%d %d\n",x.gridmax(), x.gridmin());

  printf("%d xpoints\n",y.xpoints() );
  for (int i = 0; i < y.xpoints(); i++) {
    y[i] = (float) x[i];
  }
  y *= 0.4; // nsidc is scaled, values > 250 are flags

  y.histogram(hist, toler);
  for (int i = 0; i < 256; i++) {
    printf("%3d  %6d\n",i,hist[i]);
  }

  return 0;
}
