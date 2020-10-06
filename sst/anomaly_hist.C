#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin;
  global_quarter<float> tmp;
  //global_quarter<mvector<int> > histo;
  mvector<int> histogram(1000);
  ijpt loc;
  int i;

  histogram = 0;
  //printf("argc = %d\n",argc); fflush(stdout);

  for (i = 1; i < argc; i++) {
    fin = fopen(argv[i],"r");
    tmp.binin(fin);
    fclose(fin);
    tmp /= 100.;
    //printf("tmp %f %f %f\n",tmp.gridmax(), tmp.gridmin(), tmp.average() ); fflush(stdout);

    for (loc.j = 0; loc.j < tmp.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < tmp.xpoints(); loc.i++) {
      histogram[ (int) nearbyint(tmp[loc]*10. + 400) ] += 1;
    }
    }

  }

  long long int cumulative = 0;
  for (i = 0; i < histogram.xpoints(); i++) {
    if (histogram[i] != 0) {
      if ((float)i / 10.0 - 40. != 0) {
        cumulative += histogram[i];
        printf("%6.2f %d %lld\n",(float)i / 10.0 - 40., histogram[i], cumulative );
      }
    }
  }

  return 0;
}
