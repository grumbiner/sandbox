#include "mvector.h"
#include "time_series.h"

#define MAXLEN (365*12)

int main(int argc, char* argv[]) {
  FILE *fin;
  time_series<float> conc1(MAXLEN), conc2(MAXLEN);
  time_series<float> test1(MAXLEN), test2(MAXLEN);
  int i, npts;
  float avg;

  fin = fopen(argv[1],"r");
  i = 0;
  while (!feof(fin) ) {
    fscanf(fin,"%f %f\n",&conc1[i], &test1[i]);
    i++;
  }
  fclose(fin);
  fflush(stdout);

  fin = fopen(argv[2],"r");
  i = 0;
  while (!feof(fin) ) {
    fscanf(fin,"%f %f\n",&conc2[i], &test2[i]);
    i++;
  }
  fclose(fin);
  fflush(stdout);

  npts = i - 1;
  test1.resize(npts);
  test2.resize(npts);
  for (i = 0; i < npts; i++) {
    test1[i] = conc1[i];
    test2[i] = conc2[i];
  }
  avg = test1.average();
  test1.mean = avg;
  test1 -= avg;
  avg = test2.average();
  test2.mean = avg;
  test2 -= avg;

  printf("\n Cross covariance\n");
  for (i = -400; i < 401; i++) {
    printf("%d cross %f %f\n",i, test1.crossvary(test2, i), test1.crosscorrel(test2, i)  );
  } 

  return 0;
}
