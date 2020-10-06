#include "mvector.h"
#include "time_series.h"

#define MAXLEN (365*12)

int main(int argc, char* argv[]) {
  FILE *fin;
  time_series<float> conc(MAXLEN);
  time_series<float> test(MAXLEN);
  time_series<float> real(MAXLEN), im(MAXLEN);
  int i, npts;
  float avg;

  fin = fopen(argv[1],"r");
  i = 0;
  while (!feof(fin) ) {
    fscanf(fin,"%f %f\n",&conc[i], &test[i]);
    i++;
  }
  fclose(fin);
  fflush(stdout);

  npts = i - 1;
  test.resize(npts);
  for (i = 0; i < npts; i++) {
    test[i] = conc[i];
  }
  avg = test.average();
  test.mean = avg;
  test -= avg;
  printf("%d points in series, mean = %f\n",npts, avg);
  fflush(stdout);

  printf("\n Autocovariance\n");
  for (i = 1; i < 3.*npts/4; i++) {
    printf("%d auto %f\n",i, test.autocovary(i) );
  } 

  return 0;
}
