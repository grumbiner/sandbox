#include "mvector.h"
#include "time_series.h"

#define MAXLEN (365*12)

int main(int argc, char* argv[]) {
  FILE *fin;
  time_series<float> conc1(MAXLEN), conc2(MAXLEN);
  time_series<float> test1(MAXLEN), test2(MAXLEN);
  time_series<float> year1(365), year2(365), n1(365), n2(365);
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

// Now compute, print, and subtract off, the mean annual cycle observed:
  year1.set(0.0);
  year2.set(0.0);
  n1.set(0.0);
  n2.set(0.0);
  for (i = 0; i < npts; i++) {
    year1[i%365] += test1[i];
    year2[i%365] += test2[i];
    n1[i%365] += 1;
    n2[i%365] += 1;
  }
  for (i = 0; i < 365; i++) {
    year1[i] /= n1[i]; test1[i] -= year1[i];
    year2[i] /= n2[i]; test2[i] -= year2[i];
    //printf("day %3d   %f %f  %f %f\n",i, year1[i], year2[i], n1[i], n2[i]);
  }

  avg = test1.average();
  test1.mean = avg;
  test1 -= avg;
  avg = test2.average();
  test2.mean = avg;
  test2 -= avg;

  printf("\n Cross covariance and correl\n");
  for (i = -400; i < 401; i++) {
    printf("%d cross %f %f\n",i, test1.crossvary(test2, i), test1.crosscorrel(test2, i)  );
  } 

  return 0;
}
