#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin;
  mvector<int> counts(256);
  global_ice<unsigned char> obsage;
  global_ice<float> tmp;
  global_ice<float> sum, count;
  ijpt loc;
  int age;
  double cumulative = 0.0, area = 0.0;

  fin = fopen(argv[1], "r");
  sum.set(0);
  count.set((float)0);

  while (!feof(fin) ) {
    obsage.binin(fin);
    printf("age max, min, average %f %f %f\n",(float) obsage.gridmax(255), (float) obsage.gridmin(255), (float) obsage.average(255) );

  
    if (feof(fin)) break;

    for (loc.j = 0; loc.j < obsage.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < obsage.xpoints(); loc.i++) {
      counts[obsage[loc] ] += 1;
      if (obsage[loc] != 255) {
        sum[loc] += obsage[loc]; 
        count[loc] += 1; 
      }
    }
    }

  }
  fclose(fin);


  for (loc.j = 0; loc.j < obsage.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < obsage.xpoints(); loc.i++) {
    if (count[loc] != 0) {
      sum[loc] /= count[loc];
    }
    else {
      sum[loc] = 255;
    }
  }
  }



  for (age = 0; age < counts.xpoints(); age++) {
    tmp.set((float)0.0);
    for (loc.j = 0; loc.j < obsage.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < obsage.xpoints(); loc.i++) {
      if (sum[loc] >= age && sum[loc] < age+1) tmp[loc] = 1.0;
    }
    }
    area = tmp.integrate();
    if (area != 0) {
      cumulative += area;
      printf("%3d area = %7.3f  cumulative %7.3f\n",
            age, area/1.e12, cumulative/1.e12);
      fflush(stdout);
    }
  }

  FILE *fout;
  fout = fopen(argv[2], "w");
  sum.binout(fout);
  fclose(fout);

  return 0;
}
