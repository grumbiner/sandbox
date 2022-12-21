#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin;
  mvector<int> counts(256);
  GRIDTYPE<unsigned char> obsage;
  GRIDTYPE<float> tmp;
  ijpt loc;
  int age;
  double cumulative = 0.0, area = 0.0;

  fin = fopen(argv[1], "r");
  if (fin == (FILE *) NULL) {
    printf("Could not open %s\n",argv[1]);
    return 1;
  }
  obsage.binin(fin);
  fclose(fin);
  printf("age max, min, average %f %f %f\n",(float) obsage.gridmax(), (float) obsage.gridmin(), (float) obsage.average() );
  
  counts = 0;
  for (loc.j = 0; loc.j < obsage.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < obsage.xpoints(); loc.i++) {
    //if (obsage[loc] > 30) {
    //  printf("%3d %3d  %3d\n",loc.i, loc.j, obsage[loc]);
    //}
    counts[obsage[loc] ] += 1;
  }
  }
  printf("counts.xpoints = %d\n",counts.xpoints() );

  for (age = 0; age < counts.xpoints(); age++) {
    if (counts[age] != 0) {
      printf("age %3d days gridpt count %7d ",age, counts[age]);
      fflush(stdout);

      tmp.set((float)0.0);
      for (loc.j = 0; loc.j < obsage.ypoints(); loc.j++) {
      for (loc.i = 0; loc.i < obsage.xpoints(); loc.i++) {
        if (obsage[loc] == age) tmp[loc] = 1.0;
      }
      }
      area = tmp.integrate();
      cumulative += area;
      printf(" area = %7.3f  cumulative %7.3f\n",area/1.e12, cumulative/1.e12);
      //printf("\n");

      fflush(stdout);

    }
  }

  return 0;
}
