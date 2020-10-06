#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin, *fold;
  mvector<int> counts(256), old_counts(256);
  GRIDTYPE<unsigned char> obsval, oldval;
  GRIDTYPE<float> tmp, old_tmp;
  ijpt loc;
  int age;
  double cumulative = 0.0, area = 0.0;
  double old_cumulative = 0.0, old_area = 0.0;

  fin = fopen(argv[1], "r");
  if (fin == (FILE *) NULL) {
    printf("Could not open %s\n",argv[1]);
    return 1;
  }
  obsval.binin(fin);
  fclose(fin);

  fold = fopen(argv[2], "r");
  if (fold == (FILE *) NULL) {
    printf("Could not open %s\n",argv[2]);
    return 1;
  }
  oldval.binin(fold);
  fclose(fold);

// Gross check:
  printf("new max, min, average %f %f %f\n",(float) obsval.gridmax(), (float) obsval.gridmin(), (float) obsval.average() );
  
  printf("new max, min, average %f %f %f\n",(float) oldval.gridmax(), (float) oldval.gridmin(), (float) oldval.average() );


// Histogram check:
  counts = 0;
  old_counts = 0;
  for (loc.j = 0; loc.j < obsval.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < obsval.xpoints(); loc.i++) {
    counts[obsval[loc] ] += 1;
    old_counts[oldval[loc] ] += 1;
  }
  }

  for (age = 0; age < counts.xpoints(); age++) {
    if (counts[age] != 0) {
      printf("val %3d days gridpt count %6d ",age, counts[age]);
      fflush(stdout);

      tmp.set((float)0.0);
      for (loc.j = 0; loc.j < obsval.ypoints(); loc.j++) {
      for (loc.i = 0; loc.i < obsval.xpoints(); loc.i++) {
        if (obsval[loc] == age) tmp[loc] = 1.0;
      }
      }
      area = tmp.integrate();
      cumulative += area;
      printf(" area = %7.3f  cumulative %7.3f\n",area/1.e12, cumulative/1.e12);

      fflush(stdout);

    }

    if (old_counts[age] != 0) {
      printf("old %3d days gridpt count %6d ",age, old_counts[age]);
      fflush(stdout);

      old_tmp.set((float)0.0);
      for (loc.j = 0; loc.j < obsval.ypoints(); loc.j++) {
      for (loc.i = 0; loc.i < obsval.xpoints(); loc.i++) {
        if (oldval[loc] == age) old_tmp[loc] = 1.0;
      }
      }
      old_area = old_tmp.integrate();
      old_cumulative += old_area;
      printf(" area = %7.3f  cumulative %7.3f\n",old_area/1.e12, old_cumulative/1.e12);

      fflush(stdout);

    }

  }


// Pointwise check:
    latpt ll;
    for (loc.j = 0; loc.j < obsval.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < obsval.xpoints(); loc.i++) {
      if (oldval[loc] != obsval[loc]) {
        ll = obsval.locate(loc);
        printf("pt %8.4f N %8.4f E old %3d new %3d\n",ll.lat, ll.lon, 
                  oldval[loc], obsval[loc]);
      }
    }
    }

    
  return 0;
}
