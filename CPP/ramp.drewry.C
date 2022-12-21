#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  ramp_low<unsigned short int> tmp;
  ramp_low<float> aatopo;
  drewry_grid<float> bas83, mapped, count;
  FILE *fin;
  float flagval = -1, nonval = -99;
  ijpt loc, l2;
  latpt ll;
  palette<unsigned char> gg(19,65);
  char var[11];
  fijpt fij;

// Get the Drewry low-resolution data
  //aatopo.show();
  bas83.show();
  fin = fopen(argv[1], "r");
  loc.j = 0;
  for (loc.i = 0; loc.i < bas83.xpoints(); loc.i++) {
     fscanf(fin, "%5c",&var[0]);
     bas83[loc] = atof(var);
  }

  for (loc.j = bas83.ypoints() - 1; loc.j >= 0; loc.j--) {
    fscanf(fin, "%6c",&var[0]);
    var[6] = '\0';
    for (loc.i = 0; loc.i < bas83.xpoints(); loc.i++) {
       fscanf(fin, "%10c",&var[0]);
       var[10] = '\0';
       bas83[loc] = atof(var);
       if (bas83[loc] == flagval) bas83[loc] = 0.0;
       ll = bas83.locate(loc);
    }
  }
  bas83 *= 1000.;

// Get the RAMP topography
  fin = fopen(argv[2], "r");
  tmp.binin(fin);
  fclose(fin);
  for (loc.j = 0; loc.j < aatopo.ypoints(); loc.j++) {
    l2.j = aatopo.ypoints() - 1 - loc.j;
  for (loc.i = 0; loc.i < aatopo.xpoints(); loc.i++) {
    l2.i = loc.i;
    aatopo[l2] = (float) tmp[loc];
  }
  }
  printf("grid max, min, average, rms %f %f %f %f\n",
    aatopo.gridmax(), aatopo.gridmin(), aatopo.average(), aatopo.rms() );

// Comparisons, processings
  count.set((float) 0.0);
  mapped.set((float) 0.0);
  for (loc.j = 0; loc.j < aatopo.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < aatopo.xpoints(); loc.i++) {
    ll = aatopo.locate(loc);
    fij = bas83.locate(ll);
    mapped[fij] += aatopo[loc];
    count[fij]  += 1;
  }
  }
  for (loc.j = 0; loc.j < bas83.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < bas83.xpoints(); loc.i++) {
    if (count[loc] != 0) mapped[loc] /= count[loc];
    printf("%3d %3d  %f %f  %f\n",loc.i, loc.j, bas83[loc], mapped[loc], bas83[loc] - mapped[loc]);
  }
  }

  mapped -= bas83;
  mapped.scale();
  mapped.xpm("delta.xpm",7,gg);

  return 0;
}
