#include "ncepgrids.h"

void checklats(llgrid<float> &iceconc, FILE *fout) ;

int main(int argc, char *argv[]) {
  FILE *fin, *fout;
  global_12th<float> newhigh, oldhigh;
  global_ice<float> oldhalf;
  ijpt loc;
  latpt ll;

  fin = fopen(argv[1],"r");
  newhigh.binin(fin);
  fclose(fin);

  fin = fopen(argv[2],"r");
  oldhigh.binin(fin);
  fclose(fin);

  fin = fopen(argv[3],"r");
  oldhalf.binin(fin);
  fclose(fin);

  fout = fopen("new.out","w");
  printf("New high resolution grid\n");
  checklats(newhigh, fout);
  fclose(fout);

  fout = fopen("old.out","w");
  printf("Old high resolution grid\n");
  checklats(oldhigh, fout);
  fclose(fout);

  fout = fopen("half.out","w");
  printf("Old low resolution grid\n");
  checklats(oldhalf, fout);
  fclose(fout);


  return 0;
}

void checklats(llgrid<float> &iceconc, FILE *fout) {
  latpt ll;
  ijpt loc;

// Check for southern hemisphere -- first point > 0 iceconc 
  for (loc.i = 0; loc.i < iceconc.xpoints(); loc.i++) {
    for (loc.j = iceconc.ypoints() - 1; loc.j > 0; loc.j--) {
      if (iceconc[loc] > 0 && iceconc[loc] <= 1.28) {
        ll = iceconc.locate(loc);
        fprintf(fout, "first ice at long %f is at lat %f, val = %f \n",ll.lon, ll.lat,
                 iceconc[loc]);
        break;
      }
    }
  }

  for (loc.j = 0; loc.j < iceconc.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < iceconc.xpoints(); loc.i++) {
    if (iceconc[loc] > 1.28) iceconc[loc] = 0;
    if (iceconc[loc] > 1.0) iceconc[loc] = 1.0;
  }
  }
  fprintf(fout, "global ice area %f\n",(float) iceconc.integrate()/1.e12 );

  for (loc.j = 0; loc.j < iceconc.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < iceconc.xpoints(); loc.i++) {
    if (iceconc[loc] > 0.0) iceconc[loc] = 1.0;
  }
  }
  fprintf(fout, "global ice extent %f\n",(float) iceconc.integrate()/1.e12 );

}
