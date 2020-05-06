#include "ncepgrids.h"
//5 Aug 2004  Robert Grumbine

int main(int argc, char *argv[]) {
  global_12th<unsigned char> indat;
  global_12th<float> land, outdat;
  FILE *fin, *fout;
  ijpt loc;
  latpt ll, ur;
  ijpt ill, iur;

  fin = fopen(argv[1],"r");
  indat.binin(fin);
  fclose(fin);
  fin = fopen(argv[2],"r");
  land.binin(fin);
  fclose(fin);

  ur.lat = 40.0;
  ll.lon = -94.0;
  ll.lat = 51.0;
  ur.lon = -75.0;
  iur = land.locate(ur);
  ill = land.locate(ll);
  //printf("ll %d %d  ur %d %d\n",ill.i, ill.j, iur.i, iur.j); 

  for (loc.j = 0; loc.j < indat.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < indat.xpoints(); loc.i++) {
    outdat[loc] = (float) indat[loc];
    if ((land[loc] == 0 && outdat[loc] != 0) ||
        (land[loc] == 1 && outdat[loc] != 0) ) {
      //printf("land %f  ice %f\n",land[loc],outdat[loc]);
      outdat[loc] = 0;
    }
    if (land[loc] == 18 && outdat[loc] != 0)  { 
      outdat[loc] = 224;
    }
  }
  }

  fout = fopen(argv[3],"w");
  outdat.binout(fout);
  fclose(fout);

  return 0;
}
