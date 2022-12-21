#include "ncepgrids.h"
#include "resops.h"

int main(int argc, char *argv[]) {
  hycom<float> x;
  global_12th<float> ice, mask;
  global_12th<unsigned char> cconc, cmask;
  ijpt loc;
  float flagval = 157., nonval = 157.;
  FILE *fin, *landin, *fout;
  FILE *fout2, *fout2b;
  palette<unsigned char> gg(19,65);

  fin = fopen(argv[1],"r");
  cconc.binin(fin);
  fclose(fin);
  landin = fopen("seaice_gland5min","r");
  cmask.binin(landin);
  fclose(landin);
  for (loc.j = 0; loc.j < ice.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < ice.xpoints(); loc.i++) {
    ice[loc] = (float) cconc[loc];
    mask[loc] = (float) cmask[loc];
    if ((int) mask[loc] > 100) { 
      mask[loc] = flagval; 
      ice[loc] = flagval; 
    }
  }
  }

  x.fromall(ice, mask, flagval, nonval);
  fout = fopen(argv[2],"w");
  x.binout(fout);
  fclose(fout);
  x.xpm(argv[3],14,gg);

  fout2 = fopen(argv[4],"w");
  fout2b = fopen(argv[5],"w");
  x.outa(fout2);
  x.outb(fout2b,"icec");
  fclose(fout2);
  fclose(fout2b);

  return 0;
}
