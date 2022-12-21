#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  northgrid<float> ice;
  northgrid<float> valid, v2;
  FILE *fin, *fout;
  int i;
  ijpt loc;
  palette<unsigned char> gg(19,65);
  bool weird;

  valid.set((float)0.0);
  v2.set((float)0.0);

  printf("argc = %d\n",argc);
  for (i = 1; i < argc; i++) {
    fin = fopen(argv[i],"r");
    ice.binin(fin);
    fclose(fin);
    for (loc.j = 0; loc.j < ice.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < ice.xpoints(); loc.i++) {
      if (ice[loc] > 0. && ice[loc] <= 1.00) valid[loc] += 1;
    }
    }
  }
    
  for (loc.j = 0; loc.j < ice.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < ice.xpoints(); loc.i++) {
    if (valid[loc] < argc/50 && valid[loc] > 0) {
      v2[loc] = 15;
    }
    else {
      v2[loc] = 0;
    }
  }
  }

// Now go back through and see what's going on for those days that have apparently valid
//  ice at points that it shouldn't:
//  for (i = 1; i < argc; i++) {
//    fin = fopen(argv[i],"r");
//    ice.binin(fin);
//    fclose(fin);
//    weird = false;
//    for (loc.j = 0; loc.j < ice.ypoints(); loc.j++) {
//    for (loc.i = 0; loc.i < ice.xpoints(); loc.i++) {
//       if (v2[loc] != 0 && ice[loc] > 0. && ice[loc] <= 1.00) {
//         weird = true;
//      }
//    }
//    }
//    if (weird) printf("%s is weird\n",argv[i]);
//  }

  printf("valid max, min, %f %f \n",valid.gridmax(), valid.gridmin() );
  fout = fopen("validcount","w");
  valid.binout(fout);
  fclose(fout);

  valid.scale();
  valid.xpm("valid.xpm",7,gg);
  v2.xpm("v2.xpm",1,gg);

  return 0;
}
