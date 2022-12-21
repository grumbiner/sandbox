#include "ncepgrids.h"

#define MONTH_1 155
#define MONTH_2  47
#define MONTH_3  85


int main(int argc, char *argv[]) {
  northgrid<float> grid1;
  global_sst<float> grid2;
  global_ice<float> grid3, mask;

  FILE *fin, *fout;
  int i, j, totpts = 0, lag;
  ijpt tloc, loc;
  float landval = 200., nonval = 200.;
  palette<unsigned char> gg(19,65);
  char fname[900];

  fin = fopen(argv[1],"r");
  fout = fopen(argv[2],"w+");
// Get and flip the Nomura period 1/1979 to 11/1991
  for (i = 0; i < MONTH_1 ; i++) {
    grid1.binin(fin);
    for (loc.j = 0; loc.j < grid1.ypoints(); loc.j++) {
       tloc.j = grid1.ypoints() - 1 - loc.j;
    for (loc.i = 0; loc.i < grid1.xpoints(); loc.i++) {
       tloc.i = loc.i + 180; if (tloc.i > 359) tloc.i -= 360; // Shift also
       if (grid1[loc] < 200) {
         grid2[tloc] = grid1[loc];
       }
       else {
         grid2[tloc] = 0;
       }
    }
    }
    grid2.binout(fout);
  }

// Get and echo out the 1 degree period 12/1991 to 10/1995
  for (i = 0; i < MONTH_2 ; i++) {
    grid2.binin(fin);
    grid2.binout(fout);
  }

// Get and regrid the 0.5 degree period, 11/1995 to 2/2003
  for (i = 0; i < MONTH_3 ; i++) {
    grid3.binin(fin);
    grid2.fromall(grid3, mask, landval, nonval);
    grid2.binout(fout);
  }


  fclose(fin);
// Now try printing out everything:
  rewind(fout);
  for (i = 0; i < MONTH_1 + MONTH_2 + MONTH_3; i++) {
    grid2.binin(fout);
    sprintf(fname,"mon%3d.xpm",i);
    printf("%3d %f\n",i,grid2.gridmax() );
    grid2 *= 18;
    grid2.xpm(fname, 1, gg);
  }
  fclose(fout);
    
  return 0;
}
