#include "grid_math.h"
#include "mvector.h"

#define NT 600

int main(int argc, char *argv[]) {
  FILE *fin;
  float fmax, fmin, range;
  palette<unsigned char> gg(19,65), gray(32,65);
  grid2<float> x(64, NT), y(64,NT);

  fin = fopen(argv[1], "r");
  x.binin(fin);
  fclose(fin);

  fmax = x.gridmax();
  fmin = x.gridmin();
  range = fmax - fmin;
  for (int i = 0; i < 32; i++) {
    gray.set_color(i,8*i,8*i,8*i);
  }

  ijpt loc;
  for (loc.j = 0; loc.j < y.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < y.xpoints(); loc.i++) {
    y[loc] = (x[loc]-fmin)/range * 32.;
  }
  }

  x.scale();
  x.xpm("fout.xpm",7,gg);
  y.xpm("yout.xpm",1,gray);

// Now for the slices
  grid2<float> rho(64,64), rho_out(64, 64);
  char fname[90];

  fin = fopen(argv[2], "r");
  for (int i = 0; i < 640; i++) {
    rho.binin(fin);
    for (loc.j = 0; loc.j < rho.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < rho.xpoints(); loc.i++) {
      rho_out[loc] = (rho[loc]-fmin)/range * 32.;
    }
    }
    sprintf(fname,"step%03d.xpm",i);
    rho_out.xpm(fname, 1, gray);
  }
 

  return 0;
}
