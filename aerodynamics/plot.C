#include "grid_math.h"

void vor(grid2<float> &u, grid2<float> &v, grid2<float> &zeta) ;


int main(int argc, char *argv[]) {
  FILE *fin;
  grid2<float> rho(NX, NY), u(NX, NY), v(NX, NY), zeta(NX, NY);
  palette<unsigned char> gray(64);
  int i = 0;
  char fname[900];
  
  for (i = 0; i < 64; i++) {
    gray.set_color(i,i*4,i*4,i*4);
  }

  i = 0;
  fin = fopen(argv[1],"r");
  while (!feof(fin) ) {
    rho.ftnin(fin); u.ftnin(fin); v.ftnin(fin);
    if (!feof(fin) ) {
      vor(u, v, zeta);

      printf("rho %d  %f %f %f %f\n",i,rho.gridmax(), rho.gridmin(), rho.average(), rho.rms() );
      printf("zeta %d  %f %f %f %f\n",i,zeta.gridmax(), zeta.gridmin(), zeta.average(), zeta.rms() );
      printf("uv %d  %f %f %f %f  %f %f %f %f  %f\n",i,u.gridmax(), u.gridmin(), u.average(), u.rms(), v.gridmax(), v.gridmin(), v.average(), v.rms(), sqrt(v.rms()*v.rms() + u.rms()*u.rms()) );
     


      rho.scale(); u.scale(); v.scale(), zeta.scale();

      sprintf(fname, "rho%d.xpm",i);
      rho.xpm(fname, 4, gray);
      sprintf(fname, "u%d.xpm",i);
      u.xpm(fname, 4, gray);
      sprintf(fname, "v%d.xpm",i);
      v.xpm(fname, 4, gray);
      sprintf(fname, "zeta%d.xpm",i);
      zeta.xpm(fname, 4, gray);

      i++;
    }
  }

  return 0;
}
void vor(grid2<float> &u, grid2<float> &v, grid2<float> &zeta) {
  ijpt loc, ip, im, jp, jm;

  zeta.set((float) 0.0);
  for (loc.j = 1; loc.j < u.ypoints() - 1; loc.j++) {
  for (loc.i = 1; loc.i < u.xpoints() - 1; loc.i++) {
    ip = loc; ip.i += 1;
    jp = loc; jp.j += 1;
    im = loc; im.i -= 1;
    jm = loc; jm.i -= 1;
    zeta[loc] = u[jp] - u[jm] + v[ip] - v[im];
  }
  }
  zeta /= 2.;

  return;
}
