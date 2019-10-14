#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin;
  global_12th<float> dist, grad;
  float flag = 0.;
  palette<unsigned char> gg(19, 65);
  ijpt loc;
  latpt ll;
  float maxgrad = 10.;

  fin = fopen(argv[1], "r");
  dist.binin(fin);
  fclose(fin);
  grad = dist;
  printf("dist %f %f %f %f\n",grad.gridmax(flag), grad.gridmin(flag), grad.average(flag), grad.rms(flag) );
  grad.scale();
  grad.xpm("dist.xpm", 7, gg);
  grad.set((float) 0.0);
  if (dist.gridmax() < 1e6) dist *= 1000.; // back to meters, so as to have gradient in meters per meter

  gradsq(dist, grad, flag);
  for (loc.j = 0; loc.j < grad.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < grad.xpoints(); loc.i++) {
    grad[loc] = sqrt(grad[loc]);
    if (grad[loc] > maxgrad) {
      ll = grad.locate(loc);
      printf("%d %d  %f %f  %f %f\n",loc.i, loc.j, ll.lat, ll.lon, dist[loc], grad[loc]);
      grad[loc] = maxgrad;
    }
  }
  }
  printf("grad %f %f %f %f\n",grad.gridmax(flag), grad.gridmin(flag), grad.average(flag), grad.rms(flag) );
  grad.scale();
  grad.xpm("grad.xpm", 7, gg);

  return 0;
}
