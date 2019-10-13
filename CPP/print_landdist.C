#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin;
  global_12th<float> distances;
  latpt ll;
  float tlow, tnav, thi;
  float tmplat, tmplon;
  ijpt loc;
  

  fin = fopen(argv[1], "r");
  distances.binin(fin);
  fclose(fin);

  fin = fopen(argv[2], "r");
  while (!feof(fin)) {
    char type[90];
    int nmatch, land1, land2, land3, icecon, del;
    fscanf(fin, "%s %f %f %d %d %d %d %d %f %f %f %f\n",&type[0], &tmplat, &tmplon,
       &nmatch, &land1, &land2, &land3, &icecon, &tlow, &tnav, &thi, &del);
    ll.lat = tmplat, ll.lon = tmplon;
    loc = distances.locate(ll);
    if (distances[loc] != 0.0) {
      if (tnav > 267) {
        printf("%6.2f %6.2f  %4d %4d  %6.2f %6.2f %6.2f  %6.2f %6.2f  %8.2f km\n",
          ll.lat, ll.lon, loc.i, loc.j, tlow, tnav, thi, thi - tnav, thi - tlow, distances[loc]/1e3);
      }
      else { 
        printf("%6.2f %6.2f  %4d %4d  %6.2f %6.2f %6.2f  ------ %6.2f  %8.2f km\n",
          ll.lat, ll.lon, loc.i, loc.j, tlow, tnav, thi, thi - tlow, distances[loc]/1e3);
      }
    }
  }
  fclose(fin);

  return 0;
}
