#include "ncepgrids.h"

int main(void) {
  ijpt loc;
  northgrid<float> x;
  latpt ll;
  loc.i = 240; loc.j = 230;
  ll = x.locate(loc);
  printf("%d %d  %f %f\n",loc.i, loc.j, ll.lat, ll.lon);
  loc.i = 178; loc.j = 368;
  ll = x.locate(loc);
  printf("%d %d  %f %f\n",loc.i, loc.j, ll.lat, ll.lon);
  loc.i = 184; loc.j = 183;
  ll = x.locate(loc);
  printf("%d %d  %f %f\n",loc.i, loc.j, ll.lat, ll.lon);
  loc.i = 185; loc.j =  71;
  ll = x.locate(loc);
  printf("%d %d  %f %f\n",loc.i, loc.j, ll.lat, ll.lon);
  return 0;
}
