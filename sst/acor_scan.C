#include "ncepgrids.h"

void show(int ptno, latpt &ll, global_quarter<float> *fl) ;

int main(int argc, char *argv[]) {
  FILE *fin;
  global_quarter<float> fl[35];
  int i, ptno;
  fijpt loc;
  latpt ll;
  
// read in files:
  fin = fopen("autocorrels.0t34lags","r");
  for (i = 0; i < 35; i++) {
    fl[i].binin(fin);
  }

// now write out selected points
  ptno = 1; ll.lat = -10; ll.lon = 270.;
  show(ptno, ll, fl);
  ptno = 2; ll.lat = -10.0; ll.lon = -10.0;
  show(ptno, ll, fl);

  ptno = 3; ll.lat = 5; ll.lon = 150.;
  show(ptno, ll, fl);

  ptno = 4; ll.lat = 23.5; ll.lon = -60;
  show(ptno, ll, fl);
  ptno = 5; ll.lat = 31.0; ll.lon = -60.;
  show(ptno, ll, fl);
  ptno = 6; ll.lat = 40.0; ll.lon = -60.;
  show(ptno, ll, fl);
  

  return 0;
}
void show(int ptno, latpt &ll, global_quarter<float> *fl) {
  fijpt loc;
  int i;
  loc = fl[0].locate(ll);
  for (i = 1; i < 35; i++) {
    printf("%2d %2d %f\n",ptno, i,fl[i][loc]);
  } 
}
