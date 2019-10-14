#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin, *fin2, *fout;
  GRIDTYPE<unsigned char> conc, mask, tmp;
  GRIDTYPE<int> count;
  GRIDTYPE<float> pct;
  float minlat = 23.91;
  float toplat = 67.0;
  float mincut = 0.01;
  float slope  = 0.5 / 40.0; // top is %, bottom is number of degrees to phase
  int i, days = 0;
  palette<unsigned char> gg(19,65), hh(5);
  ijpt loc;
  latpt ll;

  fin2 = fopen(argv[2],"r");
  if (fin2 == (FILE*) NULL) {
    printf("failed to open %s\n",argv[2]);
    return 1;
  }
  mask.binin(fin2);
  fclose(fin2);
  printf("mask max, min, avg %d %d %d\n",mask.gridmax(), mask.gridmin(), 
                  mask.average() );


  fin = fopen(argv[1],"r");
  if (fin == (FILE*) NULL) {
    printf("failed to open %s\n",argv[1]);
    return 1;
  }
  count.set(0);
  while (!feof(fin)) {
    conc.binin(fin);
    days += 1;
    for (i = 0; i < conc.xpoints() *conc.ypoints(); i++) {
      if (conc[i] >= MIN_CONC && conc[i] <= MAX_CONC) {
        count[i] += 1;
      }
    }
  } 
  fclose(fin);

  printf("found %d days information, nx, ny = %d %d\n",days, 
                   conc.xpoints(), conc.ypoints() );
  fflush(stdout);
  fout = fopen(argv[3],"w");
  count.binout(fout);
  fclose(fout);

// 
  loc.i = 0; loc.j = 0;
  ll = pct.locate(loc);
  if (ll.lat < 0.) {
    minlat = -32.0;
    slope *= -1;
    toplat = 60.5;
  }

  tmp = mask;
  for (loc.j = 0; loc.j < count.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < count.xpoints(); loc.i++) {
     pct[loc] = (float) count[loc] / (float) days;
     ll = pct.locate(loc);
//  If the coverage fraction is too high for the latitude, and it is 
//     currently water
     if (fabs(ll.lat) < toplat && pct[loc] > slope*(ll.lat - minlat) + mincut && 
                 tmp[loc] == 0 ) {
       printf("masking %3d %3d  %5.2f %7.2f at %5.3f percent\n",
                  loc.i, loc.j, ll.lat, ll.lon, pct[loc]); 
       tmp[loc] = 1;
       mask[loc] = 195;
     }
// Coast removal
     if (fabs(ll.lat) < toplat && pct[loc] < slope*(ll.lat - minlat)/4. + mincut && 
                 tmp[loc] == 195 ) {
       printf("decoasting %3d %3d  %5.2f %7.2f at %5.3f percent\n",
                  loc.i, loc.j, ll.lat, ll.lon, pct[loc]);  
       tmp[loc] = 2;
       mask[loc] = 0;
     }
// Land removal:
     if ((fabs(ll.lat) < toplat) && (pct[loc] < slope*(ll.lat - minlat)/4. + mincut)
          && (tmp[loc] == 157) && tmp.anyof(0, 1, loc) >= 1 ) {
       printf("delanding %3d %3d  %5.2f %7.2f at %5.3f percent\n",
                  loc.i, loc.j, ll.lat, ll.lon, pct[loc]);  
       tmp[loc] = 3;
       mask[loc] = 0;
     }
     if (tmp[loc] == 100) {
       tmp[loc] = 4;
       mask[loc] = 157;
     }
  }
  }
// Do this separately, so as to avoid bogus delanding
  for (loc.j = 0; loc.j < count.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < count.xpoints(); loc.i++) {
     if (tmp[loc] == 157) tmp[loc] = 0;
     if (tmp[loc] == 195) tmp[loc] = 0;
     if (tmp[loc] > 3) {
       printf("tmp out of range %d at %d %d\n",tmp[loc],loc.i, loc.j);
     }
  }
  }


  count.scale();
  count.xpm("count.xpm", 7 , gg); 
  
  hh.set_color(0, 255, 255, 255);
  hh.set_color(1,   0,   0, 255);
  hh.set_color(2,   0, 255,   0);
  hh.set_color(3, 255,   0,   0);
  hh.set_color(4,   0,   0,   0);
  tmp.xpm("new.xpm",1,hh);

  fout = fopen(argv[4],"w");
  mask.binout(fout);
  fclose(fout);

 
  return 0;
}
