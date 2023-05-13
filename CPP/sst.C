#include "ncepgrids.h"

#define GFS_LAND 1

int main(int argc, char *argv[]) {
  gaussian<float> nsst, land, icec;
  global_12th<float> rtg, delta;
  FILE *nin, *rin;
  float undef = -999.;

  rin = fopen(argv[1], "r");
  rtg.binin(rin);
  fclose(rin);
  delta.set(undef);
  //printf("rtg stats %f %f %f %f\n",rtg.gridmax(), rtg.gridmin(), rtg.average(), rtg.rms() );


  //printf("gaussian grids: %d by %d\n",nsst.xpoints(), nsst.ypoints() );
  nin = fopen(argv[2], "r");
  nsst.binin(nin);
  land.binin(nin);
  icec.binin(nin);
  fclose(nin);
  //printf("nsst stats %f %f %f %f\n",nsst.gridmax(), nsst.gridmin(), nsst.average(), nsst.rms() );
  //printf("land stats %f %f %f %f\n",land.gridmax(), land.gridmin(), land.average(), land.rms() );
  //printf("icec stats %f %f %f %f\n",icec.gridmax(), icec.gridmin(), icec.average(), icec.rms() );


  ijpt loc;
  latpt ll;
  fijpt floc;

  for (loc.j = 0; loc.j < rtg.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < rtg.xpoints(); loc.i++) {
    ll   = rtg.locate(loc);
    floc = nsst.locate(ll);
    if (!nsst.in(floc)) {
      printf("ran off nsst grid at rtg location %f %f\n",ll.lat, ll.lon);
    }
    else {
      if (land[floc] != GFS_LAND && icec[floc] == 0.) {
        delta[loc] = (rtg[loc] - nsst[floc]);
        //printf("%8.4f %8.4f  %.2f %.2f %.2f  %.2f %.2f\n",ll.lat, ll.lon, rtg[loc], nsst[floc], (rtg[loc] - nsst[floc]), land[floc], icec[floc]);
        printf("%8.4f %8.4f  %.2f %.2f %6.2f\n",ll.lat, ll.lon, rtg[loc], nsst[floc], delta[loc] );
      }
    }
  }
  }
  
  printf("delta stats %f %f %f %f\n",delta.gridmax(undef), delta.gridmin(undef), delta.average(undef), delta.rms(undef) );
  
  return 0;
}
