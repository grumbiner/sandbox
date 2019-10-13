#include <stdio.h>
#include <time.h>

#include "buoy.h"
#define MAXBUOYS 500000

// Program to read through an IABP 12-hourly file and compute 
//   distance and velocity displacements for those obs pairs 
//   which are 12 hours apart.
// arg1 = iabp file ame

int main(int argc, char *argv[]) {
  FILE *fin2;
  buoy_report buoy, buoys[MAXBUOYS];
  bool checked[MAXBUOYS];
  int i, j, nbuoy, oldest;
  float dist, dx, dy, theta;

/////////////////////////////////////////////////////////
  fin2 = fopen(argv[1],"r");
  if (fin2 == (FILE *) NULL) {
    printf("Failed to find the required input file %s\n",argv[1]);
    return 1;
  }
  rewind(fin2);

  nbuoy = 0;
  i = 0;

// Scan in all buoys:
  while (!feof(fin2)) {
    buoys[i].iabpread(fin2);
    checked[i] = false;
    i += 1;
  } //end while
  nbuoy = i - 1;
  printf("Total buoys: %d\n",nbuoy); fflush(stdout);
  fclose(fin2);

  j = 0;
  oldest = 0;
  for (j = 0; j < nbuoy - 1; j++) {
      if (!buoys[j].ok() ) continue;
    for (i = j+1; i < nbuoy; i++) {
      if (!buoys[i].ok() ) continue;
      if (buoys[i].near(buoys[j], (time_t) 12*3600 + 1) 
           && buoys[i].same_id(buoys[j])) {
        dist = arcdis_(buoys[j].longitude, buoys[j].latitude, 
                       buoys[i].longitude, buoys[i].latitude);
        dx  = arcdis_(buoys[j].longitude, buoys[j].latitude, 
                       buoys[i].longitude, buoys[j].latitude);
        dy  = arcdis_(buoys[j].longitude, buoys[j].latitude, 
                       buoys[j].longitude, buoys[i].latitude);
// Need to make dx, dy signed, for wdir:
        if (buoys[j].latitude > buoys[i].latitude) dy = -dy;
        if (buoys[j].longitude > buoys[i].longitude) dx = -dx;

        printf("%3d %2d %2d %2d   %6.3f %7.3f %6s  %3d %2d %2d %2d  %6.3f %7.3f  %f %f %f\n",
                buoys[j].year, buoys[j].month, buoys[j].day, (int) buoys[j].hour, 
                buoys[j].latitude, buoys[j].longitude, buoys[j].station_id, 
                buoys[i].year, buoys[i].month, buoys[i].day, (int) buoys[i].hour, 
                buoys[i].latitude, buoys[i].longitude, 
                dist, dx, dy);
        fflush(stdout);
        checked[j] = true;
      }

// If there's a gap, skip out of the i loop
      if (buoys[i].get_secs() - buoys[j].get_secs() > 36*3600) {
        break;
      }
    }
  }


  return 0;
}
