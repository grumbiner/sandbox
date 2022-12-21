// Get bounding curve information from ascii-formatted files.
// Otherwise, much the same as getseg for binaries
// Robert Grumbine 
// 11 Mar 2005
//
#include "mvector.h"
#include "points.h"
#include "corners.C"

int getseas(mvector<latpt> &locations, float &west, float &east,
              float &north, float &south, char *name, FILE *fin) {
// Varying from getseg, this is to get ascii-crafted segments
  int i, n, npts; 
  latpt llat;

  fgets(name, 900, fin);
  // needed to remove the carriage return, and pad for columnar output
  n = strlen(&name[0]);
  for (i = n-1; i < 21; i++) {
    name[i] = ' ';
  }
  name[21] = '\0';
  //printf("%s\n",name);
  fscanf(fin,"%d %f %f\n",&npts, &llat.lat, &llat.lon);
  //printf("%d %f %f\n",npts, llat.lat, llat.lon);
  locations.resize(npts);
  
  for (i = 0; i < npts; i++) {
    fscanf(fin,"%f %f\n",&llat.lat, &llat.lon);
    //printf("%f %f\n",llat.lat, llat.lon);
    if (llat.lon < 0) llat.lon += 360.;
    locations[i] = llat;
  }
  fflush(stdout);

  if (locations[0].lat != locations[npts-1].lat || 
      locations[0].lon != locations[npts-1].lon    ) {
    printf("error, last point must be same as first\n");
    fflush(stdout);
    return -1;
  }
  corners(locations, north, south, east, west);

  return npts;
}
