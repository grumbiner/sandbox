#include "mvector.h"
#include "points.h"
#include "buoy.h"
#include <cstring>
#include <unistd.h>
using namespace std;

// Reading in ensemble of sea ice drift model outputs and create a
//   best average -- minimum distance centroid from each member's estimate
// Robert Grumbine
// 24 March 2014

#define NMEMS    20
#define MAXLINE 900
#define MAXPTS 102400
#define NDAYS    16

#define NSKILE 207

void getfcst(mvector<latpt> &x, mvector<float> &dir, mvector<float> &dist, FILE *fin, int &npts) ;

void average(mvector<latpt> &x, mvector<float> *dir, mvector<float> *dist, mvector<float> &avg_dir, mvector<float> &avg_dist, int npts) ;


#include "subs.C"

#include "centroid.C"

int main(int argc, char *argv[]) {
  mvector<latpt> x[NMEMS];
  mvector<float> dist[NMEMS], dir[NMEMS];
  mvector<float> avg_dist, avg_dir, init_lon, init_lat;
  FILE *fin[NMEMS], *fout, *akout;
  int i, j, npts;

  for (i = 1; i <= NMEMS; i++) {
    fin[i-1] = fopen(argv[i],"r");
    if (fin[i-1] == (FILE *) NULL) {
      printf("failed to open %s\n",argv[i]);
      return 1;
    }
       x[i-1].resize(MAXPTS);
     dir[i-1].resize(MAXPTS);
    dist[i-1].resize(MAXPTS); 
  }
  fout  = fopen(argv[NMEMS+1],"w");
  akout = fopen(argv[NMEMS+2],"w");
  if (fout == (FILE *)NULL || akout == (FILE *) NULL) {
    printf("failed to open an output file %s %s \n",argv[NMEMS+1], argv[NMEMS+2] );
    return 1;
  }

  //printf("have opened all files and resized the vectors as needed\n"); fflush(stdout);

  for (i = 0; i < NDAYS; i++ ) {
    //printf("day %d\n",i+1); fflush(stdout);

    for (j = 0; j < NMEMS; j++) {
      getfcst(x[j], dir[j], dist[j], fin[j], npts);
      //printf("forecast %d day %d npts %d\n",j,i+1,npts); fflush(stdout);
      // Now change out of nautical miles and in to km for further work.
      //   note that arcdis is in km.
      dist[j] *= parameters::nmtokm;
    }
    // could check that all x[j][k] are same
    // also that all npts are the same
    //npts = 1;

    average(x[0], dir, dist, avg_dir, avg_dist, npts);
    avg_dist /= parameters::nmtokm;

    // now output the forecast day average:
    //   -- akout, sk2out, and .tran output
    // Need to create header properly -- this version will not read in correctly
    fprintf(fout, "%3d-Hour Forecast ice drift\n",(i+1)*24);
    fprintf(fout, "Atmosphere-only driving\n");
    fprintf(fout, "Day zero = \n");
    fprintf(fout, "\n");
    fprintf(fout, "Point  Initial location    Dir  Dist(nm)\n");
    for (j = 0; j < NSKILE; j++) {
      fprintf(fout, "%4d%8.1f%6.1f\n",j+1, avg_dir[j], avg_dist[j]);
      //fprintf(fout, "%4d%11.3f%8.3f%8.1f%6.1f\n",j+1, x[0][j].lon, x[0][j].lat, avg_dir[j], avg_dist[j]);
    }
    if (npts > NSKILE) {
      for (j = NSKILE; j < npts; j++) {
        fprintf(fout, "%4d%11.3f%8.3f%8.1f%6.1f\n",j+1, x[0][j].lon, x[0][j].lat, avg_dir[j], avg_dist[j]);
      }
    }
    fprintf(fout, "\n");
    fprintf(fout, "\n");
    
  }
  //pause();
  return 0;
}
void average(mvector<latpt> &x, mvector<float> *dir, mvector<float> *dist, mvector<float> &avg_dir, mvector<float> &avg_dist, int npts) {
  int i, j;
  latpt central;
  mvector<latpt> y(NMEMS);
  float wdir;

   avg_dir.resize(npts);
  avg_dist.resize(npts);

  for (i = 0; i < npts; i++) {
    for (j = 0; j < NMEMS; j++) {
      //printf("dirs %f \n",dir[j][i]); fflush(stdout);
      unbearing(x[i], dist[j][i], dir[j][i], y[j]);
    }
    // now find centroid of the y points
    centroid(y, central);
    //printf("centroid %6.3f %7.3f\n",central.lat, central.lon);
    
    // then find direction and distance from x to the averaged y
    bearing(central, x[i], avg_dist[i], avg_dir[i]);

    // for output purposes, now change direction from bearings convention
    // to meteorological convention
    convert_bw(avg_dir[i], wdir);
    //printf("avg-b %f wdir %f\n",avg_dir[i], wdir); fflush(stdout);
    avg_dir[i] = wdir;
      
  }

  return;
}

void getfcst(mvector<latpt> &x, mvector<float> &dir, mvector<float> &dist, FILE *fin, int &npts) {
  char line[MAXLINE];
  int i, ti;
  float tlon, tlat, tdir, tdist;
  FILE *ptsin;

  ptsin = fopen("seaice_forecast.points","r");
  if (ptsin == (FILE *) NULL) {
    printf("failed to open seaice_forecast.points\n"); 
    exit(1);
  }
// read in 5 dummy lines
  for (i = 0; i < 5; i++) {
    fgets(line, MAXLINE, fin);
    //printf("i = %d strlen = %d line = %s",i, strlen(line), line ); 
    //fflush(stdout);
  }

  for (i = 0; i < NSKILE; i++) {
#ifdef SPLIT
    fscanf(ptsin, "%d %f %f\n",&ti, &tlat, &tlon);
    //printf("skpts %d %f %f\n",ti, tlat, tlon); fflush(stdout);
    x[ti-1].lat = tlat;
    x[ti-1].lon = tlon;

    fgets(line, MAXLINE, fin);
    sscanf(line, "%d %f %f\n",&ti, &tdir, &tdist);
    dir[ti-1]   = tdir;
    dist[ti-1]  = tdist;
#else
    fgets(line, MAXLINE, fin);
    sscanf(line, "%d %f %f %f %f\n",&ti, &tlon, &tlat, &tdir, &tdist);
    //printf("%4d %8.3f %7.3f %5.1f %5.1f\n",ti, tlon, tlat, tdir, tdist);
    x[ti-1].lat = tlat;
    x[ti-1].lon = tlon;
    dir[ti-1]   = tdir;
    dist[ti-1]  = tdist;
#endif
  }
  fclose(ptsin);
  fgets(line, MAXLINE, fin); // read in header


// now read until see blank line
  while (strlen(line) > 5) {
    fgets(line, MAXLINE, fin);
    if (strlen(line) > 5) {
      sscanf(line, "%d %f %f %f %f\n",&ti, &tlon, &tlat, &tdir, &tdist);
      x[ti-1].lat = tlat;
      x[ti-1].lon = tlon;
      dist[ti-1]  = tdist;
      dir[ti-1]   = tdir;
    }
  }
  fgets(line, MAXLINE, fin); // second read for doubled blank lines at end

  npts = ti;

// Convert directions from meteorological to bearings convention
//  float bdir;
//  for (i = 0; i < npts; i++) {
//    convert_wb(x[i], dir[i], bdir);
//    dir[i] = bdir;
//  }

  return;
}
