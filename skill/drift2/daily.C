#include <cstdio>
#include <time.h>

#include "buoy.h"

// Program to read through an IABP buoy file and compute day to day (00Z)
//   ice motions for all points.
// Note that year is 2 digits through end of 1998 and 4 thereafter.

// Program to check through an IABP buoy file and print out those points 
//   which are near to any skiles point, near 00 UTC, or are a buoy that 
//   _was_ so within some time range (forecast length) of this report.  
// Arguments are space range, time range, forecast length, and 
//   output file name.
// Files forecast.points and dboydata are assumed to exist.

// Robert Grumbine 3 April 2000
// Fixes to span gaps between valid observations which are longer than
//   the forecast interval input.
// Robert Grumbine 11 January 2001


#define MAXBUOYS 70000
#define FALSE (1==0)
#define TRUE (1==1)

int main(int argc, char *argv[]) {
  FILE *fin1, *fin2, *fout;
  buoy_report buoy, near_buoys[MAXBUOYS];
  int i, j, k, nnear, wasnear, oldest;
  float time;
  int fcst_length;

  time_t deltat, delta_fcst, temporary_time_t;
  //tm start_time, end_time, cur_time, *tmp_tm ;

  bool reset_oldest;

/////////////////////////////////////////////////////////
// Set up arguments/control values.
  delta_fcst = 24 * 3600 ;

/////////////////////////////////////////////////////////
// Now read through buoy file and see what matchups we find
  fin2 = fopen(argv[1], "r");
  if (fin2 == (FILE *) NULL) {
    printf("Failed to find the required drift input file \n");
    return 1;
  }
  rewind(fin2);
  fout = fopen(argv[2], "w");
  if (fout == (FILE*) NULL) {
    printf("failed to open an output file\n");
    return 2;
  }

  nnear = 0;
  oldest = 0; // Furthest back buoy entry to check for matchup

  while (!feof(fin2)) {
    printf("reading in iabp\n"); fflush(stdout);
    buoy.iabpread(fin2);
    // If we're not near 00 UTC or we're not a drifting buoy, skip
    //if (!buoy.synoptic(time) || !buoy.isdrifter() ) continue;
    fprintf(fout,"%5.2f %6.2f %s %2d %2d %2d %5.2f\n",
              buoy.latitude, buoy.longitude, buoy.station_id, 
              buoy.year, buoy.month, buoy.day, buoy.hour);
    fflush(fout);
    continue;
   if (!wasnear) {
     wasnear = FALSE; 
     reset_oldest = false;
     for (j = oldest; (j < nnear && wasnear == FALSE) ; j++) {
       if (buoy.near(near_buoys[j], buoy.station_id, delta_fcst) && 
                 buoy.synoptic(time)  && !buoy.isship() ) {
         wasnear = TRUE;
         fprintf(fout, "%5.2f %6.2f %s %2d %2d %2d %5.2f\n",
               buoy.latitude, buoy.longitude, buoy.station_id, 
               buoy.year, buoy.month, buoy.day, buoy.hour);
       }
       // Add this to update the eldest so as to avoid wasting huge
       //   amounts of time checking against ancient observations
       else {
         deltat = buoy.obs_secs - near_buoys[oldest].obs_secs;
         if (deltat > 4.*delta_fcst) {
           while (deltat > 2*delta_fcst && oldest < nnear) {
             reset_oldest = true;
             oldest += 1;
             deltat = buoy.obs_secs - near_buoys[oldest].obs_secs;
             //printf("oldest %d, deltat %d\n",oldest, (int) deltat);
             //fflush(stdout);
           }
         }
       }
     } //end for looping over near buoy obs
     // If we have reset the oldest, then shuffle data points down,
     //   helps reduce max memory usage.
     if (reset_oldest) {
       for (k = oldest; k < nnear; k++) {
          near_buoys[k-oldest] = near_buoys[k];
       }
       printf("reset oldest, nnear = %d %d\n",oldest, nnear); fflush(stdout);
       nnear -= oldest;
       oldest = 0;
       reset_oldest = false;
     }

   } //end if

  } //end while
  printf("Total near buoys1: %d\n",nnear);
  fclose(fin2);

  return 0;
}
