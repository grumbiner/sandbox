#include <stdio.h>
#include <time.h>

#include "buoy.h"
//#include "ncepgrids.h"

// Program to check through a bouy file and print out those points which are
//   near to any skiles point, near 00 UTC, or are a buoy that _was_ so
//   within some time range (forecast length) of this report.
// Arguments are space range, time range, forecast length, and 
//   output file name.
// Files forecast.points and dboydata are assumed to exist.

// Robert Grumbine 3 April 2000

#define SKILES 207
#define MAXBUOYS 70000
#define FALSE (1==0)
#define TRUE (1==1)

int main(int argc, char *argv[]) {
  FILE *fin1, *fin2, *fout;
  latpt loc[207];
  //global_ice<unsigned char> landmask;
  buoy_report buoy, near_buoys[MAXBUOYS];
  int i, j, k, nnear, skileno, wasnear, oldest;
  float lat, lon;
  float space, time;
  int fcst_length;

  time_t deltat, delta_fcst, temporary_time_t;
  tm start_time, end_time, cur_time, *tmp_tm ;

  bool reset_oldest;


/////////////////////////////////////////////////////////
// Set up arguments/control values.
  space = atof(argv[1]);
  time  = atof(argv[2]);
  deltat = (time_t) (3600 * time);
  fcst_length = atoi(argv[3]);
  delta_fcst = 24 * 3600 * fcst_length;
  printf("Space limit = %f\n",space);
  printf("deltat = %d\n",(int) deltat);
  printf("delta_fcst = %d\n",(int) delta_fcst);
  printf("Size of a buoy report %d\n",sizeof(buoy_report) );
  fflush(stdout);
  fout = fopen(argv[4],"w+");
  if (fout == (FILE*) NULL) {
    printf("Failed to open %s for reading and writing \n",argv[4]);
    return 1;
  }

/////////////////////////////////////////////////////////
//Read in all skiles points
  fin1=fopen("forecast.points","r");
  if (fin1 == (FILE *) NULL) {
    printf("Failed to open the forecast.points file, exiting\n");
    return 1;
  }  
  i = 0;
  while (!feof(fin1) ) {
    fscanf(fin1, "%d %f %f\n",&skileno, &lat, &lon);
    i += 1;
    #ifdef VERBOSE
      printf("%d %f %f\n",skileno, lat, lon);
    #endif
    loc[skileno-1].lat = lat;
    loc[skileno-1].lon = lon;
  }
  fclose(fin1);
  printf("i at end of while loop skiles pt. read %d\n",i);
  fflush(stdout);

/////////////////////////////////////////////////////////
// Now read through buoy file and see what matchups we find
  fin2 = fopen("dboydata","r");
  rewind(fin2);
  nnear = 0;
  oldest = 0; // Furthest back buoy entry to check for matchup

  while (!feof(fin2)) {
    buoy.read(fin2);
    // If we're not near 00 UTC or we're not a drifting buoy, skip
    if (!buoy.synoptic(time) || !buoy.isdrifter() ) continue;

//If we're near to a skiles point, or if we're within 16 days of a report which
//  is, write out the data report
//Also require reports to be near synoptic times (00Z +- 'time' hours)
//Skpt test
    wasnear = FALSE;
    for (skileno = 0; (skileno < SKILES) && !wasnear; skileno++) {
      if (buoy.near(loc[skileno], space) ) {
        //printf("adding buoy to near list %d\n",nnear); fflush(stdout);
        wasnear = TRUE;
        near_buoys[nnear] = buoy;
        nnear += 1;
        if ((float) nnear > 0.75 * MAXBUOYS) {
          printf("overrunning the near buoy list, nnear = %d\n",nnear);
          if (nnear >= MAXBUOYS - 4) {
            printf("Too many buoys!!\n At buoy %d with %d max\n",nnear, 
                 MAXBUOYS);
            return 1;
          }
        }
        #ifdef VERBOSE
          printf("near_buoy id = %s\n",near_buoys[nnear].station_id);
          printf("%3d %5.2f %6.2f %s %2d %2d %2d %5.2f\n",skileno, 
                    buoy.latitude, buoy.longitude, buoy.station_id, 
                    buoy.year, buoy.month, buoy.day, buoy.hour);
          printf("nnear = %d\n",nnear);
        #endif
        fprintf(fout,"%3d %5.2f %6.2f %s %2d %2d %2d %5.2f\n",skileno, 
                  buoy.latitude, buoy.longitude, buoy.station_id, 
                  buoy.year, buoy.month, buoy.day, buoy.hour);
        fflush(fout);
      }
    }

   if (!wasnear) {
     wasnear = FALSE; skileno = 0;
     reset_oldest = false;
     for (j = oldest; (j < nnear && wasnear == FALSE) ; j++) {
       if (buoy.near(near_buoys[j], buoy.station_id, delta_fcst) && 
                 buoy.synoptic(time)  && !buoy.isship() ) {
         #ifdef VERBOSE
           printf("near in fcst time %s %2d %5.2f  %2d %5.2f \n",
              near_buoys[j].station_id, buoy.day, buoy.hour, 
              near_buoys[j].day, near_buoys[j].hour);
           printf("%3d %5.2f %6.2f %s %2d %2d %2d %5.2f\n",skileno, 
              buoy.latitude, buoy.longitude, buoy.station_id, 
              buoy.year, buoy.month, buoy.day, buoy.hour);
         #endif
         wasnear = TRUE;
         fprintf(fout, "%3d %5.2f %6.2f %s %2d %2d %2d %5.2f\n",skileno, 
               buoy.latitude, buoy.longitude, buoy.station_id, 
               buoy.year, buoy.month, buoy.day, buoy.hour);
       }
       // Add this to update the eldest so as to avoid wasting huge
       //   amounts of time checking against ancient observations
       else {
         deltat = buoy.obs_secs - near_buoys[oldest].obs_secs;
         if (deltat > 4.*delta_fcst) {
           while (deltat > 2*delta_fcst) {
             reset_oldest = true;
             oldest += 1;
             deltat = buoy.obs_secs - near_buoys[oldest].obs_secs;
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
