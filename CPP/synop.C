#include <stdio.h>
#include <time.h>

#include "buoy.h"
#define SKILES 207
#define MAXBUOYS 27000
#undef VERBOSE
#define FALSE (1==0)
#define TRUE (1==1)

int main(int argc, char *argv[]) {
  FILE *fin1, *fin2, *fout;
  latpt loc[SKILES];
  int i, j, nnear, skileno, wasnear;
  float lat, lon;
  float space, time;
  int fcst_length;

  time_t deltat, delta_fcst, temporary_time_t;
  tm start_time, end_time, cur_time, *tmp_tm ;


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
  fin2 = fopen(argv[4],"r+");
  if (fin2 == (FILE*) NULL) {
    printf("Failed to open %s for reading and writing \n",argv[4]);
    return -1;
  }

/////////////////////////////////////////////////////////
//Read in all skiles points
  fin1=fopen("forecast.points","r");
  for (i = 0; i < SKILES; i++) {
    fscanf(fin1, "%d %f %f\n",&skileno, &lat, &lon);
    #ifdef VERBOSE
      printf("%d %f %f\n",skileno, lat, lon);
    #endif
    loc[i].lat = lat;
    loc[i].lon = lon;
  }

/////////////////////////////////////////////////////////
// All buoy reports which are near a skiles point, or which are from same
//   buoy as one which was and is within the forecast time interval are
//   now written to an output file.  
// Now is the time to go back through and consolidate all those which
//   are within the synoptic time reporting windows.
//Note that tm_mon is 0-11, not 1-12
  start_time.tm_sec = 0; start_time.tm_min = 0; start_time.tm_hour = 0;
  start_time.tm_mday = 10; start_time.tm_mon = 5; start_time.tm_year = 98;
  end_time.tm_sec = 0; end_time.tm_min = 0; end_time.tm_hour = 0;
  end_time.tm_mday = 30; end_time.tm_mon = 10; end_time.tm_year = 98;
//  end_time.tm_mday = 30; end_time.tm_mon = 7; end_time.tm_year = 98;
  mktime(&start_time);
  mktime(&end_time);

//Loop one report at a time, but all times
  rewind(fin2);
  while (!feof(fin2) ) {
    float tlat, tlon, thour;
    int tskile, tid, tyear, tmon, tday;
    buoy_report tbuoy, curbuoytime, synbuoy[MAXBUOYS];
    fscanf(fin2,"%d %f %f %d %d %d %d %f\n",&tskile,
        &tlat, &tlon, &tid, &tyear, &tmon, &tday, &thour); 
    #ifdef VERBOSE
    printf("%3d %5.2f %6.2f %6d %2d %2d %2d %6.3f\n",tskile,
        tlat, tlon, tid, tyear, tmon, tday, thour); 
    #endif
    tbuoy.latitude = tlat;
    tbuoy.longitude = tlon;
    tbuoy.year = tyear;
    tbuoy.month = tmon ; // the -1 for observation time is managed in set_time
    tbuoy.day   = tday;
    tbuoy.hour  = thour;
    sprintf(tbuoy.station_id, "%6d", tid);
    tbuoy.set_time();

//  Reset the current time for each pass
    cur_time = start_time;
    mktime(&cur_time);
    #ifdef VERBOSE
      printf("cur time %d %d %d\n",cur_time.tm_year, cur_time.tm_mon+1,
                       cur_time.tm_mday);
    #endif
    wasnear = FALSE;
    while (mktime(&cur_time) < mktime(&end_time) && wasnear == FALSE) {
       if (tbuoy.near(cur_time, deltat) ) {
         printf("%3d  %2d %2d %2d  %2d %2d %5.2f %5.2f %6.2f %s\n",
               tskile, cur_time.tm_mon+1, cur_time.tm_mday, cur_time.tm_hour, 
               tbuoy.month, tbuoy.day, tbuoy.hour, tbuoy.latitude, 
               tbuoy.longitude, tbuoy.station_id);
         wasnear = TRUE;
        }
        else {
         #ifdef VERBOSE
         printf("not synoptic %d %d %d  %d %d %f  %d\n",cur_time.tm_mon+1, 
                    cur_time.tm_mday, cur_time.tm_hour, tbuoy.month, 
                    tbuoy.day, tbuoy.hour, (int) mktime(&cur_time) - 
                        mktime(&tbuoy.obs_time) );
         #endif
        }
//
// Advance the date.  Boy is this deserving of being put in to a class
//   or subroutine!
      temporary_time_t =  (time_t) mktime(&cur_time) + (time_t) (24 * 3600);
      tmp_tm = localtime(&temporary_time_t);
      cur_time.tm_year = tmp_tm->tm_year;
      cur_time.tm_mon = tmp_tm->tm_mon;
      cur_time.tm_mday = tmp_tm->tm_mday;
      cur_time.tm_hour = tmp_tm->tm_hour;
      mktime(&cur_time);
      if (cur_time.tm_hour != 0) {
        if (cur_time.tm_hour == 23) {
          temporary_time_t =  mktime(&cur_time) + (time_t) (3600);
          tmp_tm = localtime(&temporary_time_t);
          cur_time.tm_year = tmp_tm->tm_year;
          cur_time.tm_mon = tmp_tm->tm_mon;
          cur_time.tm_mday = tmp_tm->tm_mday;
          cur_time.tm_hour = tmp_tm->tm_hour;
          mktime(&cur_time);
        }
        if (cur_time.tm_hour == 1) {
          temporary_time_t =  mktime(&cur_time) - (time_t) (3600);
          tmp_tm = localtime(&temporary_time_t);
          cur_time.tm_year = tmp_tm->tm_year;
          cur_time.tm_mon = tmp_tm->tm_mon;
          cur_time.tm_mday = tmp_tm->tm_mday;
          cur_time.tm_hour = tmp_tm->tm_hour;
          mktime(&cur_time);
        }
      }
      #ifdef VERBOSE
      printf("cur time %d %d %d %d\n",cur_time.tm_year, cur_time.tm_mon+1,
                       cur_time.tm_mday, cur_time.tm_hour);
      #endif

    } //end of synoptic time while loop
  } // ran to eof on input file 


  return 0;
}
