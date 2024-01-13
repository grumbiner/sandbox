#include <stdio.h>
#include <time.h>

#include "buoy.h"
#define SKILES 207
#define MAXBUOYS 20000
#undef VERBOSE
#define FALSE (1==0)
#define TRUE (1==1)

extern "C" void getfcst_(int *date, int *funit2, float *dir, float *dist, 
                           int *code);

//Program to read in data and forecasts for verification as needed
int main(int argc, char *argv[]) {
  FILE *fin1, *buoyfile, *fcstfile;
  buoy_report buoy, near_buoys[MAXBUOYS];
  latpt loc[SKILES];
  int i, j, nnear, skileno, wasnear, date, funit=10, retcode;
  float lat, lon;
  float space, time;
  int fcst_length;
  tm start_time, end_time, cur_time, *tmp_tm;
  time_t deltat, delta_fcst, tmp;
  char fname[90];
  float dir[16][SKILES], dist[16][SKILES];

/////////////////////////////////////////////////////////
// Set up arguments/control values.
  space = atof(argv[1]);
  time  = atof(argv[2]);
  deltat = 3600 * time;
  fcst_length = atoi(argv[3]);
  delta_fcst = 24 * 3600 * fcst_length;
  printf("Space limit = %f\n",space);
  printf("deltat = %d\n",deltat);
  printf("delta_fcst = %d\n",delta_fcst);
  printf("Size of a buoy report %d\n",sizeof(buoy) );
//Note that tm_mon is 0-11, not 1-12
  start_time.tm_sec = 0; start_time.tm_min = 0; start_time.tm_hour = 0;
  start_time.tm_mday = 10; start_time.tm_mon = 5; start_time.tm_year = 98;
  end_time.tm_sec = 0; end_time.tm_min = 0; end_time.tm_hour = 0;
  end_time.tm_mday = 30; end_time.tm_mon = 10; end_time.tm_year = 98;
  mktime(&start_time);
  mktime(&end_time);
  cur_time = start_time;
  printf("cur time %d %d %d\n",cur_time.tm_year, cur_time.tm_mon+1, 
                       cur_time.tm_mday);

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
//Open the buoy file
  buoyfile = fopen("buoycheck.out","r");
  rewind(buoyfile);
/////////////////////////////////////////////////////////
  while (mktime(&cur_time) < mktime(&end_time) ) {
    date = cur_time.tm_mday + (cur_time.tm_mon+1)*100 
                        + cur_time.tm_year*100*100;
    getfcst_(&date, &funit, &dir[0][0], &dist[0][0], &retcode);
    if (retcode != 0) {
      printf("error in getfcst %d, date %d\n",retcode, date);
      continue;
    }
      //CDfor (i = 0; i < 16; i++) {
      //CD   skileno = 65;
      //CD   printf("%6d %3d %3d  %5.1f %5.1f\n",date, i, skileno, 
      //CD           dir[i][skileno], dist[i][skileno]       );
      //CD} 
//  Have a valid forecast, now try to get a buoy to match against it
  rewind(buoyfile);



// Advance the date.  Boy is this deserving of being put in to a class 
//   or subroutine!
    tmp =  mktime(&cur_time) + (time_t) (24 * 3600);
    tmp_tm = localtime(&tmp);
    cur_time.tm_year = tmp_tm->tm_year;
    cur_time.tm_mon = tmp_tm->tm_mon;
    cur_time.tm_mday = tmp_tm->tm_mday;
    cur_time.tm_hour = tmp_tm->tm_hour;
    mktime(&cur_time);
    if (cur_time.tm_hour != 0) {
      if (cur_time.tm_hour == 23) {
        tmp =  mktime(&cur_time) + (time_t) (3600);
        tmp_tm = localtime(&tmp);
        cur_time.tm_year = tmp_tm->tm_year;
        cur_time.tm_mon = tmp_tm->tm_mon;
        cur_time.tm_mday = tmp_tm->tm_mday;
        cur_time.tm_hour = tmp_tm->tm_hour;
        mktime(&cur_time);
      }
      if (cur_time.tm_hour == 1) {
        tmp =  mktime(&cur_time) - (time_t) (3600);
        tmp_tm = localtime(&tmp);
        cur_time.tm_year = tmp_tm->tm_year;
        cur_time.tm_mon = tmp_tm->tm_mon;
        cur_time.tm_mday = tmp_tm->tm_mday;
        cur_time.tm_hour = tmp_tm->tm_hour;
        mktime(&cur_time);
      }
    } 
    printf("cur time %d %d %d %d\n",cur_time.tm_year, cur_time.tm_mon+1, 
                       cur_time.tm_mday, cur_time.tm_hour);
  }
  return 0;

/////////////////////////////////////////////////////////
// Now read through buoy file and see what matchups we find
  buoyfile = fopen("buoycheck.out","r");
  rewind(buoyfile);
  nnear = 0;
  while (!feof(buoyfile)) {
    buoy.read(buoyfile);
//If we're near to a skiles point, or if we're within 16 days of a report which
//  is, write out the data report
//Skpt test
    wasnear = FALSE;
    for (skileno = 0; skileno < SKILES; skileno++) {
      if (buoy.near(loc[skileno], space)) {
        wasnear = TRUE;
        near_buoys[nnear] = buoy;
        #ifdef VERBOSE
          printf("near_buoy id = %s\n",near_buoys[nnear].station_id);
        #endif
        printf("%3d %5.2f %6.2f %s %2d %2d %2d %5.2f\n",skileno, 
                  buoy.latitude, buoy.longitude, buoy.station_id, 
                  buoy.year, buoy.month, buoy.day, buoy.hour);
        nnear += 1;
        if ((float) nnear > 0.9 * MAXBUOYS) {
          printf("overrunning the near buoy list, nnear = %d\n",nnear);
        }
        #ifdef VERBOSE
          printf("nnear = %d\n",nnear);
        #endif
      }
    }
//Forecast time test
   if (!wasnear) {
     wasnear = FALSE; skileno = 0;
     for (j = 0; (j < nnear && wasnear == FALSE) ; j++) {
       if (buoy.near(near_buoys[j], buoy.station_id, delta_fcst)) {
         #ifdef VERBOSE
         printf("near in fcst time %s %2d %5.2f  %2d %5.2f \n",
            near_buoys[j].station_id, buoy.day, buoy.hour, 
            near_buoys[j].day, near_buoys[j].hour);
         #endif
         wasnear = TRUE;
         printf("%3d %5.2f %6.2f %s %2d %2d %2d %5.2f\n",skileno, 
               buoy.latitude, buoy.longitude, buoy.station_id, 
               buoy.year, buoy.month, buoy.day, buoy.hour);
       }
     } //end looping over near buoy obs
   } //end if

  } //end while
  fclose(buoyfile);


  return 0;
}
