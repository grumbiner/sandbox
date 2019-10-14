#include <stdio.h>
#include <time.h>

#include "buoy.h"
#define SKILES 207
#define MAXBUOYS 27000
#undef VERBOSE
#define FALSE (1==0)
#define TRUE (1==1)
#define nmtokm 1.852

extern "C" float wdir_(float &dx, float &dy, float &fdummy);
extern "C" void getfcst_(int *date, int *funit, float *dir, float *dist, 
                           int *code);

int main(int argc, char *argv[]) {
  FILE *fin1, *fin2, *fout, *fcstout;
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
  //printf("Space limit = %f\n",space);
  //printf("deltat = %d\n",(int) deltat);
  //printf("delta_fcst = %d\n",(int) delta_fcst);
  //printf("Size of a buoy report %d\n",sizeof(buoy_report) );
  fflush(stdout);
  fin2 = fopen(argv[4],"r+");
  if (fin2 == (FILE*) NULL) {
    printf("Failed to open %s for reading and writing \n",argv[4]);
    return -1;
  }
  fout = fopen(argv[5],"w+");
  if (fout == (FILE*) NULL) {
    printf("Failed to open %s for reading and writing \n",argv[5]);
    return -1;
  }
  fcstout = fopen(argv[6],"w+");
  if (fcstout == (FILE*) NULL) {
    printf("Failed to open %s for reading and writing \n",argv[6]);
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
  end_time.tm_sec = 0; end_time.tm_min = 0; end_time.tm_hour = 0;

  start_time.tm_mday = 1; start_time.tm_mon = 10; start_time.tm_year = 94;
  end_time.tm_mday = 31;  end_time.tm_mon = 11;   end_time.tm_year = 98;
  mktime(&start_time);
  mktime(&end_time);

//Loop one report at a time, but all times
  printf("Reading in buoy reports\n");
  rewind(fin2);
  tyear = 90;
  while ( !feof(fin2) ) {
    float tlat, tlon, dum3, thour;
    int tskile, tid, tyear, tmon, tday, dum1, dum2;
    buoy_report tbuoy;
//    tyear = 98;
//    fscanf(fin2,"%d %d %d %d %d %d %d %f\n",&tskile,
//        &tmon, &tday, &thour, &dum1, &dum2, &dum3, &tlat,&tlon, &tid); 
    fscanf(fin2,"%d %f %f %d %d %d %d %f\n",&tskile,
        &tlat, &tlon, &tid, &tyear, &tmon, &tday, &thour); 
    #ifdef VERBOSE
      printf("new %3d %5.2f %6.2f %6d %2d %2d %2d %6.3f\n",tskile,
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
         fprintf(fout, "%3d  %2d %2d %2d  %2d %2d %5.2f %5.2f %6.2f %s\n",
               tskile, cur_time.tm_mon+1, cur_time.tm_mday, cur_time.tm_hour, 
               tbuoy.month, tbuoy.day, tbuoy.hour, tbuoy.latitude, 
               tbuoy.longitude, tbuoy.station_id);
         #ifdef VERBOSE
         printf("%3d  %2d %2d %2d  %2d %2d %5.2f %5.2f %6.2f %s\n",
               tskile, cur_time.tm_mon+1, cur_time.tm_mday, cur_time.tm_hour, 
               tbuoy.month, tbuoy.day, tbuoy.hour, tbuoy.latitude, 
               tbuoy.longitude, tbuoy.station_id);
         #endif
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

//////////////////////////////////////////////////////
//////////////////////////////////////////////////////
// Average locations per synoptic date
 {
  buoy_report tbuoy, buoy[MAXBUOYS];
  int skpts[MAXBUOYS];
  float tlat, tlon;
  int tskile, tid, tyear, tmon, tday, thour;
  int dum1, dum2;
  float dum3;
  int curno, ntrimmed, count, used[MAXBUOYS];

  printf("Averaging down to synoptic times\n"); fflush(stdout);
  rewind(fout);
  for(i = 0; i < MAXBUOYS; i++) { used[i] = FALSE; }

  nnear = 0;
  i = 0;
  tyear = 98;
  while (!feof(fout)) {
    fscanf(fout, "%d %d %d %d  %d %d %f %f %f %d",
      &skpts[i], &tmon, &tday, &thour, &dum1, &dum2, &dum3, &tlat, &tlon, &tid);
    buoy[i].latitude = tlat;
    buoy[i].longitude = tlon;
    buoy[i].year = tyear;
    buoy[i].month = tmon ; // the -1 for observation time is managed in set_time
    buoy[i].day   = tday;
    buoy[i].hour  = thour;
    sprintf(buoy[i].station_id, "%6d", tid);
    buoy[i].set_time();
    #ifdef VERBOSE
      printf("i = %5d, buoy = %s, %6.2f %6.2f\n",i, buoy[i].station_id, 
          buoy[i].latitude, buoy[i].longitude); fflush(stdout);
    #endif
    nnear += 1;
    i += 1;
  }
  printf("Found %d buoy reports on synoptic times\n",nnear);

  ntrimmed = 0;
  curno = 0;
  while (curno < nnear) {
    //find a buoy which is on a skiles point 'used' declares whether we've
    //  used a point before.
    for(i = curno; ( (i < nnear) && used[i] ) ; i++) {
      #ifdef VERBOSE
        printf("i = %5d, buoy = %s, %6.2f %6.2f\n",i, buoy[i].station_id, 
          buoy[i].latitude, buoy[i].longitude); fflush(stdout);
      #endif
      tbuoy = buoy[i];
    }
    curno = i;
    tbuoy = buoy[curno];
    tbuoy.set_time();
    used[curno] = TRUE;
    count = 1;

    for (i = curno+1 ; i < nnear; i++) {
      if (buoy[curno].near(buoy[i], buoy[curno].station_id, deltat) &&
             (used[i] == FALSE) ) {
        tbuoy.latitude += buoy[i].latitude;
        tbuoy.longitude += buoy[i].longitude; //Refine later     
        #ifdef VERBOSE
          printf("matched buoy %5d ",i);
          printf("%3d  %2d %2d %5.2f  %6.2f %6.2f %s\n",
            skpts[i], buoy[i].month, buoy[i].day, buoy[i].hour,
            buoy[i].latitude, buoy[i].longitude, buoy[i].station_id);
        #endif
        used[i] = TRUE;
        count += 1; 
      }
    }
    tbuoy.latitude /= count;
    tbuoy.longitude /= count;
    skpts[ntrimmed] = skpts[curno];
    used[ntrimmed] = TRUE;
    buoy[ntrimmed] = tbuoy;
    ntrimmed += 1;
    curno += 1;
    #ifdef VERBOSE
      printf("Count obs to the location %d\n",count);
    #endif
  }
  ntrimmed -= 1;
  printf("%d buoy reports after averaging locations vs. %d\n",ntrimmed, nnear);
  if (ntrimmed < 2) { return 2; }
  for (i = 0; i < ntrimmed; i++) {
    printf("%3d  %2d %2d %5.2f  %6.2f %6.2f %s\n",
      skpts[i], buoy[i].month, buoy[i].day, buoy[i].hour,
      buoy[i].latitude, buoy[i].longitude, buoy[i].station_id);
  }
  /////////////////////////////////////////////////////////////////////////
  // Have some buoy reports to try to compute a set of forecasts from.
  // Trimmed to one report per buoy per day, centered on synoptic time, and
  // starting from a skiles point (or some day in previous 16 was on an skpt). 
  //-- find first report which is near a skiles point
  curno = 0;
  while (curno < ntrimmed) {
    long t1;
    buoy_report dummy;
    float dx, dy, obsdir, fdummy;
    float dir[16][SKILES], dist[16][SKILES];
    int datelast=0, date, funit=10, retcode;

    while (skpts[curno] == 0 ) {
      curno += 1;
    }
    // Look for next report from this buoy within the forecast interval:
    for (i = curno+1; i < ntrimmed; i++) {
      if (buoy[curno].near(buoy[i], buoy[curno].station_id, delta_fcst) ) {
        t1 = mktime(&buoy[i].obs_time) - mktime(&buoy[curno].obs_time) ;
        t1 /= 86400;
        if (t1 < 0) {
          dummy = buoy[i];
          buoy[i] = buoy[curno];
          buoy[curno] = dummy;
          t1 *= -1;
        }
      date = buoy[curno].day + 100*buoy[curno].month + 100*100*buoy[curno].year;
      if (date != datelast) {
        getfcst_(&date, &funit, &dir[0][0], &dist[0][0], &retcode);
        if (retcode != 0) {
          printf("Need to trap on getfcst errors! retcode = %d\n",retcode);
        }
        datelast = date;
      }
        // Observed direction of drift:
           dx = arcdis_(buoy[curno].longitude, buoy[curno].latitude, 
                   buoy[i].longitude, buoy[curno].latitude)          ;     
           dy = arcdis_(buoy[curno].longitude, buoy[curno].latitude, 
                   buoy[curno].longitude, buoy[i].latitude)          ;     
           obsdir = wdir_(dx, dy, fdummy);
           printf("fcst %2d %2d %2d  %3d %2d  %5.2f %6.2f to %5.2f %6.2f  %5.1f %5.1f  %5.1f %5.1f\n",
           buoy[curno].year, buoy[curno].month, buoy[curno].day,
           skpts[curno], t1, 
               buoy[curno].latitude, buoy[curno].longitude,
               buoy[i].latitude, buoy[i].longitude, 
            arcdis_(buoy[curno].longitude, buoy[curno].latitude, 
                    buoy[i].longitude, buoy[i].latitude), obsdir,
                    nmtokm*dist[t1-1][skpts[curno]-1], dir[t1-1][skpts[curno]-1 ]  );
           fprintf(fcstout,"%2d %2d %2d  %3d %2d  %5.2f %6.2f to %5.2f %6.2f  %5.1f %5.1f  %5.1f %5.1f\n",
           buoy[curno].year, buoy[curno].month, buoy[curno].day,
           skpts[curno], t1, 
               buoy[curno].latitude, buoy[curno].longitude,
               buoy[i].latitude, buoy[i].longitude, 
            arcdis_(buoy[curno].longitude, buoy[curno].latitude, 
                    buoy[i].longitude, buoy[i].latitude), obsdir,
                    nmtokm*dist[t1-1][skpts[curno]-1], dir[t1-1][skpts[curno]-1 ]  );
      }
    } 
    curno += 1;
  }
  
  } // end of overall bracket for our averaging and forecast obs loop

////////////////////////////////////////////////////////////////////
// at long last, in the file fcstout we have a set of forecast to observation matchups.


  return 0;
}
