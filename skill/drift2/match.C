#include <stdio.h>
#include <time.h>
#include <math.h>

#include "buoy.h"
#include "mvector.h"

#define SKILES 207
#define MAXBUOYS 10000
#define PRUNE_FREQ 400
#define nmtokm 1.852
#define MAX_DRIFT 40.0
#define MIN_DRIFT  0.4

extern "C" float wdir_(float &dx, float &dy, float &fdummy);
extern "C" void getfcst_(int *date, int *funit, float *dir, float *dist, 
                           int *code);
int nearwhich(buoy_report &x, latpt *locs, int nlocs, float range) ;
bool wasnear(buoy_report &x, mvector<avbuoy> &y, int active, time_t range);
int prune(mvector<avbuoy> &y, int &active, 
        time_t fcst_range, time_t buffer_range);
void make_forecast(avbuoy &initial, avbuoy &final, const int skpt, FILE *fout);

int main(int argc, char *argv[]) {
  FILE *fin1, *fin2, *fout, *fcstout;
  latpt loc[SKILES];
  int i, skileno, initial, nnear, skpt[MAXBUOYS];
  avbuoy buoy;
  mvector<avbuoy> buoylist(MAXBUOYS);
  float lat, lon;
  float space, time;
  int fcst_length;
  
  time_t deltat, delta_fcst, buffered_range;

/////////////////////////////////////////////////////////
// Set up arguments/control values.
  if (argc < 6) {
    printf("Too few arguments\n");
    return 1;
  }
  space = atof(argv[1]);
  time  = atof(argv[2]);
  deltat = (time_t) (3600 * time);
  fcst_length = atoi(argv[3]);
  delta_fcst = 24 * 3600 * fcst_length;
  //printf("Delta_fcst = %d\n",(int) delta_fcst); fflush(stdout);
  fin2 = fopen(argv[4],"r");
  if (fin2 == (FILE*) NULL) {
    printf("Failed to open %s for reading \n",argv[4]);
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
/////////////////////////////////////////////////////////
  nnear = 0;
  while (!feof(fin2) ) {
     buoy.read(fin2);

     if (nearwhich(buoy, loc, SKILES, space) != -1) {
       fprintf(fout, "%3d ",nearwhich(buoy, loc, SKILES, space)+1 );
       buoy.write(fout);
       buoylist[nnear] = buoy;
       if (nnear > PRUNE_FREQ ) {
         if ( prune(buoylist, nnear, delta_fcst, (time_t)delta_fcst/2) < 1 ) {
           printf("Failed to prune list successfully!\n"); fflush(stdout);
           return 1;
         }
         //printf("after prune, nnear = %d\n", nnear); fflush(stdout);
       }
       else {
         nnear += 1; 
       }
     }
     else if ( wasnear(buoy, buoylist, nnear, delta_fcst) ) {
       fprintf(fout, "%3d ", 0);
       buoy.write(fout);
     }
  }
/////////////////////////////////////////////////////////////////////////
// Now have a list of buoy reports, along with skiles points that they
//  are close to.  Generate pseudo forecasts,
  rewind(fout);
  i = 0;
  while (!feof(fout) ) {
    fscanf(fout, "%d ", &skpt[i]);
    buoylist[i].read(fout);
    i++;
  } 
  //printf("Found %d locations to work with \n",i);
  nnear = i;

  buffered_range = (time_t) (delta_fcst + (time_t) 86400/2);
// ---- must only use initials which have skpt values
  for (initial = 0; initial < nnear; initial++ ) {
    i = initial+1;
    if (skpt[initial] == 0) continue;
    while ( buoylist[i].get_secs() - buoylist[initial].get_secs() < 
                  delta_fcst + 2*86400 && i < nnear ) {
       if (buoylist[initial].near(buoylist[i], buoylist[initial].station_id, 
                buffered_range) ) {
         /// This is point for call to writing out forecast equivalent
         make_forecast(buoylist[initial], buoylist[i], skpt[initial], fcstout);
       }
       i++;
    }
  } 



/////////////////////////////////////////////////////////////////////////

  return 0;
}

// Name which skiles point a buoy report was near, -1 if none 
// note that we're doing a descending search as the low-numbered 
//   points don't show up in the data set for some reason.
int nearwhich(buoy_report &x, latpt *locs, int nlocs, float range) {
   int i = nlocs - 1;
   while (i >= 0 ) {
     if (x.near(locs[i], range) ) return i;
     i--;
   }
   return -1;
} 

// Return true if this buoy was previously (time range specified) near
//   a skiles point
// Do reverse check because most recent will be last in file
bool wasnear(buoy_report &x, mvector<avbuoy> &y, int active, time_t range) {
   int i = active - 1;

   while (i >= 0) {
     if (x.near(y[i], x.station_id, range)) {
       return true; 
     }
     i--;
   } 
   return false;
}

//Prune a list of buoy reports, down to within some time interval of
//  the last report.
//Note that 'last' refers to the subscript number of the last report.
//  there are last+1 elements in used
int prune(mvector<avbuoy> &buoylist, int &last, 
        time_t fcst_range, time_t buffer_range) {
  int i = last - 1, j;
  time_t long_range = fcst_range + buffer_range;
  
  //printf("last, on entry %d\n", last);
  //printf("%d %d times %d\n", (int) buoylist[i].get_secs(), 
  //               (int) buoylist[last].get_secs(), (int) long_range); 
  //fflush(stdout);
  while (i > 0 && abs(buoylist[i].get_secs() - 
                      buoylist[last].get_secs()) < long_range) {
     i--;
  }
  if (i <= 0) {
    printf("Error, cannot prune list!\n");
    return -1;
  }
  //printf("In prune, i = %d\n",i); fflush(stdout);

  for (j = i; j <= last; j++) {
    buoylist[j-i] = buoylist[j];
  }
  last = last - i + 1;
  return last;
}

// Take two buoys (av. buoys) and make a forecast out of their positions
void make_forecast(avbuoy &initial, avbuoy &final, const int skpt, FILE *fout) {
  int lead;
  float dist, dir, dx, dy, fdummy;
  tm *round_time;
  time_t delta;


  dx = arcdis_(initial.longitude, initial.latitude,
                   final.longitude, initial.latitude);

  dy = arcdis_(initial.longitude, initial.latitude,
                   initial.longitude, final.latitude);

  dir = wdir_(dx, dy, fdummy);

  dist = sqrt(dx*dx + dy*dy); // Need to test effect of this vs. arcdis


  lead = (int) ( (final.get_secs() - initial.get_secs())/86400. + 0.5 );

  delta = initial.get_secs() ;
  round_time = gmtime( &delta );
  if (round_time->tm_hour >= 12) {
    delta = 86400 - 3600*round_time->tm_hour - 60 * round_time->tm_min - 
         round_time->tm_sec;
    delta += initial.get_secs();
    round_time = gmtime(&delta);
  }

// QC and print out to appropriate files: 
  if (lead == 0) { 
    printf("%d %d ", (int) initial.get_secs(), (int) final.get_secs() ); 
    printf(
    "%3d %2d %2d %2d  %5.2f %6.2f to %5.2f %6.2f  %5.1f %5.1f %2d %s %6.1f\n",
     skpt, round_time->tm_year, round_time->tm_mon+1, round_time->tm_mday,  
         initial.latitude, initial.longitude, final.latitude, final.longitude, 
         dist, dir, lead, initial.station_id, dist/lead);
    return;
  }
// Make the minimum decrease for longer leads, as it is possible to loop
//   back around
  if (dist/lead > MAX_DRIFT || dist/lead < MIN_DRIFT/sqrt(lead) ) {
    printf(
    "%3d %2d %2d %2d  %5.2f %6.2f to %5.2f %6.2f  %5.1f %5.1f %2d %s %6.1f\n",
     skpt, round_time->tm_year, round_time->tm_mon+1, round_time->tm_mday,  
         initial.latitude, initial.longitude, final.latitude, final.longitude, 
         dist, dir, lead, initial.station_id, dist/lead);
  }
  else { 
  fprintf(fout, 
    "%3d %2d %2d %2d  %5.2f %6.2f to %5.2f %6.2f  %5.1f %5.1f %2d %s %6.1f\n",
    skpt, round_time->tm_year, round_time->tm_mon+1, round_time->tm_mday,  
         initial.latitude, initial.longitude, final.latitude, final.longitude, 
         dist, dir, lead, initial.station_id, dist/lead);
  }

  return;
}
