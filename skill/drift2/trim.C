#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

#include "buoy.h"
#include "mvector.h"

// Trim down a buoy file.  
// Step 1: select only those reports that are from A) North of 45N,
//    B) within HH hours of 00 UTC  C) From drifting buoys
// Step 2: average down the reports to single locations.
// Arguments:
//   HH -- size of time window around 00 UTC
//   Buoy file for input
//   File for step one output
//   File for step two output
// Robert Grumbine
// Last Modified 4 April 2000
 
#define MAXBUOY 4000
void listaverage(mvector<buoy_report> &buoylist, mvector<bool> &used, 
    int count, FILE *fout) ;
int timesort(buoy_report *x, buoy_report *y);
int idsort(buoy_report *x, buoy_report *y);

int main(int argc, char *argv[]) {
  FILE *fin, *fout1, *fout2; // out1 is all positions, fout2 is averaged
  mvector<buoy_report> buoylist(MAXBUOY);
  mvector<bool> used(MAXBUOY);
  buoy_report buoy;
  int count;
  float time;
  int i = 0, j;
  time_t deltat;

  time  = atof(argv[1]);
  deltat = (time_t) (3600 * time);
  fflush(stdout);
  fin = fopen(argv[2],"r");
  if (fin == (FILE*) NULL) {
    printf("Failed to open %s for reading\n",argv[2]);
    return 1;
  }
  fout1 = fopen(argv[3], "w+");
  if (fout1 == (FILE*) NULL) {
    printf("Failed to open %s for read/write\n",argv[3]);
    return 1;
  }
  fout2 = fopen(argv[4], "w");
  if (fout2 == (FILE *) NULL) {
    printf("Failed to open %s for writing\n",argv[4]);
    return 1;
  }

///////////////////
  rewind(fin);
  count = 0;
  while (!feof(fin) ) {
     buoy.read(fin);
     if ( ( !buoy.poleward(45.) || !buoy.synoptic(time) 
      || !buoy.isdrifter() ) || (buoy.sst > 5. && buoy.sst < 99.)  ) continue ;
//  sst test added 6 April 2000 -- sea ice drifters can't be warm.  99.9 is
//     flag value, and we do need flag value points as many drifters don't
//     report sst. 
     
     buoylist[count] = buoy;
     if (count > 0.85 *MAXBUOY) {
       // Put in time order
       qsort(&buoylist[0], count, sizeof(buoy_report), &timesort);
       // Write out everything not close to the end of the list
       buoylist[0].write(fout1);
       i = 1;
       while (i < count && 
             ! buoylist[i].near(buoylist[count], (time_t)(4*deltat) ) ) {
         buoylist[i].write(fout1); //write back out to smaller file
         i += 1; 
       }
       // Now shuffle down all the data that were near the end of the file
       for (j = 0; j < count - i; j++) {
         buoylist[j] = buoylist[i+j];
       }
       count -= i;
     }
     count += 1;
  }
  // Ensure that the buffer is cleared!
  qsort(&buoylist[0], count, sizeof(buoy_report), &timesort);
  for (i = 0; i < count; i++) {
     buoylist[i].write(fout1);
  }    
  fclose(fin);

/////////////////// Average each buoy on its daily time window

  rewind(fout1);
  buoy.read(fout1);
  while (!feof(fout1)) {
    buoylist[0] = buoy;
    used[0] = false;
    count = 0;
    // Note that this is trying to be 'cute' -- read in the report and test
    //   it for being close to the first buoy, in time -- the times2 is to
    // get ones on opposite sides of 00.  The list is pre-trimmed, so we
    // don't have to worry about things on the wrong side.
    while (buoy.read(fout1).near(buoylist[0], (time_t) (deltat * 4)) 
           && !feof(fout1) ) {
      count += 1;
      if (count > 0.5 *MAXBUOY) {printf("count is large! %d\n",count); }
      buoylist[count] = buoy;
      used[count]     = false;
    }
    //printf("calling listaverage, count = %d\n",count);
    listaverage(buoylist, used, count, fout2);
  }

  return 0;

}
void listaverage(mvector<buoy_report> &buoylist, mvector<bool> &used, int count, FILE *fout) {
   int i=0, base = 0;
   avbuoy tempor;
   buoy_report   refbuoy;
 
   while (base < count - 1 && !used[i]) {
     if (used[i]) {
       printf("Error, have a used buoy report\n"); 
       printf("i = %d, buoyname = %s \n",i, buoylist[i].station_id);
     }
     tempor  = buoylist[i];
     refbuoy = buoylist[i];
     used[i] = true;
     base    = i+1;
     for (i = base; i <= count; i++) {
       if (!used[i] && buoylist[i].same_id(refbuoy) ) {
         used[i] = true; 
         tempor += buoylist[i];
       }
     }
     tempor.write(fout);
     i = base;
     while (used[i] && i < count ) {
       i += 1;
     }
     if (used[i]) break; // if the last report is used, you're done!
   }
       
   return;
}
int timesort(buoy_report *x, buoy_report *y) {
  if (x->obs_secs < y->obs_secs) {
    return -1;
  }
  else if (x->obs_secs == y->obs_secs) {
    return 0;
  }
  else {
    return 1;
  }
}
int idsort(buoy_report *x, buoy_report *y) {
  return strncmp(x.station_id, y.station_id, 6);
}
