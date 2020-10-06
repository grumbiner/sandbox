#include "ncepgrids.h"
#include "eta.h"
#include <stdlib.h>
#include <string.h>

#define BUFLIM 900
#define PTSMAX 20000

int main(int argc, char *argv[]) {
  char line[BUFLIM], tmp[BUFLIM];
  int i, j, k;
  int pt, ptno, count;
  latpt ll;
  fijpt floc;
  ijpt iloc;
  mvector<latpt> locs(PTSMAX);
  FILE *finloc, *findat, *fout;
  float sst;
  eta32<float> etasst[34];
  eta32<int>   etacount[34];
  
  finloc = fopen(argv[1], "r");
  findat = fopen(argv[2], "r");
  if (finloc == (FILE *) NULL ) {
    printf("Failed to open location file %s\n",argv[1]);
    return 1;
  }
  if (findat == (FILE *) NULL ) {
    printf("Failed to open data file %s\n", argv[2]);
    return 2;
  }

  for (i = 0; i < 34; i++) {
    etacount[i].set(0);
    etasst[i].set((float) 0.0);
  }

// Get locations
  for (i = 0; i < 8; i++) {
    fgets(line, BUFLIM, finloc);
    //printf("%s",line); fflush(stdout);
  }

  i = 0;
  while (!feof(finloc) ) {
    fgets(line, BUFLIM, finloc);
    //printf("%s",line); fflush(stdout);
    sscanf(line, "%d %f %f",&pt, &ll.lat, &ll.lon);
    ll.lon = 360. - ll.lon;
    locs[pt - 1] = ll;
    i+= 1;
  }
  count = i-3;
  printf("count = %d\n",count);

// Read past data file header
  for (i = 0; i < 10; i++) {
    fgets(line, BUFLIM, findat);
    //printf("%s",line); fflush(stdout);
  }

  for (k = 0; k < 3; k++) {  // Have to loop through 3 times 
    for (i = 0; i < count; i++) {
      fgets(line, BUFLIM, findat);
      printf("%s",line); fflush(stdout);
      strncpy(tmp, line, 6);
      tmp[6] = '\0';
      ptno=atoi(tmp) - 1; // -1 needed as data sources are 1-based instead of 0

      // Loop through the days of the month across the line
      for (j = 0; j < 11; j++) {
        strncpy(tmp, &(line[6+j*6]), 6);
        sst = atof(tmp);
        //printf("%d %f\n",ptno, sst); 
        ll = locs[ptno];
        floc = etasst[0].locate(ll);
        if (sst > -90) {
          etasst[j+11*k+1][floc] += sst;
          etacount[j+11*k+1][floc] += 1;
        }
      }
    }
    if (k == 0 || k == 1) {
      for (i = 0; i < 6; i++) {
        fgets(line, BUFLIM, findat);
      }
    }
  }

// Average
  for (k = 0; k < 34; k++ ) {
    for (iloc.j = 0; iloc.j < etasst[0].ypoints(); iloc.j++) {
    for (iloc.i = 0; iloc.i < etasst[0].xpoints(); iloc.i++) {
       if (etacount[k][iloc] != 0) etasst[k][iloc] /= etacount[k][iloc];
    }
    }
    sprintf(line, "%s.%02d",argv[3],k);
    fout = fopen(line, "w");
    etasst[k].binout(fout);
    fclose(fout);
  }

  return 0;
}
