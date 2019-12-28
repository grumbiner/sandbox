#include "ncepgrids.h"
#include "time_series.h"
#include <errno.h>

//#define MAXPTS  57817
#define MAXPTS  54221
int bcount = 0;

void select_key(mvector<ijpt> &locs, int nkeys, int &key_no, 
          mvector<float> &correl, global_ice<int> &remap, float &rcrit) ;

// Use 'key' points and attach to them the points which have highest
// correlation to them (vs. other 'key' points)
//
//
//

int main(int argc, char *argv[]) {
  FILE *fin, *fout;
  mvector<float> avg(MAXPTS), count(MAXPTS), sd(MAXPTS), norm(MAXPTS);
  mvector<int>   index(MAXPTS);
  mvector<ijpt>  loc(MAXPTS);
  mvector<latpt> ll(MAXPTS);

  mvector<latpt> key_ll(MAXPTS);
  mvector<ijpt>  key_loc(MAXPTS);

  mvector<float> correl(MAXPTS);

  char fname[900];
  global_ice<float> region_no;
  global_ice<int> remap;
  float rcrit;

  int i, j, ti, tj, tindex, nkeys, key_no;
  int countin, indexin;
  float ain, sdin;
  float lat, lon, d1, f1, f2, f3;
  ijpt tloc;
  latpt tll;


  rcrit = atof(argv[1]);
  printf("rcrit = %f\n",rcrit);
  region_no.set((float) -1.0);

// Scan the summary file for point locations
  fin = fopen("allice.summary","r");
  if (fin == (FILE*) NULL) {
    printf("failed to open allice.summary\n");
    return 1;
  }
  for (i = 0; i < MAXPTS; i++) {
    fscanf(fin, "%d %d %d %f %f %d %f %f %f %f %f %f\n",&indexin, &ti, &tj, &lat, &lon, &countin, &ain, &d1, &sdin, &f1, &f2, &f3);
    index[i] = indexin;
    tloc.i = ti; tloc.j = tj;
    loc[i] = tloc;
    tll.lat = lat; tll.lon = lon;
    ll[i]   = tll;
    count[i] = (float) countin;
    avg[i] = ain;
    sd[i]  = sqrt(sdin);
    
    remap[loc[i] ] = i;
  }
  fclose(fin);
  printf("remap %d %d \n",remap.gridmax(), remap.gridmin() ); fflush(stdout);

// Scan the key file for point locations 
  fin = fopen("keys","r");
  if (fin == (FILE*) NULL) {
    printf("failed to open keys\n");
    return 1;
  }

  fscanf(fin, "%d\n",&nkeys); 
  //printf("nkeys = %d\n",nkeys); fflush(stdout);
  for (i = 0; i < nkeys; i++) {
    //fscanf(fin, "%d %f %f\n",&indexin, &lat, &lon);
    fscanf(fin, "%f %f\n",&lat, &lon);
    tll.lat  = lat; tll.lon = lon;
    key_ll[i]    = tll;
    key_loc[i]   = region_no.locate(key_ll[i]);
    printf("%2d %6.2f %6.2f  %3d %3d\n",i, lat, lon, key_loc[i].i, key_loc[i].j);
  } 
  fclose(fin);

  fout = fopen("regions","w");
  
  for (j = 0; j <= 6; j++) {
      sprintf(fname, "correlout.%d",j);    
      fin = fopen(fname,"r"); 
  for (i = 0; i < min(9000,MAXPTS-j*9000); i++) {
    tindex = i + 9000*j;
    correl.binin(fin);

    select_key(key_loc, nkeys, key_no, correl, remap, rcrit);
    region_no[ loc[tindex] ] = (float) key_no;

    printf("%6d  %3d %3d has key_no %3d\n",tindex, loc[tindex].i, loc[tindex].j, key_no);
    //if ((tindex % 1000) == 0) { region_no.binout(fout); }
    fflush(stdout);

  }
      fclose(fin);
  }

  region_no.binout(fout);
  fclose(fout);
  printf("now see about histogram gridmax = %f\n", region_no.gridmax() ); fflush(stdout);

  // area histogram by point #
  double asum = 0.0, cumulative = 0.0;
  for (i = -2; i <= (int) region_no.gridmax(); i++) {
    asum = 0;
    for (tloc.j = 0; tloc.j < region_no.ypoints(); tloc.j++) {
    for (tloc.i = 0; tloc.i < region_no.xpoints(); tloc.i++) {
      if (region_no[tloc] == (float) i) asum += region_no.cellarea(tloc);
    }
    }
    cumulative += asum;
    if (i < 0) {
      printf("csv\t%4d\t%6.2f\t%6.2f\t%12.3f\t%9.3f\n",i, 0., 0.,
                asum/1e6, asum/1e9);
    }
    else {
      printf("csv\t%4d\t%6.2f\t%6.2f\t%12.3f\t%9.3f\n",i, key_ll[i].lat, key_ll[i].lon,
                asum/1e6, asum/1e9);
    }
  }
  printf("final total area %f\n",cumulative / 1e12);
    
  
  return 0;
}

void select_key(mvector<ijpt> &locs, int nkeys, int &key_no, mvector<float> &correl, global_ice<int> &index, float &rcrit) {
  int i;
  float score = -900., r;
  key_no = -2;

  for (i = 0; i < nkeys; i++) {
    r = correl[index[locs[i] ] ];
    //printf("i j correl %d %d %d %f\n",locs[i].i, locs[i].j, index[locs[i] ], r);

    if ( (r*r > score) && (r*r > rcrit) ) {
      key_no = i;
      score  = r*r;
    }
  }
  printf("correl = %f\n",score);

  return;
}
