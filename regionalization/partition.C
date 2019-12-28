#include "ncepgrids.h"
#include "time_series.h"
#include <errno.h>

//#define MAXPTS  57817
#define MAXPTS  54221
int bcount = 0;

void   flood(global_ice<unsigned char> &land, global_ice<bool> &tmask, 
             global_ice<float> &corin, float &rcrit, ijpt &loc );
void newFill(const int x, const int y, grid2<unsigned char> &land, grid2<bool> &tmask, grid2<float> &corin, float &rcrit) ;

int main(int argc, char *argv[]) {
  FILE *fin, *fout;
  mvector<float> avg(MAXPTS), count(MAXPTS), sd(MAXPTS), norm(MAXPTS);
  mvector<int> index(MAXPTS);
  mvector<ijpt> loc(MAXPTS);
  mvector<latpt> ll(MAXPTS);

  mvector<float> score(MAXPTS);
  mvector<int> score_index(MAXPTS);
  mvector<latpt> score_ll(MAXPTS);
  mvector<ijpt> score_loc(MAXPTS);

  mvector<float> correl(MAXPTS);
  global_ice<float> corin;

  int i, j, ti, tj, points, total = 0;
  int countin, indexin;
  float ain, sdin;
  float lat, lon, d1, f1, f2, f3;
  ijpt tloc;
  latpt tll;


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
  }
  fclose(fin);

// Scan the score file for point locations and scores -- presume already sorted by score
  fin = fopen("scores","r");
  if (fin == (FILE*) NULL) {
    printf("failed to open scores\n");
    return 1;
  }

  index = 0;
  score = 0.0;
  for (i = 0; i < MAXPTS; i++) {
  //for (i = 0; i < 9000; i++) {
    fscanf(fin, "%d %d %f %f %f\n",&indexin, &ti, &lat, &lon, &f1);
      //printf("i = %d %d  %f %f\n",i, indexin, lat, lon); fflush(stdout);
    score_index[i] = indexin;
    tll.lat  = lat; tll.lon = lon;
    score_ll[i]    = tll;
    score[i] = f1;

    score_loc[i]   = corin.locate(ll[i]);
  } 
  fclose(fin);


  int k = 0, nreg = 0;
  char fname[900];
  global_ice<bool> mask, tmask;
  global_ice<unsigned char> land;
  global_ice<float> region_no;
  float rcrit;
  FILE *corout;

  rcrit = atof(argv[1]);
  printf("rcrit = %f\n",rcrit);
  mask.set(false);
  tmask.set(false);
  region_no.set((float) -1.0);

  fin = fopen("seaice_newland","r");
  land.binin(fin);
  fclose(fin);

  fout = fopen("regions","w");
  corout = fopen("cormaps","w");
  
  while ( nreg < 30000 ) {
    //printf("nreg = %d\n",nreg); fflush(stdout);
    while ( (mask[score_loc[score_index[k] ] ] ) && (k < MAXPTS) ) {
      k++;
    }
    if (k >= MAXPTS) break;
    //printf("past while %d %d  mask %d score %f  k %d\n",score_loc[score_index[k] ].i, 
    //       score_loc[score_index[k] ].j, mask[score_loc[score_index[k] ] ], score[k], k);
     
    //printf("%d %d  %d %d  %f %f\n",k, score_index[k], score_loc[score_index[k] ].i, 
    //         score_loc[score_index[k] ].j, ll[score_index[k] ].lat, ll[score_index[k] ].lon);

    j = score_index[k] / 9000;
    i = score_index[k] % 9000;
    //printf("j i = %d %d\n", j, i); fflush(stdout);
    sprintf(fname, "correlout.%d",j);    
    fin = fopen(fname,"r"); 
    fseek(fin, i*sizeof(float)*MAXPTS, SEEK_SET);
    correl.binin(fin);
    fclose(fin);

    corin.set((float) 0.0);
    for (i = 0; i < MAXPTS; i++) {
      corin[loc[i] ] = correl[i];
    }
    //corin.binout(corout);

    // now that we have a map of correlations, flood out until land or r < rcrit
    tmask = mask;
    flood(land, tmask, corin, rcrit, score_loc[score_index[k] ]);

    points = 0;
    for (tloc.j = 0; tloc.j < mask.ypoints(); tloc.j++) {
    for (tloc.i = 0; tloc.i < mask.xpoints(); tloc.i++) {
      if (!mask[tloc] && tmask[tloc]) {
        region_no[tloc] = nreg;
        points++;
      }
    }
    }
    //region_no.binout(fout);
    if (points > 0) {
      tll = region_no.locate(score_loc[score_index[k] ]);
      printf("region %4d has %5d points key pt %3d %3d  %7.2f %7.2f\n",nreg, points,
          score_loc[score_index[k] ].i, score_loc[score_index[k] ].j, tll.lat, tll.lon ); 
      fflush(stdout);
    }
    total += points;

    mask = tmask;
    nreg++;
  }
  region_no.binout(fout);
  fclose(fout);

  printf("%d total points accounted for\n",total);
  // area histogram by point #
  // note that a number of pts won't be counted because count < 1000
  double asum = 0.0, cumulative = 0.0;
  for (i = 0; i < region_no.gridmax(); i++) {
    asum = 0;
    for (tloc.j = 0; tloc.j < mask.ypoints(); tloc.j++) {
    for (tloc.i = 0; tloc.i < mask.xpoints(); tloc.i++) {
      if (region_no[tloc] == i) asum += mask.cellarea(tloc);
    }
    }
    cumulative += asum;
    //printf("region %3d area %12.3f km^2 %9.3f kkm^2 \n",i, asum/1e6, asum/1e9);
    printf("csv\t%4d\t%12.3f\t%9.3f\n",i, asum/1e6, asum/1e9);
  }
  printf("final total area %f\n",cumulative / 1e12);
    
  
  return 0;
}

void flood(global_ice<unsigned char> &land, global_ice<bool> &tmask, 
             global_ice<float> &corin, float &rcrit, ijpt &loc ) {

  newFill(loc.i, loc.j, land, tmask, corin, rcrit);  
  
  return;
}
void newFill(const int x, const int y, grid2<unsigned char> &land, grid2<bool> &mask, grid2<float> &corin, float &rcrit) {
  ijpt tloc;

  tloc.i = x; tloc.j = y;
  // manage longitude wrap-around 
  if (tloc.i >= land.xpoints()) tloc.i -= land.xpoints();
  if (tloc.i < 0)               tloc.i += land.xpoints();
  //and pole-crossing:
  if (tloc.j >= land.ypoints()) {
    tloc.j = (land.ypoints()-1) - (tloc.j - land.ypoints());
    tloc.i = tloc.i + 180.0/0.5; 
    if (tloc.i >= land.xpoints()) tloc.i -= land.xpoints();
  }
  if (tloc.j < 0) {
    tloc.j = -tloc.j;
    tloc.i = tloc.i + 180.0/0.5; 
    if (tloc.i >= land.xpoints()) tloc.i -= land.xpoints();
  }

  bcount += 1;
 
  if ( !mask[tloc] && (corin[tloc] > rcrit) && (land[tloc] == (unsigned char) 0) ) {
    mask[tloc] = true;

    // recursive portion:
    newFill(x+1, y,   land, mask, corin, rcrit);
    newFill(x  , y+1, land, mask, corin, rcrit);
    newFill(x-1, y,   land, mask, corin, rcrit);
    newFill(x  , y-1, land, mask, corin, rcrit);
  }

  bcount -= 1;
  return;
}
