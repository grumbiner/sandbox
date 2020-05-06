#include "ncepgrids.h"
#include "time_series.h"
#include <errno.h>
#include <stack>
using namespace std;

#define MAXPTS  54221
#define TIME 10957
int bcount = 0;

float correl(int x, int y, mvector<float> *datary) ;

//void region_find(const int x, const int y, const ijpt refloc, 
void region_find(const ijpt tloc, const ijpt refloc, 
  global_ice<int> &indices, mvector<float> *datary,
  grid2<stack<ijpt> > &locs, grid2<stack<float> > &scores,
  float rcrit) ; 

int main(int argc, char *argv[]) {
  FILE *fin;
  mvector<float> avg(MAXPTS), count(MAXPTS), sd(MAXPTS);
  mvector<int> index(MAXPTS);
  mvector<ijpt> loc(MAXPTS);
  mvector<latpt> ll(MAXPTS);

  int i, j, k = 0, ti, tj;
  ijpt tloc;
  latpt tll;

//
  float rcrit;
  rcrit = atof(argv[1]);
  printf("rcrit = %f\n",rcrit);

// Scan the summary file for point locations
  int countin, indexin;
  float ain, sdin;
  float lat, lon, d1, f1, f2, f3;
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
  printf("done reading in the index file\n"); fflush(stdout);

/////////////////////////////////////////////////////////////////////
  mvector<float> x[MAXPTS];
  global_ice<int> indices;
  indices.set((int) -1);
  
  fin = fopen("transposed_data","r");
  for (i = 0; i < MAXPTS; i++) {
    x[i].resize(TIME);
    x[i].binin(fin);
    //printf("average of %d is %f\n",i, x[i].average() ); fflush(stdout);
    indices[loc[i] ] = i; // the transposed data are referenced by sequence number, not ijloc 
  }
  fclose(fin);
  printf("done reading in the data file\n"); fflush(stdout);
  printf("index min, max %d %d\n",indices.gridmax(), indices.gridmin() );
  fflush(stdout);
  //return 0;
/////////////////////////////////////////////////////////////////////
  grid2<stack<ijpt>  > region_locs(indices.xpoints(), indices.ypoints());
  grid2<stack<float> > region_scores(indices.xpoints(), indices.ypoints());
  // now that we have data, flood out r < rcrit
  // -- compute the correlations on the fly and save results to a stack

  //for (k = 0; k < MAXPTS; k++) {
  for (k = MAXPTS - 1; k > 0; k--) {
    tloc = loc[k];
    printf("point %5d loc %3d %3d  %7.2f %7.2f ",k, loc[k ].i, loc[k ].j, ll[k].lat, ll[k].lon); fflush(stdout);

    region_find(tloc, loc[k], indices,
         x, region_locs, region_scores, rcrit);

    printf(" #points in its region: %5d\n", (int)region_locs[tloc].size() );
    fflush(stdout);  
  }
  
  return 0;
}

void region_find(ijpt tloc, const ijpt refloc, 
  global_ice<int> &indices, mvector<float> *datary,
  grid2<stack<ijpt> > &locs, grid2<stack<float> > &scores,
  float rcrit) { 

  bcount += 1;
 
//  ijpt tloc;
//  tloc.i = x; tloc.j = y;

  // need to do something to avoid simply cycling around a line of longitude
  // manage longitude wrap-around 
//  if (tloc.i >= scores.xpoints()) tloc.i -= scores.xpoints();
//  if (tloc.i < 0)               tloc.i += scores.xpoints();
  //manage pole-crossing:
//  if (tloc.j >= scores.ypoints()) {
//    tloc.j = (scores.ypoints()-1) - (tloc.j - scores.ypoints());
//    tloc.i = tloc.i + 180.0/0.5; 
//    if (tloc.i >= scores.xpoints()) tloc.i -= scores.xpoints();
//  }
//  if (tloc.j < 0) {
//    tloc.j = -tloc.j;
//    tloc.i = tloc.i + 180.0/0.5; 
//    if (tloc.i >= scores.xpoints()) tloc.i -= scores.xpoints();
//  }
// check that the ij space point has a valid index
  if (indices[tloc] == -1 || tloc.i <= 0 || tloc.j <= 0 || 
        tloc.j >= scores.ypoints() || tloc.i >= scores.xpoints() ) {
    bcount -= 1;
    return;
  }

  float score = correl(indices[tloc], indices[refloc], datary);
  printf("score %5d %5d = %f bcount %d tloc %d %d refloc %d %d\n",indices[tloc], indices[refloc], score, bcount, tloc.i, tloc.j, refloc.i, refloc.j); 
  fflush(stdout);
  if ( score >= rcrit) {
    scores[refloc].push(score);
    locs[refloc].push(tloc);

    // recursive portion:
    tloc.j+= 1; region_find(tloc, refloc, indices, datary, locs, scores, rcrit);
    tloc.i+= 1; region_find(tloc, refloc, indices, datary, locs, scores, rcrit);
    tloc.j-= 1; region_find(tloc, refloc, indices, datary, locs, scores, rcrit);
    tloc.i-= 1; region_find(tloc, refloc, indices, datary, locs, scores, rcrit);
  }

  bcount -= 1;
  return;
}

float correl(int x, int y, mvector<float> *datary) {
  double sumx = 0., sumy = 0., sumxy = 0., sumx2 = 0., sumy2 = 0.;
  int i, nx = datary[0].xpoints();

  if (x < 0 || y < 0) return 0;
  //printf("computing score %d %d nx = %d\n",x,y, nx);

  for (i = 0; i < datary[0].xpoints(); i++) {
    sumx  += datary[x][i];
    sumy  += datary[y][i];
    sumxy += datary[x][i]*datary[y][i];
    sumx2 += datary[x][i]*datary[x][i];
    sumy2 += datary[y][i]*datary[y][i];
  }

  // note, must be returning r^2, or other non-negative score
  float sigx = (nx*sumx2 - sumx*sumx);
  float sigy = (nx*sumy2 - sumy*sumy);
  //printf("sigx sigy = %f %f  %e %e\n",sigx, sigy, sumx, sumy);
  float r2 = 0.;
  if (sigx <= 0 || sigy <= 0) {
    return r2; } 
  else {
    sigx = sqrt(sigx);
    sigy = sqrt(sigy);
    r2 = (nx*sumxy - sumx*sumy) / sigx / sigy;
  }
  return r2;
}
