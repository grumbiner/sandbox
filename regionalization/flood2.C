#include "ncepgrids.h"
#include "time_series.h"
#include <errno.h>
#include <stack>
using namespace std;

#define MAXPTS  54221
#define TIME 10957
int bcount = 0;

float correl(int x, int y, mvector<float> *datary) ;

void region_find(const int x, const int y, const ijpt refloc, 
  global_ice<int> &indices, global_ice<bool> &visited, mvector<float> *datary,
  grid2<stack<ijpt> > &locs, grid2<stack<float> > &scores,
  float rcrit) ; 

float locmax(grid2<float> &x, ijpt &loc) ;
void renew_areas(global_ice<float> &areas, global_ice<bool> &visited, grid2<stack<ijpt> > &region_locs);

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

/////////////////////////////////////////////////////////////////////
  grid2<stack<ijpt>  > region_locs(indices.xpoints(), indices.ypoints());
  grid2<stack<float> > region_scores(indices.xpoints(), indices.ypoints());
  global_ice<bool> visited;
  global_ice<int> counts;
  global_ice<float> areas;
  stack<ijpt> xx;
  double sum = 0.;
  ijpt stackloc;
  // now that we have data, flood out r < rcrit
  // -- compute the correlations on the fly and save results to a stack
  counts.set(0);
  areas.set((float) 0);
  //for (k = 0; k < MAXPTS; k++) {
  for (k = MAXPTS ; k > 0; k--) {
    tloc = loc[k];
    visited.set(false);
    printf("point %5d loc %3d %3d  %7.2f %7.2f ",k, loc[k ].i, loc[k ].j, ll[k].lat, ll[k].lon); fflush(stdout);

    region_find(tloc.i, tloc.j, loc[k], indices, visited,
         x, region_locs, region_scores, rcrit);
    counts[loc[k] ] = (int)region_locs[tloc].size();
    // compute the area:
    sum = 0.;
    if (counts[tloc] != 0) {
      xx = region_locs[tloc];
      for (i = 0; i < (int) xx.size(); i++) {
        stackloc = xx.top();
        sum += areas.cellarea(stackloc);
        xx.pop();
      }
    }
    areas[tloc] = sum;
    printf(" #points in its region: %5d area %f \n", (int)region_locs[tloc].size(),sum/1e12 );
    fflush(stdout);  
  }

//////////////////////////////////////////////////////////////////////////
//  Now have the stacks of points that passed the rcrit test
//  Look for the unique
  int pass = 0;
  visited.set(false); // will now use this to mark points that have been used in regions.
  renew_areas(areas, visited, region_locs);
  float amax = 1.e30;
  global_ice<float> belongs;

  belongs.set(-1);

  while (pass < 200 && amax > 1e5*1e6) {
    // location of maximum area's point:
    amax = locmax(areas,tloc);
    tll = areas.locate(tloc);
    //printf("amax = %e\n",amax); fflush(stdout);
    printf("Region number # %d area = %f million km^2 at %3d %3d %7.2f %7.2f\n",
      pass, areas[tloc]/1e12, tloc.i, tloc.j, tll.lat, tll.lon);
    // now mark all points that are highly correlated to tloc as 'visited' and 
    // recompute the areas grid
    xx = region_locs[tloc];
    for (i = 0; i < (int) xx.size(); i++) {
      stackloc = xx.top();
      visited[stackloc] = true;
      belongs[stackloc] = pass;
      xx.pop();
    }
  
    renew_areas(areas, visited, region_locs);

    pass++;
  }
  FILE *fout;
  fout = fopen("belongs","w");
  belongs.binout(fout);
  fclose(fout);


  return 0;
}

//void region_find(ijpt tloc, const ijpt refloc, 
void region_find(const int x, const int y, const ijpt refloc, 
  global_ice<int> &indices, global_ice<bool> &visited, mvector<float> *datary,
  grid2<stack<ijpt> > &locs, grid2<stack<float> > &scores,
  float rcrit) { 

  ijpt loc, tloc;
  tloc.i = x; tloc.j = y;

  bcount += 1;

// need to do something to avoid simply cycling around a line of longitude
//   -- handled by 'visited' array.
//
// manage longitude wrap-around
  if (tloc.i >= scores.xpoints()) tloc.i -= scores.xpoints();
  if (tloc.i < 0)               tloc.i += scores.xpoints();
// manage pole-crossing:
  if (tloc.j >= scores.ypoints()) {
    tloc.j = (scores.ypoints()-1) - (tloc.j - scores.ypoints());
    tloc.i = tloc.i + 180.0/0.5;
    if (tloc.i >= scores.xpoints()) tloc.i -= scores.xpoints();
  }
  if (tloc.j < 0) {
    tloc.j = -tloc.j;
    tloc.i = tloc.i + 180.0/0.5;
    if (tloc.i >= scores.xpoints()) tloc.i -= scores.xpoints();
  }


  if (visited[tloc] || indices[tloc] == -1 || 
        tloc.i < 0 || tloc.j < 0 || 
        tloc.j >= scores.ypoints() || tloc.i >= scores.xpoints() ) {
    bcount -= 1;
    return;
  }

  visited[tloc] = true;
  
  loc.i = tloc.i; loc.j = tloc.j;
  float score = correl(indices[tloc], indices[refloc], datary);
  //printf("score %5d %5d = %f bcount %d tloc %d %d refloc %d %d\n",
  //           indices[tloc], indices[refloc], score, bcount, tloc.i, tloc.j, 
  //           refloc.i, refloc.j); 
  fflush(stdout);
  if ( score >= rcrit) {
    scores[refloc].push(score);
    locs[refloc].push(tloc);

    // recursive portion:
    region_find(x, y+1, refloc, indices, visited, datary, locs, scores, rcrit);
    region_find(x+1, y, refloc, indices, visited, datary, locs, scores, rcrit);
    region_find(x, y-1, refloc, indices, visited, datary, locs, scores, rcrit);
    region_find(x-1, y, refloc, indices, visited, datary, locs, scores, rcrit);
  }

  bcount -= 1;
  return;
}

float correl(int x, int y, mvector<float> *datary) {
  double sumx = 0., sumy = 0., sumxy = 0., sumx2 = 0., sumy2 = 0.;
  int i, nx = datary[0].xpoints();

  if (x < 0 || y < 0) return 0;

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
float locmax(grid2<float> &x, ijpt &loc) {
  float maximum;
  maximum = x.gridmax();
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    if (x[loc] == maximum) return maximum;
  }
  }
  return maximum;
}
void renew_areas(global_ice<float> &areas, global_ice<bool> &visited, 
               grid2<stack<ijpt> > &region_locs) {
  ijpt loc, stackloc;
  stack<ijpt> xx;
  double sum;
  int i;

  for (loc.j = 0; loc.j < areas.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < areas.xpoints(); loc.i++) {
    // compute the area:
    sum = 0.;
    if (region_locs[loc].size() != 0) {
      //printf("renewing %d %d\n",loc.i, loc.j); fflush(stdout);
      xx = region_locs[loc];
      for (i = 0; i < (int) xx.size(); i++) {
        stackloc = xx.top();
        if (!visited[stackloc]) sum += areas.cellarea(stackloc);
        xx.pop();
      }
    }
    areas[loc] = sum;
  }
  }

  return;

}
