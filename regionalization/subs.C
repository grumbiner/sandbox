#include "ncepgrids.h"
#include "time_series.h"
#include <errno.h>
#include <stack>
using namespace std;
extern int bcount;

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

