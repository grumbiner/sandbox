#include "ncepgrids.h"

void score(northgrid<float> &pred, northgrid<float> &obsd, northgrid<float> &persist) ;

int main(int argc, char *argv[]) {
  northgrid<float> a, b, mask, icein, climo, iceout, obsd;
  global_ice<float> sst;
  FILE *fin, *fout;
  latpt ll;
  ijpt tloc;
  ijpt loc;
  float r, ta, tb, vx, vy;
  int ti, tj, nday;

  a.set((float) 0.0);
  b.set((float) 0.0);
  mask.set((float) -1.0);

  fin = fopen(argv[1],"r");
  if (fin == (FILE*) NULL) return -1;
  icein.binin(fin);
  fclose(fin);

  fin = fopen(argv[2],"r");
  if (fin == (FILE*) NULL) return -1;
  climo.binin(fin);
  fclose(fin);

  fin = fopen(argv[3],"r");
  if (fin == (FILE*) NULL) return -1;
  sst.binin(fin);
  fclose(fin);

  fin = fopen(argv[4],"r");
  if (fin == (FILE*) NULL) return -1;
  obsd.binin(fin);
  fclose(fin);


  fin = fopen(argv[5],"r");
  if (fin == (FILE*) NULL) return -1;
  while ( !feof(fin) ) {
    fscanf(fin,"%f %f %f %f %f %d %d %d\n",&r, &ta, &tb, &vx, &vy, &ti, &tj, &nday);
    //printf("%f %f  %d\n",ta, tb, nday);
    loc.i = ti;
    loc.j = tj;
    mask[loc] = nday;
    if (r < 0.) {
      a[loc] = -ta;
      b[loc] = -tb;
    }
    else {
      a[loc] = ta;
      b[loc] = tb;
    }
  }
  fclose(fin);

  for (loc.j = 0; loc.j < a.ypoints(); loc.j++ ) {
  for (loc.i = 0; loc.i < a.xpoints(); loc.i++ ) {
    
    if (icein[loc] > 1.0 && icein[loc] < 1.3) icein[loc] = 1.0; 
    if (icein[loc] > 1.2 && icein[loc] < 2.) icein[loc] = 2.24;
    if (fabs(a[loc]) > 100 || fabs(b[loc]) > 100) {
      a[loc] = 0.0;
      b[loc] = 0.0;
      icein[loc] = 2.24;
    }

// Now, filter out ice points over warm water
    ll = a.locate(loc);
    tloc = sst.locate(ll);
    if (sst[tloc] > 275.15) icein[loc] = 2.24;

// Compute delta w.r.t. climo
    if (icein[loc] < 2.24 && climo[loc] < 2.24) {
      icein[loc] -= climo[loc];
    }
    else {
      icein[loc] = 2.24;
    }

// Apply a, b to make prediction
   if (icein[loc] <= 1.0 && mask[loc] > 0) {
     iceout[loc] = a[loc]*icein[loc] + b[loc];
   }
   else {
     iceout[loc] = 2.24;
   }


  }
  } 


// write out
  fout = fopen(argv[6],"w");
  iceout.binout(fout);
  fclose(fout);

// Now assess the skill -- filter with sst of observed day 
  fin = fopen(argv[7],"r");
  if (fin == (FILE*) NULL) return -1;
  sst.binin(fin);
  fclose(fin);

  for (loc.j = 0; loc.j < a.ypoints(); loc.j++ ) {
  for (loc.i = 0; loc.i < a.xpoints(); loc.i++ ) {
    if (obsd[loc] > 1.0 && obsd[loc] < 1.3) obsd[loc] = 1.0;
    if (obsd[loc] > 1.2 && obsd[loc] < 2.) obsd[loc] = 2.24;

// Now, filter out ice points over warm water
    ll = a.locate(loc);
    tloc = sst.locate(ll);
    if (sst[tloc] > 275.15) obsd[loc] = 2.24;

// Compute delta w.r.t. climo
    if (obsd[loc] < 2.24 && climo[loc] < 2.24) {
      //obsd[loc] = climo[loc] - obsd[loc] ;
      obsd[loc] -= climo[loc] ;
    }
    else {
      obsd[loc] = 2.24;
    }
  }
  }

  score(iceout, obsd, icein);

  return 0;
}


void score(northgrid<float> &pred, northgrid<float> &obsd, northgrid<float> &persist) {
  double sx = 0., sy = 0., sxy = 0., sx2 = 0., sy2 = 0.;
  int count, nx = pred.xpoints()*pred.ypoints();
  int i;
  float r, a, b;
  float pr, pa, pb;
  double psx = 0, psy = 0, psxy = 0, psx2 = 0, psy2 = 0;

  count = 0;
  for (i = 0; i < nx; i++) {
    if (pred[i] <= 1.0 && obsd[i] <= 1.0) {
      sx  += pred[i];
      sy  += obsd[i];
      sxy += pred[i]*obsd[i];
      sx2 += pred[i]*pred[i];
      sy2 += obsd[i]*obsd[i];

      psx  += persist[i];
      psy  += obsd[i];
      psxy += persist[i]*obsd[i];
      psx2 += persist[i]*persist[i];
      psy2 += obsd[i]*obsd[i];

      count += 1;
    }
  }
  if (count > 1 && ((double)count*sx2 - sx*sx) > 0 && ((double)count*sy2 - sy*sy) > 0 ) {
    a = ((double)count*sxy - sx*sy)/((double)count*sx2 - sx*sx);
    b = (sy - a*sx)/(double)count;
    r = ((double)count*sxy - sx*sy)/sqrt((double)count*sx2 - sx*sx)/sqrt((double)count*sy2 - sy*sy);
    pa = ((double)count*psxy - psx*psy)/((double)count*psx2 - psx*psx);
    pb = (psy - pa*psx)/(double)count;
    pr = ((double)count*psxy - psx*psy)/sqrt((double)count*psx2 - psx*psx)/sqrt((double)count*psy2 - psy*psy);
    printf("%f %f %f  %f %f  %4d  %f %f %f\n",r, a, b,
         sqrt((double)count*sx2 - sx*sx), sqrt((double)count*sy2 - sy*sy), count, pr, pa, pb);
  }
  else {
    printf("unscoreable! count = %d\n", count);
    return ;
  }


  return ;
}
