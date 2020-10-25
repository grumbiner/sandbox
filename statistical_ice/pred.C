#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  northgrid<float> a, b, mask, icein, climo, iceout;
  global_ice<float> sst;
  FILE *fin, *fout;
  latpt ll;
  ijpt tloc;
  ijpt loc;
  palette<unsigned char> gg(19,65);

  a.set((float) 0.0);
  b.set((float) 0.0);
  mask.set((float) -1.0);

  fin = fopen(argv[1],"r");
  icein.binin(fin);
  fclose(fin);

  fin = fopen(argv[2],"r");
  climo.binin(fin);
  fclose(fin);

  fin = fopen(argv[3],"r");
  sst.binin(fin);
  fclose(fin);

  fin = fopen(argv[4],"r");
  while ( !feof(fin) ) {
    float r, ta, tb, vx, vy;
    int ti, tj, nday;
    fscanf(fin,"%f %f %f %f %f %d %d %d\n",&r, &ta, &tb, &vx, &vy, &ti, &tj, &nday);
    //printf("%f %f  %d\n",ta, tb, nday);
    loc.i = ti;
    loc.j = tj;
    mask[loc] = nday;
    a[loc] = ta;
    b[loc] = tb;
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
     printf("%f %f %f  %f %f\n",a[loc], b[loc], icein[loc], climo[loc], iceout[loc]);
   }
   else {
     iceout[loc] = 2.24;
   }

   if (a[loc] > 1.0 || b[loc] > 1.0) {
     printf("%3d %3d  %f %f\n",loc.i, loc.j, a[loc], b[loc]);
   }


  }
  } 


// write out
  fout = fopen(argv[5],"w");
  iceout.binout(fout);
  fclose(fout);
  printf("max, min, average %f %f %f\n",icein.gridmax(), icein.gridmin(), icein.average(2.24) );
  printf("max, min, average %f %f %f\n",iceout.gridmax(), iceout.gridmin(), iceout.average(2.24) );
  printf("max, min, average %f %f %f\n",a.gridmax(), a.gridmin(), a.average() );
  printf("max, min, average %f %f %f\n",b.gridmax(), b.gridmin(), b.average() );

  iceout.scale();
  iceout.xpm("p.xpm",7,gg);

  return 0;
}
