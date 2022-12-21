#include "ncepgrids.h"

void update_histogram(llgrid<mvector<short int> > &count, llgrid<float> &x, float tmin, float tmax, float prec) ;

template <class T>
float shannon(mvector<T> &x) ;

int main(int argc, char *argv[]) {
  FILE *fin;
  global_quarter<float> x, old;
  global_quarter<mvector<short int> > count;
  int i;
  float tmin = -7.5, tmax = 7.5;
  float prec =  0.25;
  int nc;

  nc = 1 + (short int) nearbyint( (tmax - tmin)/prec);
  for (i = 0; i < count.xpoints()*count.ypoints() ; i++) {
    count[i].resize(nc);
    count[i] = 0;
  }

  fin = fopen(argv[1],"r");
  old.binin(fin);
  fclose(fin);
  old /= 100.0; // rescale out of centidegrees
  for (i = 2; i < argc; i++) {
    fin = fopen(argv[i],"r");
    x.binin(fin);
    fclose(fin);

    x /= 100.0; // rescale out of centidegrees
    old -= x;
    printf("max %f min %f\n",old.gridmax(), old.gridmin() ); fflush(stdout);
    update_histogram(count, old, tmin, tmax, prec);
    old = x;
  }

  ijpt loc;
  latpt ll;
  float si;
  x.set((float) 0.0);
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    si = shannon(count[loc]); 
    if (si != 0) {
      x[loc] = si;
    }
  }
  }
  FILE *fout;
  char fname[900];
  sprintf(fname,"delta_%04.2f",prec);
  fout = fopen(fname,"w");
  x.binout(fout);
  fclose(fout);

  return 0;
}
void update_histogram(llgrid<mvector<short int> > &count, llgrid<float> &x, float tmin, float tmax, float prec) {
  int i;
  for (i = 0; i < x.xpoints()*x.ypoints(); i++) {
    count[i][ (short int) nearbyint( (x[i]-tmin)/prec) ] += 1;  
  }
  return;
}
template <class T>
float shannon(mvector<T> &x) {
  int i, count = 0;
  float p, info = 0.0;
  for (i = 0; i < x.xpoints(); i++) {
    if (x[i] != 0) { 
      count += x[i];
    }
  }
  for (i = 0; i < x.xpoints(); i++) {
    if (x[i] != 0) { 
      p = (float) x[i] / (float) count;
      info  -= p    * log10(p);
      // note -= because SI is -sum(p*logp)
     }
  }
  return info / log10(2.);
}
