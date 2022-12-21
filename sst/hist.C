#include "ncepgrids.h"

#define NHIST 4273
#define NNEG  2106
#define NGT   2167
#define PREC     5

void globalhisto(llgrid<mvector<short int> > &x, llgrid<short int> &y, int base, int prec) ;

int main(int argc, char *argv[]) {
  global_quarter<mvector<short int> > x;
  global_quarter<short int> y;
  global_quarter<float> sout;
  int i;
  FILE *fin, *fout;
  ijpt loc;
  int base = NNEG, prec = PREC;
  int nhist = (NHIST/PREC + 1);

// Experimentation on being able to allocate memory for doing
// all at once computation of shannon info
//  printf("number of bins = %d\n",NHIST/PREC);
  for (i = 0; i < x.xpoints()*x.ypoints(); i++) {
//    if ( (i%1024) == 0) printf("i = %d\n",i); fflush(stdout);
    x[i].resize(nhist);
    x[i] = 0;
  }
  printf("precision %d centidegrees, number of bins = %d\n",PREC, nhist);
  fflush(stdout); 

// Read in data and accumulate the histogram
  for (i = 1; i < argc; i++) {
    if ( (i%365) == 0) printf("file %d\n",i); fflush(stdout);
    fin = fopen(argv[i],"r");
    y.binin(fin);
    fclose(fin);
    globalhisto(x, y, base, prec);
  }

  double p, shannon = 0.0;
  latpt ll;
  printf("nfiles = %d\n",argc-1);
  for (loc.j = 0; loc.j < y.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < y.xpoints(); loc.i++) {
    shannon = 0.0;
    for (i = 0; i < nhist; i++) {
      if (x[loc][i] != 0) {
        p = (double) x[loc][i] / (double) (argc-1);
        shannon -= p*log(p); 
      } 
    }
    if (shannon != 0) {
      shannon /= log(2.);
      ll = y.locate(loc);
      printf("%4d %4d  %7.3f %8.3f  %f\n",loc.i, loc.j, ll.lat, ll.lon, shannon);
    }
    sout[loc] = (float) shannon ;

  }
  }

  fout = fopen("shannon.out", "w");
  sout.binout(fout);
  fclose(fout);

  return 0;

}

void globalhisto(llgrid<mvector<short int> > &x, llgrid<short int> &y, int base, int prec) {
  int i, npts = x.xpoints() * x.ypoints();
  for (i = 0; i < npts;  i++) {
      x[i][(short int) rint((float) (y[i] + base)/(float)prec) ] += 1;
  }

  return;
}
