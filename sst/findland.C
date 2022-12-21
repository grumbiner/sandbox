#include "points.h"
#include "ncepgrids.h"

#define NPER 6

// Assemble the estimate for current sst given the reference fields
//   and the time
// Input is the name of the reference fields file, then YYYY MM DD,
//   and finally, the name of the file that you want the output in. 

template <class T>
void  read_reference(FILE *fin, global_quarter<T> &slope, global_quarter<T> &intercept,
                     mvector<global_quarter<T> > &ampl, mvector<global_quarter<T> > &phase) ;

////////////////////////////////////////////////////////////////////////
int main(int argc, char *argv[]) {
// From first pass
  global_quarter<float> slope, intercept;
  mvector<global_quarter<float> > ampl(NPER), phase(NPER);

// utility:
  FILE *fin, *fout;
  int i, j, nharm = NPER;
  ijpt loc;
  float time;
  global_quarter<float> expect;
  global_quarter<float> mask;
  global_quarter<unsigned char> umask;
  global_12th<float> expect_high;
  float landval = 157;

///////////////////////////////////////////////////////////
// Start working
  fin = fopen(argv[1],"r");
  read_reference(fin, slope, intercept, ampl, phase);
  fclose(fin);

  // construct a mask file:
  int count = 0;
  for (i = 0; i < mask.xpoints()*mask.ypoints(); i++ ) {
    if (ampl[0][i] == 0) {
      mask[i] = landval;
      count++;
    }
    else {
      mask[i] = 0;
    }
  }
  printf("land count = %d\n",count); fflush(stdout);
  fout = fopen(argv[2], "w");
  mask.binout(fout);
  fclose(fout);

  return 0;
}
template <class T>
void  read_reference(FILE *fin, global_quarter<T> &slope, global_quarter<T> &intercept,
                  mvector<global_quarter<T> > &ampl, mvector<global_quarter<T> > &phase) {
  intercept.binin(fin);
  slope.binin(fin);

  for (int i = 0; i < NPER; i++) {
    ampl[i].binin(fin);
    phase[i].binin(fin);
  }

  return;
}

