#include "ncepgrids.h"

// General program for recoding from the reconciler format to the
//   assorted formats and conventions used in ice/sst analysis, etc.
// Robert Grumbine 22 April 2009
void master(metricgrid<unsigned char> &in, metricgrid<unsigned char> &out, 
            FILE *fout) ; 
void seaice(metricgrid<unsigned char> &in, metricgrid<unsigned char> &out, 
            FILE *fout) ; 
void sst(metricgrid<unsigned char> &in, metricgrid<unsigned char> &out, 
            FILE *fout) ; 

int main(int argc, char *argv[]) {
  GRIDTYPE<unsigned char> in, out;
  FILE *fin, *fout;

  fin = fopen(argv[1], "r");
  in.binin(fin);
  fclose(fin);

  fout = fopen(argv[2], "w");
  master(in, out, fout);

  fout = fopen(argv[3], "w");
  seaice(in, out, fout);

  fout = fopen(argv[4], "w");
  sst(in, out, fout);

  return 0;
}


void master(metricgrid<unsigned char> &in, metricgrid<unsigned char> &out, 
            FILE *fout) { 
  const unsigned char coast_in = 1, land_in = 5, inland_in = 17, ocean_in = 15;

  unsigned char coast_out = 3, land_out = 2, inland_out = 1, ocean_out = 0;
  for (int i = 0; i < in.xpoints()*in.ypoints(); i++) {
    switch(in[i]) {
      case coast_in  : out[i] = coast_out ; break;
      case land_in   : out[i] = land_out  ; break;
      case inland_in : out[i] = inland_out; break;
      case ocean_in  : out[i] = ocean_out ; break;
      default :
        printf("invalid value %d at point %d\n",in[i], i);
    }
  }
  out.binout(fout);
  fclose(fout);

}
void seaice(metricgrid<unsigned char> &in, metricgrid<unsigned char> &out, 
            FILE *fout) { 
  const unsigned char coast_in = 1, land_in = 5, inland_in = 17, ocean_in = 15;

  unsigned char coast_out = 195, land_out = 157, inland_out = 0, ocean_out = 0;
  for (int i = 0; i < in.xpoints()*in.ypoints(); i++) {
    switch(in[i]) {
      case coast_in  : out[i] = coast_out ; break;
      case land_in   : out[i] = land_out  ; break;
      case inland_in : out[i] = inland_out; break;
      case ocean_in  : out[i] = ocean_out ; break;
      default :
        printf("invalid value %d at point %d\n",in[i], i);
    }
  }
  out.binout(fout);
  fclose(fout);

}
void sst(metricgrid<unsigned char> &in, metricgrid<unsigned char> &out, 
            FILE *fout) { 

  const unsigned char coast_in = 1, land_in = 5, inland_in = 17, ocean_in = 15;
  unsigned char coast_out = 3, land_out = 3, inland_out = 0, ocean_out = 0;
  for (int i = 0; i < in.xpoints()*in.ypoints(); i++) {
    switch(in[i]) {
      case coast_in  : out[i] = coast_out ; break;
      case land_in   : out[i] = land_out  ; break;
      case inland_in : out[i] = inland_out; break;
      case ocean_in  : out[i] = ocean_out ; break;
      default :
        printf("invalid value %d at point %d\n",in[i], i);
    }
  }

  ijpt loc;
  for (loc.j = out.ypoints()-1; loc.j >= 0; loc.j--) {
  for (int i = 0; i < in.xpoints()/80; i++) {
    for (loc.i = i*80; loc.i < i*80+80; loc.i++) {
      fprintf(fout,"%1d", out[loc]);
    }
    fprintf(fout,"\n");
  }
  }
  fclose(fout);

}
