#include "globs.h"

// auxiliary program to transfer from global_15th to hycom_15th

int main(int argc, char *argv[]) {
  FILE *fin, *fout;
  hycom_15th<float> outbathy;
  hycom_15th<unsigned char> outmask;
  global_15th<float> inbathy;
  global_15th<unsigned char> inmask;
  fijpt pt1;
  latpt ll1;
  ijpt loc, inloc;
  palette<unsigned char> gg(19,65);

  printf("Successfully opened the program\n"); fflush(stdout);
  fin = fopen(argv[1], "r");
  if (fin == (FILE *) NULL) {
    printf("Failed to open the input file %s\n",argv[1]);
    return 1;
  }
  inbathy.binin(fin);
  inmask.binin(fin);
  fclose(fin);
  printf("Successfully read the data\n"); fflush(stdout);

  for (loc.j = 0; loc.j < outbathy.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < outbathy.xpoints(); loc.i++) {
	  ll1 = outbathy.locate(loc);
	  pt1 = inbathy.locate(ll1);
	  inloc = pt1;
	  outbathy[loc] = inbathy[inloc];
	  outmask[loc] = inmask[inloc];
  }
  }

  fout = fopen(argv[2], "w");
  outbathy.binout(fout);
  outmask.binout(fout);
  fclose(fout);

  outmask.xpm("mask.xpm",1, gg);
  outbathy.scale();
  outbathy.xpm("bathy.xpm",8, gg);

  return 0;

}
