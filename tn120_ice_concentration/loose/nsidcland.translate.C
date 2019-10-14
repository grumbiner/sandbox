#include <stdio.h>

#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin1, *fout;
  nsidcnorth<short int> oldland;
  nsidcnorth<unsigned char> param;
  ijpt loc;
  int count = 0;

  fin1 = fopen(argv[1], "r");
  fout = fopen(argv[2], "w");
  if (fin1 == (FILE*)NULL) {
    printf("Failed to open %s\n",argv[1]);
    return 1;
  }
  if (fout == (FILE*)NULL) {
    printf("Failed to open %s\n",argv[3]);
    return 1;
  }

  oldland.binin(fin1);
  fclose(fin1);

  printf("oldland stats %d %d  %d %d\n", 
       oldland.gridmax(), oldland.gridmin(), oldland.average(), oldland.rms() );

  for (loc.j = 0; loc.j < param.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < param.xpoints(); loc.i++) {
    if (oldland[loc] == 2) {
      param[loc] = (unsigned char) 157; 
    }
    else if (oldland[loc] == 1) {
      param[loc] = (unsigned char) 195;
    }
    else if (oldland[loc] == 0) {
      param[loc] = (unsigned char) 0;
    }
    else {
      printf("Impossible land %d %d  %d\n",loc.i, loc.j, oldland[loc]);
    }
  }
  } 


  param.binout(fout);
  fclose(fout);

  return 0;

}
