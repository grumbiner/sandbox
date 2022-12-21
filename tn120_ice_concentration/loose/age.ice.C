#include <stdio.h>

#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin, *fout;
  global_ice<unsigned char> glob;
  palette<unsigned char> gg(19, 65);
  
  fin = fopen(argv[1],"r");
  glob.binin(fin);
  printf("max min ages %d %d\n",glob.gridmax(), glob.gridmin() );
  glob *= 10;
  glob.xpm("age.xpm", 1, gg);

  return 0;
}
  

  
