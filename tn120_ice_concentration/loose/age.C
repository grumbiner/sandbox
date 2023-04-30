#include <stdio.h>

#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin;
  global_ice<unsigned char> ice;

  fin = fopen(argv[1], "r");
  if (fin == (FILE *) NULL) {
    printf("failed to open %s\n",argv[1]);
    return 1;
  }

  ice.binin(fin);
  fclose(fin);

  if (ice.gridmax() > 8) {
    printf("%s gridmax = %d\n",argv[1], ice.gridmax());
  }

  return 0;
}
