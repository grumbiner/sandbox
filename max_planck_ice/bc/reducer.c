#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[])
{
  unsigned char *omap;
  float rfld[512][512];
  short int ifld[512][512];

  char a, b;
  int nfld, nreduce;

  int i, j, x;
  FILE *nin, *nout;
 
/* Open data files */
  nin   = fopen(argv[1], "r");
  nout  = fopen(argv[2], "w");
  if (nin == NULL || nout == NULL ) {
    printf("failed to open one of the 2 files \n");
    return -1;
  }

  printf("What reduction ratio do you want?\n");
  scanf("%1d",&nreduce);
 
  printf("field type %1d, reduction %1d \n",nfld, nreduce);

  i = fread(ifld, sizeof(short int), (512)*(512), nin);
  if (i != (512)*(512)) {
    printf("Error reading in data, %d bytes read, \
                                         when %d are expected\n",
      i, (512)*(512) );
  }

  for ( j = 0; j < 512; j++) {
    for ( i = 0; i < 512; i++) {
       rfld[j][i] = (float) ifld[j][i];
    }
  }

  freduce(&rfld[0][0], 512, 512, nreduce, nout); 

  return 0;

}
