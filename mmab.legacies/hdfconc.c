#include <stdio.h>
#include <string.h>
#include "hdf.h"
#include "herr.h"
#include "icessmi.h"

int hdfget(char *fname, float *science, int32 *dims);

int main(int argc, char *argv[])
{
  int32 dims[10];
  intn rank;

  int i, j;
  float fconc[448][304];
  unsigned char map[448][304];
  char fname[320];
  float t1, t2, t3, t4, t5, t6, t7;
  FILE *outfile;


  sprintf(fname,"%s",argv[1]);
  hdfget(fname, &fconc[0][0], &dims[0]);
  for(i = 0; i < 10; i++) {
    printf("%d %d\n",i,dims[i]);
  }

  for (i = 0; i < 448 ; i++) {
    for (j = 0; j < 304 ; j++) {
       map[i][j] = (unsigned char) ((int) (fconc[i][j] + 0.5) ) ;
    }
  }

  outfile = fopen("conc","w");
  fwrite(map, sizeof(unsigned char), 448*304, outfile);

  return 0;

}

int hdfget(char *fname, float *science, int32 *dims)
{
  int32 number_type, i, j, x;
  intn rank;
  uint16 scien_data[448][304];
  int index;

  printf("Entered hdfget, fname = %s\n", fname); fflush(stdout);
  DFSDgetdims(fname, &rank, dims, 10);
  printf("rank = %d\n",rank);
  for (i = 0; i < rank ; i++) {
    printf("%d %d\n",i, dims[i]); fflush(stdout);
  }

  if (rank != 2) {
    printf("Error in rank, this is supposed to be a 2d array, %d were found\n",
              rank);
    return -1;
  }

  DFSDgetNT(&number_type);
  printf("number type = %d \n",number_type); 

  i = DFSDgetdata(fname, rank, dims, &scien_data[0][0] );
  printf("return from get data was %d\n", i);
  if (i != 0) { return -1; }

  for (i = 0; i < dims[0] ; i++) {
    for (j = 0 ; j < dims[1] ; j++) {
      index = j + i*dims[1];
      science[index] = scien_data[i][j] / 10.0;
      printf("%3d %3d %7d\n",i, j, scien_data[i][j] ); 
    }
  }

  return 0;

}
