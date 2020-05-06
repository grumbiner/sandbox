#include <stdio.h>
#include <string.h>
#include "hdf.h"
#include "herr.h"
#include "icessmi.h"

float nasa_team(const float t1, const float t2, const float t3, 
                const float t4, const float t5, const float t6, 
                const float t7, const char x, const int ant, const int regress);

int hdfget(char *fname, float *science, int32 *dims);

int main(int argc, char *argv[])
{
  int32 dims[10];
  intn rank;

  int i, j;
  float t19v[332][316], t19h[332][316], t22v[332][316];
  float t37h[332][316], t37v[332][316], conc[332][316];
  char fname[320];
  unsigned char map[332][316];
  float t1, t2, t3, t4, t5, t6, t7;
  FILE *outfile;

  sprintf(fname,"%s.19v",argv[1]);
  hdfget(fname, &t19v[0][0], &dims[0]);

  sprintf(fname,"%s.19h",argv[1]);
  hdfget(fname, &t19h[0][0], &dims[0]);

  sprintf(fname,"%s.22v",argv[1]);
  hdfget(fname, &t22v[0][0], &dims[0]);

  sprintf(fname,"%s.37v",argv[1]);
  hdfget(fname, &t37v[0][0], &dims[0]);

  sprintf(fname,"%s.37h",argv[1]);
  hdfget(fname, &t37h[0][0], &dims[0]);

  t6 = 0.0;
  t7 = 0.0;

  for (i = 0; i < 332 ; i++) {
    for (j = 0; j < 316 ; j++) {
       t1 = t19v[i][j];
       t2 = t19h[i][j];
       t3 = t22v[i][j];
       t4 = t37v[i][j];
       t5 = t37h[i][j];
       if (t1 <= 50.0) t1 = 0.0;
       if (t2 <= 50.0) t2 = 0.0;
       if (t3 <= 50.0) t3 = 0.0;
       if (t4 <= 50.0) t4 = 0.0;
       if (t5 <= 50.0) t5 = 0.0;
  
       conc[i][j] = nasa_team(t1, t2, t3, t4, t5, t6, t7, 's', 0, 1);
       if (conc[i][j] > 100. && conc[i][j] < 128.) conc[i][j] = 100.;
       if (conc[i][j] < 0.) conc[i][j] = 0.;

       map[i][j] = (unsigned char) ((int) (conc[i][j] + 0.5) ) ;

    }
  }

  strcpy(fname, "conc");
  outfile = fopen(fname,"w");
  fwrite(map, sizeof(unsigned char), 332*316, outfile);

  return 0;

}

int hdfget(char *fname, float *science, int32 *dims)
{
  int32 number_type, i, j, x;
  intn rank;
  uint16 scien_data[332][316];
  int index;

  DFSDgetdims(fname, &rank, dims, 10);
/*  printf("rank = %d\n",rank);
  for (i = 0; i < rank ; i++) {
    printf("%d %d\n",i, dims[i]);
  }
*/
  if (rank != 2) {
    printf("Error in rank, this is supposed to be a 2d array, %d were found\n",
              rank);
    return -1;
  }

  DFSDgetNT(&number_type);
/*  printf("number type = %d \n",number_type); */

  i = DFSDgetdata(fname, rank, dims, scien_data );
  printf("return from get data was %d\n", i);
  if (i != 0) { return -1; }

  for (i = 0; i < dims[0] ; i++) {
    for (j = 0 ; j < dims[1] ; j++) {
      index = j + i*dims[1];
      science[index] = scien_data[i][j] / 10.0;
/*      printf("%3d %3d %7d\n",i, j, scien_data[i][j] ); */
    }
  }

  return 0;

}
