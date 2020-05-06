#include <stdio.h>
#include <math.h>
#include "inc8.h"

float scale[NFIELD] = {64.0,  64.0,  64.0,  64.0, 64.0,
                       64.0,  64.0,   1.0,   1.0,  1.0,
                      100.0, 128.0, 128.0, 128.0,  1.0,
                       10.0,  10.0,  10.0,   1.0}  ;

typedef struct {unsigned int v19    : 16;
                unsigned int h19    : 16;
                unsigned int v37    : 16;
                unsigned int nasa   : 8;
                unsigned int nesdis : 8;
                }                         ssmi;

void nasa_team(float *v19, float *h19, float *v37, float *nasa, float *c, 
               int nx, int ny);
void usetie ( float *c, int ihem);
void conv(unsigned char *map, float *nasa, int nx, int ny, 
		const int add, const int mult);

/* Compute ice concentrations from mastermap brightness temperatures */
/* Bob Grumbine 9 February 1995 */

int main(int argc, char *argv[])
{
  float v19[GRID][GRID], h19[GRID][GRID], v37[GRID][GRID];
  float nasa[GRID][GRID];
  float nesdis[GRID][GRID];

  ssmi map[GRID][GRID];
  float TIE_POINTS[12];
  unsigned char outmap[GRID][GRID];

  FILE *in, *mout, *out;
  size_t bytes_read;
  int i, j;

  if (argc < 4) { printf ("need names for input and output files\n");
                  return -1;
                };

  in   = fopen(argv[1], "r");
  mout = fopen(argv[2], "w");
  out  = fopen(argv[3], "w");

  bytes_read = fread(v19, sizeof(float), GRID*GRID, in);
  if (bytes_read != GRID*GRID) return -2;

  bytes_read = fread(h19, sizeof(float), GRID*GRID, in);
  if (bytes_read != GRID*GRID) return -2;

  bytes_read = fread(v37, sizeof(float), GRID*GRID, in);
  if (bytes_read != GRID*GRID) return -2;

  bytes_read = fread(nesdis, sizeof(float), GRID*GRID, in);
  if (bytes_read != GRID*GRID) return -2;

  usetie(TIE_POINTS, NORTH);

  nasa_team(&v19[0][0], &h19[0][0], &v37[0][0], &nasa[0][0], &TIE_POINTS[0], 
      GRID, GRID); 

  conv(&outmap[0][0], &nasa[0][0], GRID, GRID, 0, 1);
  bytes_read = fwrite(outmap, sizeof(unsigned char), GRID*GRID, mout);
  if (bytes_read != GRID*GRID) return -3;

  for (i = 0; i < GRID; i++)
  {  for(j = 0; j < GRID; j++)
     {
     map[i][j].v19    = v19[i][j] * scale[TB_19V];
     map[i][j].h19    = h19[i][j] * scale[TB_19H];
     map[i][j].v37    = v37[i][j] * scale[TB_37V];
     map[i][j].nasa   = nasa[i][j];
     map[i][j].nesdis = nesdis[i][j] ;
     }
  }
  printf("size of ssmi is %d ",sizeof(ssmi));
  bytes_read = fwrite(map, sizeof(ssmi), GRID*GRID, out);
  if (bytes_read != GRID*GRID) return -4;
  
  return 0;
}
