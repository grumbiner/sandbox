/* Routine to compute the square of the laplacean in ice concentration */
/* Should be useful for detecting points which merit further consideration
   for analysis/clean-up */
/* Bob Grumbine 7 July 1995. */
/* Version 0: Don't do anything special for land, coast, missing data, ... */
/*          The initial presumption is that these are interesting areas too. */

#include <math.h>

void icelapl(const unsigned char *map, int *lapl, 
                 const int nx, const int ny )
{
  int i, j, index, indexip, indexjp, indexim, indexjm;

  for (j = 0; j < ny ; j++) {
    for (i = 0; i < nx; i++) {
       index = i + j*nx;
       lapl[index] = 0;
    }
  }

  for (j = 1; j < ny-1 ; j++) {
    for (i = 1; i < nx-1 ; i++) {
       index = i + j*nx;
       indexip = index + 1;
       indexjp = index + nx;
       indexim = index - 1;
       indexjm = index - nx;

       lapl[index] = map[indexip] + map[indexim] \
                   + map[indexjp] + map[indexjm] - 4*map[index];
   
    }
  }

  return ;
}       
