#include <stdio.h>
/* #include <malloc.h> */
#include "icessmi.h"

/* Check all points 0-reduce away in positive i, j directions to see
   if any of them are the same type/value as the argument 'type' */
/* Useful for checking the presence of land, coast, ... */
/* Bob Grumbine 1 March 1995 */

int any(unsigned char *x, const int i, const int j, const int nx, 
                  const int reduce, const int type)
{
  int index, l, m, truth;
  
  truth = (1 == 0);
  for (m = 0; m < reduce; m++)
  {  for (l = 0; l < reduce; l++)
     {  index = reduce*i+l + reduce*j*nx + m*nx;
        truth = truth || ( x[index] == type ) ;
     }
  }

  return truth;

}
