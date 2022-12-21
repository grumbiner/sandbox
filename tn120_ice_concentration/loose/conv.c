#include <stdio.h>
#include <math.h>

/* Convert floating point input to single byte output*/
/* Permit linear rescaling */
/* Bob Grumbine 6 June 1994 */
void conv(char *map, float *nasa, const int nx, const int ny, 
          const float plus, const float mult)
{  long int i, npts;

   npts = nx*ny;
   for (i = 0; i < npts; i++)
   { 
     map[i] = (char) (mult*(nasa[i] +  plus));
/*     printf ("map, char, mult, plus, %6.2f %c %f %f\n",nasa[i], map[i], 
               mult, plus); */
   }
  return;
}  
