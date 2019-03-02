#include <math.h>
#include <stdlib.h>

void interval(char *grid, const float rearth, const float nom_res,
  const int lat_min, const int nlat, const int nlong)
{
  int i, j;
  float latitude, dx, dprd;
  int ndx, k, test, *matches, nmatch;

  matches = malloc(sizeof(float)*nlong );
  if (matches == NULL) {
    exit (-1);
  }

  dprd = atan(1.)*4./180.;
  for (i = 0; i < nlat; i++)
  {  latitude = 0.25*(i+lat_min);
     dx = rearth*cos(latitude*dprd)*2*atan(1.)*4. / nlong *sqrt(2.);
     if (dx > 0.5) {
       ndx = (int) (nom_res / dx);
     }
     else {
       ndx = nlong/4;
     }
     printf("i, latitude, dx, ndx, %4d %f %f %d \n",i, latitude, dx, ndx);

     for (j = 0; j < nlong; j++)
     {
       if ( *(grid+j+i*nlong) == 0) {
         nmatch = 0;
         for (k = -ndx; k <= ndx; k++)
         {
           if (j+k < 0) {test = nlong+k+j;}
             else       {test = k + j;    }
           if (test > nlong) {test = test - nlong; }
      
           if ( *(grid+test+i*nlong) != 0) {
             nmatch += 1;
             matches[nmatch-1] = test;
           }
         }

         switch (nmatch) {
           case 0 : break;
           case 1 : 
                    *(grid+j+i*nlong) = *(grid+test+i*nlong);
                    break;
           case 2 :
                    interp(i, j, nlong, nmatch, matches, grid);
                    break;
           default :
                    interp(i, j, nlong, nmatch, matches, grid);
                    if (nmatch > 2*ndx) printf("Too many\n");
         }  /* End of the switch */
       } /* End of the if grid == 0 */
     } /* End of the j loop */
   }  /* End of the i loop */

  free (matches);

  return;
}
