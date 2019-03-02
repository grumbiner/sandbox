#include <math.h>
#include <stdlib.h>

extern int nlong;

void interp(const int ilat, const int jlon, const int nlong, const int nmatch, 
            const int *matches, char *grid)
{
  float x0, y0, x1, y1, yj;
  float dx, dydx;
  int    c, loc, *delta;

  delta = malloc (sizeof(int)*nlong);
  if (delta == NULL) {
    exit (-1);
  }

  yj = -1;
  switch (nmatch) {
    case 0:
    case 1:
           printf("Error, too few points passed to interp\n");
           free(delta);
           return;
    case 2:
           y0 = *(grid+matches[0] + ilat*nlong);
           y1 = *(grid+matches[1] + ilat*nlong);
           x0 = matches[0] - jlon;
           x1 = matches[1] - jlon;
           if (x0 < -nlong/2) x0 = x0 + nlong;
           if (x1 < -nlong/2) x1 = x1 + nlong;
           dx = 0 - x0;
           dydx = (y1-y0) / (x0-x1);
           yj   = y0+dx*dydx;
           break;
    default :
           c = 0;
           delta[c] = matches[c] - jlon;
           if (delta[c] > nlong/2) { delta[c] -= nlong;}
           if (delta[c] < -nlong/2) {delta[c] += nlong;}
           
           for (c = 1; c < nmatch; c++)
           {
             delta[c] = matches[c] - jlon;
             if (delta[c] > nlong/2) { delta[c] -= nlong;}
             if (delta[c] < -nlong/2) {delta[c] += nlong;}
           }

           loc = -1;
           for (c = 0; c < nmatch -1; c++)
           {  if (delta[c] < 0 && delta[c+1] > 0) { 
                loc = c;
              }
           }

           if (loc == -1) {
/*             printf("Failed to find the sign switch\n"); */
             if ( fabs(delta[0]) < fabs(delta[nmatch-1]) ) {
               loc = 0;
             }
             else {
               loc = nmatch - 1 ;
             }
             yj = *(grid + matches[loc]+ilat*nlong);
           }
           if (yj != -1) break;

           y0 = *(grid+matches[loc] + ilat*nlong);
           y1 = *(grid+matches[loc+1] + ilat*nlong);
           x0 = matches[loc] - jlon;
           x1 = matches[loc+1] - jlon;
           if (x0 < -nlong/2) x0 = x0 + nlong;
           if (x1 < -nlong/2) x1 = x1 + nlong;
           dx = 0 - x0;
           dydx = (y1-y0) / (x0-x1);
           yj   = y0+dx*dydx;
           break;

    }

    *(grid+jlon+ilat*nlong) = yj;

    free(delta);
    return;
}                
