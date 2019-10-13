#include "inc8.h"

void nasa_team(float *v19, float *h19, float *v37, float *nasa, float *C, 
               int nx, int ny)
{ long int i;
  long int npts;
  int fy_ice, my_ice, tot_ice;
  float gr, pr, prgr;
  float anf, anm, dd;

  npts = nx * ny;

  for (i = 0; i < npts; i++)
  {
    if (v19[i] <= low[TB_19V] || h19[i] <=low[TB_19H] ||
        v37[i] <= low[TB_37V] ) {
      fy_ice = NO_VALUE;
      my_ice = NO_VALUE;
      tot_ice = NO_VALUE;
    }
     else {
      gr = (v37[i] - v19[i]) / (v37[i] + v19[i]);
      if (gr > WEATHER_PARAM) {
        fy_ice = NO_VALUE;
        my_ice = NO_VALUE;
        tot_ice = NO_VALUE;
       }
      else {
       pr = (v19[i] - h19[i])/ (v19[i]+h19[i]);
       prgr = pr * gr;
       anf  = C[0] + C[1]*pr + C[2]*gr  + C[3]*prgr;
       anm  = C[8] + C[9]*pr + C[10]*gr + C[11]*prgr;
       dd   = C[4] + C[5]*pr + C[6] *gr + C[7]*prgr;
       fy_ice = (int) (100.0 * anf/dd + 0.5);
       my_ice = (int) (100.0 * anm/dd + 0.5);
       tot_ice = fy_ice + my_ice;
       
       if ( fy_ice < -20 || my_ice < -20 || tot_ice < -20 ||
            fy_ice > 120 || my_ice > 120 || tot_ice > 120   ) {
        fy_ice  = NO_VALUE;
        my_ice  = NO_VALUE;
        tot_ice = NO_VALUE;
       }

       if (tot_ice != NO_VALUE) {
         if (tot_ice < 0) {
           nasa[i] = 0.;
          }
          else if (tot_ice <= 100) {
           nasa[i] = (float) tot_ice;
          }
          else {
           nasa[i] = 100.;
          }
        }
       else { nasa[i] = (float) NO_VALUE; }


      }
    }
  }    /* Done looping over points */

  return;
}
