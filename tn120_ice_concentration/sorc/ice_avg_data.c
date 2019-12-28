#include "ssmi.h"
#include "icessmi.h"
#include "icegrids.h"

int ice_avg_data(ssmi_tmp *north_tmp, ssmi_tmp *south_tmp,
                 int *count_n, int *count_s,
                 ssmi *north, ssmi *south, int *north_pts, int *south_pts)
{ 
  int i, nlim, slim;

  nlim = (NX_NORTH)*(NY_NORTH);
  slim = (NX_SOUTH)*(NY_SOUTH);
  *north_pts = nlim;
  *south_pts = slim;
  
  for (i = 0; i < nlim ; i++)
  {
    if (count_n[i] != 0) {
      north[i].t19v = north_tmp[i].t19v / count_n[i];
      north[i].t19h = north_tmp[i].t19h / count_n[i];
      north[i].t22v = north_tmp[i].t22v / count_n[i];
      north[i].t37v = north_tmp[i].t37v / count_n[i];
      north[i].t37h = north_tmp[i].t37h / count_n[i];
      north[i].t85v = north_tmp[i].t85v / count_n[i];
      north[i].t85h = north_tmp[i].t85h / count_n[i];
      north[i].conc_bar = north_tmp[i].conc_bar / count_n[i];
      north[i].bar_conc = 0.5 + 100.*nasa_team(
        (float)north[i].t19v /100.,
        (float)north[i].t19h /100.,
        (float)north[i].t22v /100.,
        (float)north[i].t37v /100.,
        (float)north[i].t37h /100.,
        (float)north[i].t85v /100.,
        (float)north[i].t85h /100., 'n'
      ); 
/*      printf("averaged data, %4d, %5d %5d %5d %5d %3d %3d %5d\n",
        i, north[i].t19v, north[i].t19h, north[i].t22v, north[i].t37v,
        north[i].conc_bar, north[i].bar_conc, 
        north[i].conc_bar - north[i].bar_conc);
*/
      
    }
  }

  for (i = 0; i < slim ; i++)
  {
    if (count_s[i] != 0) {
      south[i].t19v = south_tmp[i].t19v / count_s[i];
      south[i].t19h = south_tmp[i].t19h / count_s[i];
      south[i].t22v = south_tmp[i].t22v / count_s[i];
      south[i].t37v = south_tmp[i].t37v / count_s[i];
      south[i].t37h = south_tmp[i].t37h / count_s[i];
      south[i].t85v = south_tmp[i].t85v / count_s[i];
      south[i].t85h = south_tmp[i].t85h / count_s[i];
      south[i].conc_bar = south_tmp[i].conc_bar / count_s[i];
      south[i].bar_conc = 0.5 + 100.*nasa_team(
        (float)south[i].t19v /100.,
        (float)south[i].t19h /100.,
        (float)south[i].t22v /100.,
        (float)south[i].t37v /100.,
        (float)south[i].t37h /100.,
        (float)south[i].t85v /100.,
        (float)south[i].t85h /100., 's'
       ); 
    }
  }

  return 0;

}
