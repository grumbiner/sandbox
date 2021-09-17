#include "icessmi.h"

/* Average (as needed) the brightness temperatures and ice concentrations
     on the sea ice grid. */
/* Compute the ice concentration with the averaged ice temperatures (a1 
     files) */
/* Robert Grumbine 10 February 1995 */

int ice_avg_data(ssmi_tmp *north_tmp, ssmi_tmp *south_tmp,
                 ssmi *north, ssmi *south, 
                 const int north_pts, const int south_pts)
{ 
  int i, nlim, slim;

  nlim = north_pts;
  slim = south_pts;
  
  for (i = 0; i < nlim ; i++)
  {
    if (north_tmp[i].count == 0) {
      north[i].conc_bar = north_tmp[i].conc_bar;
      north[i].bar_conc = north_tmp[i].conc_bar;
      north[i].t19v = 0;
      north[i].t19h = 0;
      north[i].t22v = 0;
      north[i].t37v = 0;
      north[i].t37h = 0;
      north[i].t85v = 0;
      north[i].t85h = 0;
      north[i].count = 0;
    }
    else {
      north[i].t19v = (int) (0.5 + 100.*(float) north_tmp[i].t19v / \
                                        (float) north_tmp[i].count);
      north[i].t19h = (int) (0.5 + 100.*(float) north_tmp[i].t19h / \
                                        (float) north_tmp[i].count);
      north[i].t22v = (int) (0.5 + 100.*(float) north_tmp[i].t22v / \
                                        (float) north_tmp[i].count);
      north[i].t37v = (int) (0.5 + 100.*(float) north_tmp[i].t37v / \
                                        (float) north_tmp[i].count);
      north[i].t37h = (int) (0.5 + 100.*(float) north_tmp[i].t37h / \
                                        (float) north_tmp[i].count);
      north[i].t85v = (int) (0.5 + 100.*(float) north_tmp[i].t85v / \
                                        (float) north_tmp[i].count);
      north[i].t85h = (int) (0.5 + 100.*(float) north_tmp[i].t85h / \
                                        (float) north_tmp[i].count);
      north[i].conc_bar = (int) (0.5 + (float) north_tmp[i].conc_bar / 
                                       (float) north_tmp[i].count);
      north[i].count    = north_tmp[i].count;
      north[i].bar_conc = 0.5 + nasa_team(
        (float)north[i].t19v / 100.,
        (float)north[i].t19h / 100.,
        (float)north[i].t22v / 100.,
        (float)north[i].t37v / 100.,
        (float)north[i].t37h / 100.,
        (float)north[i].t85v / 100.,
        (float)north[i].t85h / 100., 'n', 1, 1
      ); 
      #ifdef VERBOSE2
        printf("averaged data, %4d, %5d %5d %5d %5d %3d %3d %5d %3d\n",
        i, north[i].t19v, north[i].t19h, north[i].t22v, north[i].t37v,
        north[i].conc_bar, north[i].bar_conc, 
        north[i].conc_bar - north[i].bar_conc, north[i].count);
      #endif 
      
    }
  }

  for (i = 0; i < slim ; i++)
  {
    if (south_tmp[i].count == 0) {
      south[i].conc_bar = south_tmp[i].conc_bar;
      south[i].bar_conc = south_tmp[i].conc_bar;
      south[i].t19v = 0;
      south[i].t19h = 0;
      south[i].t22v = 0;
      south[i].t37v = 0;
      south[i].t37h = 0;
      south[i].t85v = 0;
      south[i].t85h = 0;
      south[i].count = 0;
    }
    else {
      south[i].t19v = (int) (0.5 + 100.*(float) south_tmp[i].t19v / 
                                        (float) south_tmp[i].count);
      south[i].t19h = (int) (0.5 + 100.*(float) south_tmp[i].t19h /
                                        (float)  south_tmp[i].count);
      south[i].t22v = (int) (0.5 + 100.*(float) south_tmp[i].t22v / 
                                        (float) south_tmp[i].count);
      south[i].t37v = (int) (0.5 + 100.*(float) south_tmp[i].t37v / 
                                        (float) south_tmp[i].count);
      south[i].t37h = (int) (0.5 + 100.*(float) south_tmp[i].t37h / 
                                        (float) south_tmp[i].count);
      south[i].t85v = (int) (0.5 + 100.*(float) south_tmp[i].t85v / 
                                        (float) south_tmp[i].count);
      south[i].t85h = (int) (0.5 + 100.*(float) south_tmp[i].t85h / 
                                        (float) south_tmp[i].count);
      south[i].conc_bar = (int) (0.5 + (float)south_tmp[i].conc_bar / 
                                       (float)south_tmp[i].count);
      south[i].count    = south_tmp[i].count;
      south[i].bar_conc = 0.5 + nasa_team(
        (float)south[i].t19v / 100. ,
        (float)south[i].t19h  / 100.,
        (float)south[i].t22v  / 100.,
        (float)south[i].t37v  / 100.,
        (float)south[i].t37h  / 100.,
        (float)south[i].t85v  / 100.,
        (float)south[i].t85h  / 100., 's', 1, 1
       ); 
    }
  }

  return 0;

}

/* Apply a land mask to the concentrations (but not the brightness 
     temperatures ) */
/* Robert Grumbine 28 March 1995 */

int ice_mask( ssmi *north, ssmi *south, 
              const int north_pts, const int south_pts, 
              unsigned char *nmap, unsigned char *smap                     )
{ 
  int i, nlim, slim;

  nlim = north_pts;
  slim = south_pts;
  
  for (i = 0; i < nlim ; i++)
  {
    if ( ((int) nmap[i]) == ((int) LAND) ) {
      north[i].conc_bar = LAND ;
      north[i].bar_conc = LAND ;
    }
    if ( ((int) nmap[i]) == ((int) COAST) ) {
      north[i].conc_bar = COAST ;
      north[i].bar_conc = COAST ;
    }
  }

  for (i = 0; i < slim ; i++)
  {
    if (smap[i] == LAND) {
      south[i].conc_bar = LAND ;
      south[i].bar_conc = LAND ;
    }
    if ( ((int) smap[i]) == ((int) COAST) ) {
      south[i].conc_bar = COAST ;
      south[i].bar_conc = COAST ;
    }
  }

  return 0;

}

/* Zero out the information in the temporary data files for ssmi 
      average computation */
/* Robert Grumbine 15 December 1994 */

int ice_zero(ssmi_tmp *north_tmp, ssmi_tmp *south_tmp, 
              const int north_pts, const int south_pts)
{
  int i, j;
  ssmi_tmp blank;

  blank.t19v = 0;
  blank.t19h = 0;
  blank.t22v = 0;
  blank.t37v = 0;
  blank.t37h = 0;
  blank.t85v = 0;
  blank.t85h = 0;
  blank.conc_bar = NO_DATA;
  blank.count    = 0;

  j = north_pts;
  for (i = 0; i < j; i++)
  { 
    north_tmp[i] = blank;
  }

  j = south_pts;
  for (i = 0; i < j; i++)
  { 
    south_tmp[i] = blank;
  }

  return 0;

}

