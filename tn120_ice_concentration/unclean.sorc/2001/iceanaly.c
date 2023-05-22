#include <stdio.h>
#include "ssmi.h"
#include "icessmi.h"
#include "icegrids.h"

int ice_add_bufr(ssmi_tmp *north_tmp, ssmi_tmp  *south_tmp, 
                 bufr_line *a)
{
/* Version of the SDR ice_add_data suited to work with BUFR input */ 

  int j, n_north, n_south;
  float tlat, tlon;
  float t19v, t19h, t22v, t37v, t37h, t85v, t85h;
  float nasa;
  int ilat, jlon, stype, index;

/* Peel off data from the record.  Transfer to physical units. */
  for (j = 0; j < NSCANS; j++) {
    tlat =  ( a->full[j].latitude);
    if (tlat > -25. && tlat < 20. ) continue;

    tlon = ( (float)a->full[j].longitude);
    t19v = ( (float)a->full[j].t19v );
    t19h = ( (float)a->full[j].t19h );
    t22v = ( (float)a->full[j].t22v );
    t37v = ( (float)a->full[j].t37v );
    t37h = ( (float)a->full[j].t37h );
    t85v = ( (float)a->full[j].t85v );
    t85h = ( (float)a->full[j].t85h );
    stype = a->full[j].surface_type;
    nasa = 0.;


    if ((t19v == 0.) && (tlat == 0.) && (tlon == 0.) ) {
      printf("no good tb\n"); 
      if (tlat > 0.) {
        mapll(tlat, tlon, &ilat, &jlon, xorig_NORTH, yorig_NORTH,
              eccen2, slat_NORTH, slon_NORTH, rearth, dx, dy, sgn_NORTH);
        if ( (ilat >= 0) && (ilat <  (NX_NORTH) ) && 
             (jlon >= 0) && (jlon <  (NY_NORTH) ) ) {
          index = ilat + jlon*(NX_NORTH);
          north_tmp[index].count = 0;
          north_tmp[index].conc_bar = BAD_DATA;
        }
      }
      else {
        mapll(tlat, tlon, &ilat, &jlon, xorig_SOUTH, yorig_SOUTH,
              eccen2, slat_SOUTH, slon_SOUTH, rearth, dx, dy, sgn_SOUTH);
        if (ilat >= 0 && ilat < NX_SOUTH && jlon >= 0 && jlon < NY_SOUTH) {
          index = ilat + jlon*(NX_SOUTH);
          south_tmp[index].count = 0;
          south_tmp[index].conc_bar = BAD_DATA;
        }
      }
      continue;
    } /* done handling the bad data case */

/* Now check for which hemisphere the data are in. */
    if (tlat > 0.) {
      mapll(tlat, tlon, &ilat, &jlon, xorig_NORTH, yorig_NORTH,
            eccen2, slat_NORTH, slon_NORTH, rearth, dx, dy, sgn_NORTH);
      if ( (ilat >= 0) && (ilat <  (NX_NORTH) ) && 
           (jlon >= 0) && (jlon <  (NY_NORTH) )    ) {
        index = ilat+jlon*(NX_NORTH);
        nasa = (int) (0.5 + nasa_team(t19v, t19h, t22v, 
                              t37v, t37h, t85v, t85h, 'n', 1, 1)   );
        if (nasa != NO_DATA && nasa != BAD_DATA && nasa != WEATHER) {
          north_tmp[index].count += 1;
          north_tmp[index].t19v += t19v; 
          north_tmp[index].t19h += t19h; 
          north_tmp[index].t22v += t22v; 
          north_tmp[index].t37v += t37v; 
          north_tmp[index].t37h += t37h; 
          north_tmp[index].t85v += t85v; 
          north_tmp[index].t85h += t85h; 
          if ( north_tmp[index].conc_bar == NO_DATA ||
               north_tmp[index].conc_bar == BAD_DATA ||
               north_tmp[index].conc_bar == WEATHER  ) {
            north_tmp[index].conc_bar = (int) (0.5+nasa);
            north_tmp[index].count = 1;
          }
          else {
            north_tmp[index].conc_bar +=   (int) (0.5 + nasa);
          }
        } /* end of adding data if data not equal bad or land */
        else {
          if ( nasa == BAD_DATA && north_tmp[index].count == 0 ) {
            north_tmp[index].conc_bar = BAD_DATA;
          }
          if ( nasa == WEATHER && north_tmp[index].count == 0 ) {
            north_tmp[index].conc_bar = WEATHER;
          }
        }

      } /* end of lat-long range test */
    } /* end of north hemisphere work */

    else {
/* South branch : BEWARE!! must have positive latitudes, take -tlat.*/
/* Mapll fixed 4 June 1997 to accept physical latitudes */
      mapll(tlat, tlon, &ilat, &jlon, xorig_SOUTH, yorig_SOUTH,
            eccen2, slat_SOUTH, slon_SOUTH, rearth, dx, dy, sgn_SOUTH);
      if (ilat >= 0 && ilat < NX_SOUTH && jlon >= 0 && jlon < NY_SOUTH) {
        index = ilat + jlon*(NX_SOUTH);
        nasa = (int) (0.5 +  nasa_team(t19v, t19h, t22v, 
                                t37v, t37h, t85v, t85h, 's', 1, 1) ) ; 
        if (nasa != NO_DATA && nasa != BAD_DATA && nasa != WEATHER ) {
          south_tmp[index].count += 1;
          south_tmp[index].t19v += t19v; 
          south_tmp[index].t19h += t19h; 
          south_tmp[index].t22v += t22v; 
          south_tmp[index].t37v += t37v; 
          south_tmp[index].t37h += t37h; 
          south_tmp[index].t85v += t85v; 
          south_tmp[index].t85h += t85h; 
          if ( south_tmp[index].conc_bar == NO_DATA || 
               south_tmp[index].conc_bar == BAD_DATA ||
               south_tmp[index].conc_bar == WEATHER   ) {
            south_tmp[index].conc_bar = (int) (0.5 + nasa) ;
            south_tmp[index].count = 1;
          }
          else {
            south_tmp[index].conc_bar += (int) (0.5 + nasa);
          }
        } /* end of adding data if data not equal bad or land */
        else {
         if ( nasa == BAD_DATA && south_tmp[index].count == 0 ) {
           south_tmp[index].conc_bar = BAD_DATA;
         }
         if ( nasa == WEATHER && south_tmp[index].count == 0 ) {
           south_tmp[index].conc_bar = WEATHER;
         }
        }


      } /* end of adding data */

    } /* end north-south splitting */


  } /* end for loop */

  return 0;

}

int ice_add_data(ssmi_tmp *north_tmp, ssmi_tmp  *south_tmp, 
                 struct data_record *a)
{

  int j, n_north, n_south;
  float tlat, tlon;
  float t19v, t19h, t22v, t37v, t37h, t85v, t85h;
  float nasa;
  int ilat, jlon, stype, index;

/* Peel off data from the record.  Transfer to physical units. */
  for (j = 0; j < NSCANS; j++) {
    tlat = ( (float)a->data.full[j].latitude);
    if (tlat > -25. && tlat < 20. ) continue;

    tlon = ( (float)a->data.full[j].longitude);
    t19v = ( (float)a->data.full[j].t19v );
    t19h = ( (float)a->data.full[j].t19h );
    t22v = ( (float)a->data.full[j].t22v );
    t37v = ( (float)a->data.full[j].t37v );
    t37h = ( (float)a->data.full[j].t37h );
    t85v = ( (float)a->data.full[j].t85v );
    t85h = ( (float)a->data.full[j].t85h );
    stype = a->data.full[j].surface_type;
    nasa = 0.;


    if ((t19v == 0.) && (tlat == 0.) && (tlon == 0.) ) {
      printf("no good tb\n"); 
      if (tlat > 0.) {
        mapll(tlat, tlon, &ilat, &jlon, xorig_NORTH, yorig_NORTH,
              eccen2, slat_NORTH, slon_NORTH, rearth, dx, dy, sgn_NORTH);
        if ( (ilat >= 0) && (ilat <  (NX_NORTH) ) && 
             (jlon >= 0) && (jlon <  (NY_NORTH) ) ) {
          index = ilat + jlon*(NX_NORTH);
          north_tmp[index].count = 0;
          north_tmp[index].conc_bar = BAD_DATA;
        }
      }
      else {
        mapll(tlat, tlon, &ilat, &jlon, xorig_SOUTH, yorig_SOUTH,
              eccen2, slat_SOUTH, slon_SOUTH, rearth, dx, dy, sgn_SOUTH);
        if (ilat >= 0 && ilat < NX_SOUTH && jlon >= 0 && jlon < NY_SOUTH) {
          index = ilat + jlon*(NX_SOUTH);
          south_tmp[index].count = 0;
          south_tmp[index].conc_bar = BAD_DATA;
        }
      }
      continue;
    } /* done handling the bad data case */

/* Now check for which hemisphere the data are in. */
    if (tlat > 0.) {
      mapll(tlat, tlon, &ilat, &jlon, xorig_NORTH, yorig_NORTH,
            eccen2, slat_NORTH, slon_NORTH, rearth, dx, dy, sgn_NORTH);
      if ( (ilat >= 0) && (ilat <  (NX_NORTH) ) && 
           (jlon >= 0) && (jlon <  (NY_NORTH) )    ) {
        index = ilat+jlon*(NX_NORTH);
        nasa = (int) (0.5 + nasa_team(t19v, t19h, t22v, 
                              t37v, t37h, t85v, t85h, 'n', 1, 1)   );
        if (nasa != NO_DATA && nasa != BAD_DATA && nasa != WEATHER) {
          north_tmp[index].count += 1;
          north_tmp[index].t19v += (int) (t19v*100 + 0.5); 
          north_tmp[index].t19h += (int) (t19h*100 + 0.5); 
          north_tmp[index].t22v += (int) (t22v*100 + 0.5); 
          north_tmp[index].t37v += (int) (t37v*100 + 0.5); 
          north_tmp[index].t37h += (int) (t37h*100 + 0.5); 
          north_tmp[index].t85v += (int) (t85v*100 + 0.5); 
          north_tmp[index].t85h += (int) (t85h*100 + 0.5); 
          if ( north_tmp[index].conc_bar == NO_DATA ||
               north_tmp[index].conc_bar == BAD_DATA ||
               north_tmp[index].conc_bar == WEATHER  ) {
            north_tmp[index].conc_bar = (int) (0.5+nasa);
            north_tmp[index].count = 1;
          }
          else {
            north_tmp[index].conc_bar +=   (int) (0.5 + nasa);
          }
        } /* end of adding data if data not equal bad or land */
        else {
          if ( nasa == BAD_DATA && north_tmp[index].count == 0 ) {
            north_tmp[index].conc_bar = BAD_DATA;
          }
          if ( nasa == WEATHER && north_tmp[index].count == 0 ) {
            north_tmp[index].conc_bar = WEATHER;
          }
        }

      } /* end of lat-long range test */
    } /* end of north hemisphere work */

    else {
/* South branch : BEWARE!! must have positive latitudes, take -tlat.*/
/* Mapll fixed 4 June 1997 to accept physical latitudes */
      mapll(tlat, tlon, &ilat, &jlon, xorig_SOUTH, yorig_SOUTH,
            eccen2, slat_SOUTH, slon_SOUTH, rearth, dx, dy, sgn_SOUTH);
      if (ilat >= 0 && ilat < NX_SOUTH && jlon >= 0 && jlon < NY_SOUTH) {
        index = ilat + jlon*(NX_SOUTH);
        nasa = (int) (0.5 +  nasa_team(t19v, t19h, t22v, 
                                t37v, t37h, t85v, t85h, 's', 1, 1) ) ; 
        if (nasa != NO_DATA && nasa != BAD_DATA && nasa != WEATHER ) {
          south_tmp[index].count += 1;
          south_tmp[index].t19v += (int) (t19v*100 + 0.5); 
          south_tmp[index].t19h += (int) (t19h*100 + 0.5); 
          south_tmp[index].t22v += (int) (t22v*100 + 0.5); 
          south_tmp[index].t37v += (int) (t37v*100 + 0.5); 
          south_tmp[index].t37h += (int) (t37h*100 + 0.5); 
          south_tmp[index].t85v += (int) (t85v*100 + 0.5); 
          south_tmp[index].t85h += (int) (t85h*100 + 0.5); 
          if ( south_tmp[index].conc_bar == NO_DATA || 
               south_tmp[index].conc_bar == BAD_DATA ||
               south_tmp[index].conc_bar == WEATHER   ) {
            south_tmp[index].conc_bar = (int) (0.5 + nasa) ;
            south_tmp[index].count = 1;
          }
          else {
            south_tmp[index].conc_bar += (int) (0.5 + nasa);
          }
        } /* end of adding data if data not equal bad or land */
        else {
         if ( nasa == BAD_DATA && south_tmp[index].count == 0 ) {
           south_tmp[index].conc_bar = BAD_DATA;
         }
         if ( nasa == WEATHER && south_tmp[index].count == 0 ) {
           south_tmp[index].conc_bar = WEATHER;
         }
        }


      } /* end of adding data */

    } /* end north-south splitting */


  } /* end for loop */

  return 0;

}

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
      north[i].bar_conc = 0.5 + nasa_team(
        (float)north[i].t19v / 100.,
        (float)north[i].t19h / 100.,
        (float)north[i].t22v / 100.,
        (float)north[i].t37v / 100.,
        (float)north[i].t37h / 100.,
        (float)north[i].t85v / 100.,
        (float)north[i].t85h / 100., 'n', 1, 1
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
