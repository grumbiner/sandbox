#include "ssmi.h"
#include "icessmi.h"
#include "icegrids.h"

int ice_add_data(ssmi_tmp *north_tmp, ssmi_tmp  *south_tmp, 
                 int *count_n, int *count_s, struct data_record *a)
{

  int j, n_north, n_south;
  float tlat, tlon;
  float t19v, t19h, t22v, t37v, t37h, t85v, t85h;
  float nasa;
  int ilat, jlon;

/* Peel off data from the record.  Transfer to physical units. */
  for (j = 0; j < NSCANS; j++) {
    tlat = -90. + ( (float)a->data.full[j].latitude)/100.;
    tlon =        ( (float)a->data.full[j].longitude)/100.;
    t19v = ( (float)a->data.full[j].t19v )/100.;
    t19h = ( (float)a->data.full[j].t19h )/100.;
    t22v = ( (float)a->data.full[j].t22v )/100.;
    t37v = ( (float)a->data.full[j].t37v )/100.;
    t37h = ( (float)a->data.full[j].t37h )/100.;
    t85v = ( (float)a->data.full[j].t85v )/100.;
    t85h = ( (float)a->data.full[j].t85h )/100.;
    if ((t19v == 0.) && (tlat == 0.) && (tlon == 0.) ) {
       printf("no good tb\n"); 
    }

    else { 
/* Now check for which hemisphere the data are in. */
    if (tlat >= 0.) {
      mapll(tlat, tlon, &ilat, &jlon, xorig_NORTH, yorig_NORTH,
            eccen2, slat_NORTH, slon_NORTH, rearth, dx, dy, sgn_NORTH);
      if (ilat >= 0 && ilat <= NX_NORTH && jlon >= 0 && jlon <= NY_NORTH) {
        count_n[ilat+jlon*(NX_NORTH)] += 1;
        north_tmp[ilat+jlon*(NX_NORTH)].t19v += a->data.full[j].t19v; 
        north_tmp[ilat+jlon*(NX_NORTH)].t19h += a->data.full[j].t19h; 
        north_tmp[ilat+jlon*(NX_NORTH)].t22v += a->data.full[j].t22v; 
        north_tmp[ilat+jlon*(NX_NORTH)].t37v += a->data.full[j].t37v; 
        north_tmp[ilat+jlon*(NX_NORTH)].t37h += a->data.full[j].t37h; 
        north_tmp[ilat+jlon*(NX_NORTH)].t85v += a->data.full[j].t85v; 
        north_tmp[ilat+jlon*(NX_NORTH)].t85h += a->data.full[j].t85h; 
        nasa = 
          nasa_team(t19v, t19h, t22v, 
                  t37v, t37h, t85v, t85h, 'n');
        if (nasa*100. != BAD_DATA && nasa*100. != NO_DATA && 
            nasa*100. != LAND){
          north_tmp[ilat+jlon*(NX_NORTH)].conc_bar += 
            (int) (0.5 + 100.*nasa);
        }
      } /* end of lat-long range test */
    } /* end of north hemisphere work */

    else {
/* South branch : BEWARE!! must have positive latitudes, take -tlat.*/
      mapll(-tlat, tlon, &ilat, &jlon, xorig_SOUTH, yorig_SOUTH,
            eccen2, slat_SOUTH, slon_SOUTH, rearth, dx, dy, sgn_SOUTH);
      if (ilat >= 0 && ilat <= NX_SOUTH && jlon >= 0 && jlon <= NY_SOUTH) {
        count_s[ilat+jlon*(NX_SOUTH)] += 1;
        south_tmp[ilat+jlon*(NX_SOUTH)].t19v += a->data.full[j].t19v; 
        south_tmp[ilat+jlon*(NX_SOUTH)].t19h += a->data.full[j].t19h; 
        south_tmp[ilat+jlon*(NX_SOUTH)].t22v += a->data.full[j].t22v; 
        south_tmp[ilat+jlon*(NX_SOUTH)].t37v += a->data.full[j].t37v; 
        south_tmp[ilat+jlon*(NX_SOUTH)].t37h += a->data.full[j].t37h; 
        south_tmp[ilat+jlon*(NX_SOUTH)].t85v += a->data.full[j].t85v; 
        south_tmp[ilat+jlon*(NX_SOUTH)].t85h += a->data.full[j].t85h; 
        nasa = 
          nasa_team(t19v, t19h, t22v, 
                    t37v, t37h, t85v, t85h, 's');
        if (nasa*100. != BAD_DATA && nasa*100. != NO_DATA && 
            nasa*100. != LAND){
          south_tmp[ilat+jlon*(NX_SOUTH)].conc_bar += 
            (int) (0.5 + 100.*nasa);
        }

      } /* end of range tested section, south */
  
    } /* end north-south splitting */

    } /* end of else clause - finished working with a good tb */ 

  } /* end for loop */

  return 0;

}
