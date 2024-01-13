#include <stdio.h>

#include "ssmi.h"
#include "icessmi.h"
#include "icegrids.h"

int ice_add_bufr(ssmi_tmp *north_tmp, ssmi_tmp  *south_tmp, 
                 bufr_line *a)
{
/* Version of the SDR ice_add_data suited to work with BUFR input */ 

  int j;
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
          north_tmp[index].t19v += (unsigned int) (0.5+t19v); 
          north_tmp[index].t19h += (unsigned int) (0.5+t19h); 
          north_tmp[index].t22v += (unsigned int) (0.5+t22v); 
          north_tmp[index].t37v += (unsigned int) (0.5+t37v); 
          north_tmp[index].t37h += (unsigned int) (0.5+t37h); 
          north_tmp[index].t85v += (unsigned int) (0.5+t85v); 
          north_tmp[index].t85h += (unsigned int) (0.5+t85h); 
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
          south_tmp[index].t19v += (unsigned int) (0.5 + t19v); 
          south_tmp[index].t19h += (unsigned int) (0.5 + t19h); 
          south_tmp[index].t22v += (unsigned int) (0.5 + t22v); 
          south_tmp[index].t37v += (unsigned int) (0.5 + t37v); 
          south_tmp[index].t37h += (unsigned int) (0.5 + t37h); 
          south_tmp[index].t85v += (unsigned int) (0.5 + t85v); 
          south_tmp[index].t85h += (unsigned int) (0.5 + t85h); 
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

int process_bufr(bufr_line *b)
/* Process the bufr data records, which will eventually include the short 
    data as well. 
   Only processing is to check for qc purposes.
*/
{
  return check_bufr(b);
}

int process_short_bufr(short_bufr *c)
/* Process the short data record */
{
  return check_short_bufr(c);
}


/* Perform some bounds checking for the long data */
/* Order of checking is related to the frequency with which
   a given field is erroneous */
/* Note that typically if one number is wrong, so are several others */
/* Robert Grumbine 10 February 1994 */

int check_bufr(bufr_line *b)
{
  int nerr = 0;
  int i;

  for (i = 0; i < NSCANS; i++) {
    if ((int) b->full[i].surface_type > 8 ) {
      nerr += 1;
      zero_bufr(b, i);
    }
  
    if ( b->full[i].t19h > 295.0 ||  b->full[i].t19h <  75.0) {
      nerr += 1;
      zero_bufr(b, i);
    } 
    if ( b->full[i].t19v > 295.0 ||  b->full[i].t19v < 150.0) {
      nerr += 1;
      zero_bufr(b, i);
    } 

    if ( b->full[i].t22v > 295.0 ||  b->full[i].t22v < 150.0) {
      nerr += 1;
      zero_bufr(b, i);
    } 

    if ( b->full[i].t37h > 295.0 ||  b->full[i].t37h < 100.0) {
      nerr += 1;
      zero_bufr(b, i);
    } 
    if ( b->full[i].t37v > 295.0 ||  b->full[i].t37v < 150.0) {
      nerr += 1;
      zero_bufr(b, i);
    } 
  
    if ( b->full[i].t85h > 295.0 ||  b->full[i].t85h < 125.0) {
      nerr += 1;
      zero_bufr(b, i);
    } 
    if ( b->full[i].t85v > 295.0 ||  b->full[i].t85v < 150.0) {
      nerr += 1;
      zero_bufr(b, i);
    } 
  
    if ( b->full[i].latitude > 180.) {
       nerr += 1;
       zero_bufr(b, i);
    }
    if ( b->full[i].longitude > 360.) {
       nerr += 1;
       zero_bufr(b, i);
    }
  
  } /* end checking */

  return nerr;
  
}

void zero_bufr(bufr_line *b, int i)
{
   if ( ! (
     ((int) b->full[i].scan_counter == 0 ) &&
     ( b->full[i].latitude     == 0 ) &&
     ( b->full[i].longitude    == 0 ) &&
     ( b->full[i].t19v         == 0 ) &&
     ( b->full[i].t19h         == 0 ) &&
     ( b->full[i].t22v         == 0 ) &&
     ( b->full[i].t37v         == 0 ) &&
     ( b->full[i].t37h         == 0 ) &&
     ( b->full[i].t85v         == 0 ) &&
     ( b->full[i].t85h         == 0 ) &&
     ( b->full[i].surface_type == 0 ) &&
     ( b->full[i].position_num == 0 )   ) )
   {
/*     printf("Bad = ");
     printf("%5d %5d %5d %5d %5d %5d %5d %5d %5d %5d %3d %3d\n",
     b->full[i].scan_counter ,
     b->full[i].latitude     ,
     b->full[i].longitude    ,
     b->full[i].t19v         ,
     b->full[i].t19h         ,
     b->full[i].t22v         ,
     b->full[i].t37v         ,
     b->full[i].t37h         ,
     b->full[i].t85v         ,
     b->full[i].t85h         ,
     b->full[i].surface_type ,
     b->full[i].position_num  );
*/
   }


   b->full[i].scan_counter = 0;
   b->full[i].latitude     = 0;
   b->full[i].longitude    = 0;
   b->full[i].t19v         = 0;
   b->full[i].t19h         = 0;
   b->full[i].t22v         = 0;
   b->full[i].t37v         = 0;
   b->full[i].t37h         = 0;
   b->full[i].t85v         = 0;
   b->full[i].t85h         = 0;
   b->full[i].surface_type = 0;
   b->full[i].position_num = 0;
  
  return;
}

/* Bounds checking on a short data record */
/* Robert Grumbine 1 March 1995 */

int check_short_bufr(short_bufr *c)
{
  int nerr = 0;

  if (c->latitude > 180.) {
      nerr += 1;
      c->latitude     = 0;
      c->longitude    = 0;
      c->t85v         = 0;
      c->t85h         = 0;
  }
  if (c->longitude > 360.) {
      nerr += 1;
      c->latitude     = 0;
      c->longitude    = 0;
      c->t85v         = 0;
      c->t85h         = 0;
  }
  if ( c->t85v > 295.0 ||  c->t85h > 295.0) {
      nerr += 1;
      c->latitude     = 0;
      c->longitude    = 0;
      c->t85v         = 0;
      c->t85h         = 0;
  }
  if ( c->t85v < 150.0 ) {
      nerr += 1;
      c->latitude     = 0;
      c->longitude    = 0;
      c->t85v         = 0;
      c->t85h         = 0;
  }
  if ( c->t85h < 125.0 ) {
      nerr += 1;
      c->latitude     = 0;
      c->longitude    = 0;
      c->t85v         = 0;
      c->t85h         = 0;
  }

  return nerr;
  
}
