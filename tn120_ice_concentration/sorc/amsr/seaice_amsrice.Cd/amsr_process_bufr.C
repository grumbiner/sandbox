#include "amsrice.h"
#include "icegrids.h"

// Process AMSR bufr information
// Derived from SSMI processing codes


/* For error qc 16 March 2004 */
extern int err_19h_range ;
extern int err_19v_range ;
extern int err_24v_range ;
extern int err_24h_range ;
extern int err_37v_range ;
extern int err_37h_range ;
extern int err_89v_range ;
extern int err_89h_range ;
extern int err_19_polar ;
extern int err_37_polar ;
extern int err_89_polar ;
extern int err_lat      ;
extern int err_lon      ;

int ice_add_bufr(amsr_tmp *north_tmp, amsr_tmp  *south_tmp, 
                 amsr_scan_points *a, int nread, 
                 amsr_team2_tables &arctic, amsr_team2_tables &antarctic) {

  int j, k;
  float tlat, tlon;
  float t19v, t19h, t24v, t24h, t37v, t37h, t89v, t89h;
  float nasa;
  int ilat, jlon, index;

/* Peel off data from the record.  Transfer to physical units. */
  for (j = 0; j < NSPOTS; j++) {
    for (k = 0; k < 2; k++) {
      t19v = ( (float)a->full[j].obs[AMSR_T19V].tmbr  );
      t19h = ( (float)a->full[j].obs[AMSR_T19H].tmbr  );
      t24v = ( (float)a->full[j].obs[AMSR_T24V].tmbr  );
      t24h = ( (float)a->full[j].obs[AMSR_T24H].tmbr  );
      t37v = ( (float)a->full[j].obs[AMSR_T37V].tmbr  );
      t37h = ( (float)a->full[j].obs[AMSR_T37H].tmbr  );

      if (k == 0) {
        t89v = ( (float)a->full[j].obs[AMSR_T89Va].tmbr );
        t89h = ( (float)a->full[j].obs[AMSR_T89Ha].tmbr );
      } 
      else {
        t89v = ( (float)a->full[j].obs[AMSR_T89Vb].tmbr );
        t89h = ( (float)a->full[j].obs[AMSR_T89Hb].tmbr );
      }

      tlat = ( (float)a->full[j].clat[k]);
      tlon = ( (float)a->full[j].clon[k]);
      if (tlon < 0) tlon += 360.;
  
      #ifdef DEBUG
        if (t19v*t19h*t24v != 0 && (tlat > 24 || tlat < -30) ) {
        printf("%5.1f %5.1f %5.1f %5.1f %5.1f %5.1f %5.1f %5.1f  %6.2f %7.2f\n",
               t19h, t19v, t24h, t24v, t37h, t37v, t89h, t89v, tlat, tlon);
        }
        else {
          continue;
        }
      #endif
  


/* Changed from -25, +20 to -30, 24 on 16 March 2004 */
      if (tlat > -30. && tlat < 24. ) continue;
  
      nasa = 0.;
  
      if ((t19v == 0.) && (tlat == 0.) && (tlon == 0.) ) {
        printf("probably shouldn't be here\n"); fflush(stdout);
        continue;
      } /* done handling the bad data case */
  
  /* Now check for which hemisphere the data are in. */
      if (tlat > 0.) {
        mapll(tlat, tlon, &ilat, &jlon, xorig_NORTH, yorig_NORTH,
              eccen2, slat_NORTH, slon_NORTH, rearth, dx, dy, sgn_NORTH);

        if ( (ilat >= 0) && (ilat <  (NX_NORTH) ) && 
             (jlon >= 0) && (jlon <  (NY_NORTH) )    ) {
          index = ilat+jlon*(NX_NORTH);

          nasa = (int) (0.5 + nasa_team2(t19v, t19h, t24v, 
                                t37v, t37h, t89v, t89h, arctic)  );
          //DEBUG: printf("debug conc = %3d\n",(int) (0.5 + nasa));
          if (nasa != BAD_DATA && nasa != WEATHER) {

/* Need the count == 0 test along with flags because flag values can be
   reached for count != 0 and summing up, e.g. values of 89 and 88 to reach
   177 (weather)
   20 April 2004
*/
            if ( ( north_tmp[index].conc_bar == NO_DATA ||
                   north_tmp[index].conc_bar == BAD_DATA ||
                   north_tmp[index].conc_bar == WEATHER  ) && 
                   north_tmp[index].count == 0) {
              if (north_tmp[index].count != 0) {
                printf("conc_bar was %d, count was %d 19v %d\n", 
                    north_tmp[index].conc_bar, 
                    north_tmp[index].count, north_tmp[index].t19v );
              }
              north_tmp[index].t19v = (unsigned int) (0.5+t19v*10); 
              north_tmp[index].t19h = (unsigned int) (0.5+t19h*10); 
              north_tmp[index].t24v = (unsigned int) (0.5+t24v*10); 
              north_tmp[index].t24h = (unsigned int) (0.5+t24h*10); 
              north_tmp[index].t37v = (unsigned int) (0.5+t37v*10); 
              north_tmp[index].t37h = (unsigned int) (0.5+t37h*10); 
              north_tmp[index].t89v = (unsigned int) (0.5+t89v*10); 
              north_tmp[index].t89h = (unsigned int) (0.5+t89h*10); 
              north_tmp[index].conc_bar = (int) (0.5+nasa);
              north_tmp[index].count = 1;
            }
            else {
              north_tmp[index].conc_bar +=   (int) (0.5 + nasa);
              north_tmp[index].t19v += (unsigned int) (0.5+t19v*10); 
              north_tmp[index].t19h += (unsigned int) (0.5+t19h*10); 
              north_tmp[index].t24v += (unsigned int) (0.5+t24v*10); 
              north_tmp[index].t24h += (unsigned int) (0.5+t24h*10); 
              north_tmp[index].t37v += (unsigned int) (0.5+t37v*10); 
              north_tmp[index].t37h += (unsigned int) (0.5+t37h*10); 
              north_tmp[index].t89v += (unsigned int) (0.5+t89v*10); 
              north_tmp[index].t89h += (unsigned int) (0.5+t89h*10); 
              north_tmp[index].count += 1;
            }
          } /* end of adding data if data not equal bad or land */
          else {
            if ( nasa == BAD_DATA && north_tmp[index].count == 0 ) {
              north_tmp[index].conc_bar = BAD_DATA;
            }
            if ( nasa == WEATHER && north_tmp[index].count == 0 ) {
              north_tmp[index].conc_bar = WEATHER;
            }
            north_tmp[index].weather_count += 1;
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

          nasa = (int) (0.5 + nasa_team2(t19v, t19h, t24v, 
                                t37v, t37h, t89v, t89h, antarctic)  );
          //DEBUG: printf("debug conc = %3d\n",(int) (0.5 + nasa));

          if (nasa != BAD_DATA && nasa != WEATHER ) {
            if ( ( south_tmp[index].conc_bar == NO_DATA ||
                   south_tmp[index].conc_bar == BAD_DATA ||
                   south_tmp[index].conc_bar == WEATHER  ) && 
                   south_tmp[index].count == 0) {
              south_tmp[index].t19v = (unsigned int) (0.5 + t19v*10); 
              south_tmp[index].t19h = (unsigned int) (0.5 + t19h*10); 
              south_tmp[index].t24v = (unsigned int) (0.5 + t24v*10); 
              south_tmp[index].t24h = (unsigned int) (0.5 + t24h*10); 
              south_tmp[index].t37v = (unsigned int) (0.5 + t37v*10); 
              south_tmp[index].t37h = (unsigned int) (0.5 + t37h*10); 
              south_tmp[index].t89v = (unsigned int) (0.5 + t89v*10); 
              south_tmp[index].t89h = (unsigned int) (0.5 + t89h*10); 
              south_tmp[index].conc_bar = (int) (0.5 + nasa) ;
              south_tmp[index].count = 1;
            }
            else {
              south_tmp[index].t19v += (unsigned int) (0.5 + t19v*10); 
              south_tmp[index].t19h += (unsigned int) (0.5 + t19h*10); 
              south_tmp[index].t24v += (unsigned int) (0.5 + t24v*10); 
              south_tmp[index].t24h += (unsigned int) (0.5 + t24h*10); 
              south_tmp[index].t37v += (unsigned int) (0.5 + t37v*10); 
              south_tmp[index].t37h += (unsigned int) (0.5 + t37h*10); 
              south_tmp[index].t89v += (unsigned int) (0.5 + t89v*10); 
              south_tmp[index].t89h += (unsigned int) (0.5 + t89h*10); 
              south_tmp[index].conc_bar += (int) (0.5 + nasa);
              south_tmp[index].count += 1;
            }
          } /* end of adding data if data not equal bad or land */
          else {
           if ( nasa == BAD_DATA && south_tmp[index].count == 0 ) {
             south_tmp[index].conc_bar = BAD_DATA;
           }
           if ( nasa == WEATHER && south_tmp[index].count == 0 ) {
             south_tmp[index].conc_bar = WEATHER;
           }
           south_tmp[index].weather_count += 1;
          }
  
  
        } /* end of adding data */
  
      } /* end north-south splitting */

    } /* end of checking multiple spots (89 GHz) */
  } /* end for loop */

  return 0;

}

/* Process the bufr data records
   Only processing is to check for qc purposes.
*/
int process_bufr(amsr_scan_points *b) {
  return check_bufr(b);
}

/* Perform some bounds checking for the long data */
/* Order of checking is related to the frequency with which
   a given field is erroneous */
/* Note that typically if one number is wrong, so are several others */
/* Robert Grumbine 10 February 1994 */

int check_short_bufr(amsr_short_bufr *c) ;
int check_bufr(amsr_scan_points *b) {
  int nerr = 0, npts = 0;
  int i, k;


  //for (i = 0; i < NSPOTS; i++) {
  //for (k = 0; k < 14; k++) {
  //  printf(" %f ",b->full[i].obs[k].tmbr);
  //}
  //printf("\n");
  //}
  //exit(1);

  for (i = 0; i < NSPOTS; i++) {
    if ( b->full[i].obs[AMSR_T19H].tmbr > 305.0 ||  
         b->full[i].obs[AMSR_T19H].tmbr <  75.0) {
      nerr += 1;
      err_19h_range += 1;
    } 
    if ( b->full[i].obs[AMSR_T19V].tmbr > 295.0 ||  
         b->full[i].obs[AMSR_T19V].tmbr < 150.0) {
      nerr += 1;
      err_19v_range += 1;
    } 

    if ( b->full[i].obs[AMSR_T24H].tmbr > 285.0 ||  
         b->full[i].obs[AMSR_T24H].tmbr < 100.0) {
      nerr += 1;
      err_24v_range += 1;
    } 
    if ( b->full[i].obs[AMSR_T24V].tmbr > 285.0 ||  
         b->full[i].obs[AMSR_T24V].tmbr < 150.0) {
      nerr += 1;
      err_24h_range += 1;
    } 

    if ( b->full[i].obs[AMSR_T37H].tmbr > 285.0 ||  
         b->full[i].obs[AMSR_T37H].tmbr < 100.0) {
      err_37h_range += 1;
      nerr += 1;
    } 
    if ( b->full[i].obs[AMSR_T37V].tmbr > 285.0 ||  
         b->full[i].obs[AMSR_T37V].tmbr < 150.0) {
      err_37v_range += 1;
      nerr += 1;
    } 
  
    if ( b->full[i].obs[AMSR_T89Ha].tmbr > 285.0 ||  
         b->full[i].obs[AMSR_T89Ha].tmbr < 135.0) {
      err_89h_range += 1;
      nerr += 1;
    } 
    if ( b->full[i].obs[AMSR_T89Va].tmbr > 285.0 ||  
         b->full[i].obs[AMSR_T89Va].tmbr < 135.0) {
      err_89v_range += 1;
      nerr += 1;
    } 

/* Polarization tests: */ 
    if ( b->full[i].obs[AMSR_T19H].tmbr > b->full[i].obs[AMSR_T19V].tmbr) {
      nerr += 1;
      err_19_polar += 1;
    }
    if ( b->full[i].obs[AMSR_T37H].tmbr > b->full[i].obs[AMSR_T37V].tmbr) {
      nerr += 1;
      err_37_polar += 1;
    }
    if ( b->full[i].obs[AMSR_T89Ha].tmbr > b->full[i].obs[AMSR_T89Va].tmbr) {
      nerr += 1;
      err_89_polar += 1;
    }

  
/* Location and surface type tests: */
    for (int k = 0; k < 2; k++) {
      if ( b->full[i].clat[k] > 180.) {
         nerr += 1;
         err_lat += 1;
      }
      if ( b->full[i].clon[k] > 360.) {
         nerr += 1;
         err_lon += 1;
      }
    }

    if (nerr != 0) {
      npts += 1;
      zero_bufr(b, i);
    }

  
  } /* end checking */

  return npts;
  
}

void zero_bufr(amsr_scan_points *b, int i) {

   for (int k = 0; k < 2; k++) {
     b->full[i].clat[k]    = 0;
     b->full[i].clon[k]    = 0;
   }
   for (int j = 0; j < 14; j++) {
     b->full[i].obs[j].tmbr         = 0;
     b->full[i].obs[j].channel      = 0;
   }

  return;
}

/* Bounds checking on a short data record */
/* Robert Grumbine 1 March 1995 */

int check_short_bufr(amsr_short_bufr *c) {
  int nerr = 0;

  if (c->latitude > 180.) {
      nerr += 1;
      err_lat += 1;
  }
  if (c->longitude > 360.) {
      nerr += 1;
      err_lon += 1;
  }
  if ( c->t89v > 285.0 ||  c->t89v < 150.0 ) {
      nerr += 1;
      err_89v_range += 1;
  }
  if ( c->t89h > 285.0 ||  c->t89h < 125.0) {
      err_89h_range += 1;
      nerr += 1;
  }
/* Polarization test:       */
  if ( c->t89h > c->t89v ) {
      nerr += 1;
      err_89_polar += 1;
  }

  if (nerr != 0) {
      c->latitude     = 0;
      c->longitude    = 0;
      c->t89v         = 0;
      c->t89h         = 0;
  }

  return nerr;
  
}
