int ice_add_bufr(ssmi_tmp *north_tmp, ssmi_tmp  *south_tmp,
                 bufr_line *a)
{
/* Version of the SDR ice_add_data suited to work with BUFR input */

  int j, k;
  float tlat, tlon;
  float t19v, t19h, t22v, t37v, t37h, t85v, t85h;
  float nasa;
  int ilat, jlon, stype, index;
  #ifdef HIRES
    int KMAX = 4;
  #else
    int KMAX = 1;
  #endif

/* Peel off data from the record.  Transfer to physical units. */
  for (j = 0; j < NSCANS; j++) {
    t19v = ( (float)a->full[j].t19v );
    t19h = ( (float)a->full[j].t19h );
    t22v = ( (float)a->full[j].t22v );
    t37v = ( (float)a->full[j].t37v );
    t37h = ( (float)a->full[j].t37h );
    stype = a->full[j].surface_type;
    for (k = 0; k < KMAX; k++) {
      if (k > 0) {
        #ifdef HIRES
        tlat =  ( a->full[j].hires[k-1].latitude);
        tlon = ( (float)a->full[j].hires[k-1].longitude);
        t85v = ( (float)a->full[j].hires[k-1].t85v );
        t85h = ( (float)a->full[j].hires[k-1].t85h );
        #endif
      }
      else {
        tlat =  ( a->full[j].latitude);
        tlon = ( (float)a->full[j].longitude);
        t85v = ( (float)a->full[j].t85v );
        t85h = ( (float)a->full[j].t85h );
      }

      if (tlat > -25. && tlat < 20. ) continue;

    /* Note that we are now doubly-nested in j, k though indentation is not*/
    nasa = 0.;

    if ((t19v == 0.) && (tlat == 0.) && (tlon == 0.) ) {
      printf("no good tb\n");
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
          north_tmp[index].t19v += t19v*100.;
          north_tmp[index].t19h += t19h*100.;
          north_tmp[index].t22v += t22v*100.;
          north_tmp[index].t37v += t37v*100.;
          north_tmp[index].t37h += t37h*100.;
          north_tmp[index].t85v += t85v*100.;
          north_tmp[index].t85h += t85h*100.;
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
          south_tmp[index].t19v += t19v*100.;
          south_tmp[index].t19h += t19h*100.;
          south_tmp[index].t22v += t22v*100.;
          south_tmp[index].t37v += t37v*100.;
          south_tmp[index].t37h += t37h*100.;
          south_tmp[index].t85v += t85v*100.;
          south_tmp[index].t85h += t85h*100.;
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


  } /* end for k loop -- checking full resolution parameters*/
  } /* end for j loop -- spot values */

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


/* Perform some bounds checking for the long data */
/* Order of checking is related to the frequency with which
   a given field is erroneous */
/* Note that typically if one number is wrong, so are several others */
/* Robert Grumbine 10 February 1994 */
/* High resolution processing added 11 October 2001 */

int check_bufr(bufr_line *b)
{
  int nerr = 0;
  int i, k;

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

    if ( b->full[i].t19h > b->full[i].t19v) {
      printf("failed v > h 19 test\n");
      nerr += 1;
      zero_bufr(b,i);
    }
    if ( b->full[i].t37h > b->full[i].t37v) {
      printf("failed v > h 37 test\n");
      nerr += 1;
      zero_bufr(b,i);
    }
    if ( b->full[i].t85h > b->full[i].t85v) {
      printf("failed v > h 85 test\n");
      nerr += 1;
      zero_bufr(b,i);
    }

    #ifdef HIRES
    for (k = 0; k < 3; k++) {
      if (check_short_bufr(&(b->full[i].hires[k]) ) != 0) {
        nerr += 1;
        zero_bufr(b, i);
      }
    }
    #endif
       

  } /* end checking */

  return nerr;

}

void zero_bufr(bufr_line *b, int i)
{
   int k;

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
   #ifdef HIRES
   for (k = 0; k < 3; k++) {
     b->full[i].hires[k].t85v = 0;
     b->full[i].hires[k].t85h = 0;
     b->full[i].hires[k].latitude = 0;
     b->full[i].hires[k].longitude = 0;
   }
   #endif

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
  if ( c->t85h > c->t85v ) {
    nerr += 1;
    c->latitude = 0;
    c->longitude = 0;
    c->t85v = 0;
    c->t85h = 0;
  }

  return nerr;

}

