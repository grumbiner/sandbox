#include <cstdio>

#include "icegrids.h"
#include "ssmi.h"

void show_bufr(ssmi_bufr_line *b);

/* For error qc 16 March 2004 */
extern int err_stype ;
extern int err_19h_range ;
extern int err_19v_range ;
extern int err_22v_range ;
extern int err_37v_range ;
extern int err_37h_range ;
extern int err_85v_range ;
extern int err_85h_range ;
extern int err_19_polar ;
extern int err_37_polar ;
extern int err_85_polar ;
extern int err_lat      ;
extern int err_lon      ;


/* Perform some bounds checking for the long data */
/* Order of checking is related to the frequency with which
   a given field is erroneous */
/* Note that typically if one number is wrong, so are several others */
/* Robert Grumbine 10 February 1994 */

int check_bufr(ssmi_bufr_line *b) {
  int nerr = 0, npts = 0;
  int i;

  #ifdef VERBOSE2
  show_bufr(b);
  #endif

  for (i = 0; i < NSCANS; i++) {
    if ( b->full[i].t19h > 295.0 ||  b->full[i].t19h <  75.0) {
      nerr += 1;
      err_19h_range += 1;
    } 
    if ( b->full[i].t19v > 295.0 ||  b->full[i].t19v < 150.0) {
      nerr += 1;
      err_19v_range += 1;
    } 

    if ( b->full[i].t22v > 295.0 ||  b->full[i].t22v < 150.0) {
      nerr += 1;
      err_22v_range += 1;
    } 

    if ( b->full[i].t37h > 295.0 ||  b->full[i].t37h < 100.0) {
      err_37h_range += 1;
      nerr += 1;
    } 
    if ( b->full[i].t37v > 295.0 ||  b->full[i].t37v < 150.0) {
      err_37v_range += 1;
      nerr += 1;
    } 
  
    if ( b->full[i].t85h > 295.0 ||  b->full[i].t85h < 125.0) {
      err_85h_range += 1;
      nerr += 1;
    } 
    if ( b->full[i].t85v > 295.0 ||  b->full[i].t85v < 150.0) {
      err_85v_range += 1;
      nerr += 1;
    } 

/* Polarization tests: */ 
    if ( b->full[i].t19h > b->full[i].t19v) {
      #ifdef VERBOSE
        printf("failed v > h 19 test\n");
      #endif
      nerr += 1;
      err_19_polar += 1;
    }
    if ( b->full[i].t37h > b->full[i].t37v) {
      #ifdef VERBOSE
        printf("failed v > h 37 test\n");
      #endif
      nerr += 1;
      err_37_polar += 1;
    }
    if ( b->full[i].t85h > b->full[i].t85v) {
      #ifdef VERBOSE
        printf("failed v > h 85 test\n");
      #endif
      nerr += 1;
      err_85_polar += 1;
    }
  
/* Location and surface type tests: */
    if ((int) b->full[i].surface_type > 8 ) {
      nerr += 1;
      err_stype += 1; 
    }
    if ( b->full[i].latitude > 180.) {
       nerr += 1;
       err_lat += 1;
    }
    if ( b->full[i].longitude > 360.) {
       nerr += 1;
       err_lon += 1;
    }


    if (nerr != 0) {
      npts += 1;
      zero_bufr(b, i);
    }

  
  } /* end checking */

  return npts;
  
}

/* Bounds checking on a short data record */
/* Robert Grumbine 1 March 1995 */

int check_short_bufr(ssmi_short_bufr *c) {
  int nerr = 0;

  if (c->latitude > 180.) {
      nerr += 1;
      err_lat += 1;
  }
  if (c->longitude > 360.) {
      nerr += 1;
      err_lon += 1;
  }
  if ( c->t85v > 295.0 ||  c->t85v < 150.0 ) {
      nerr += 1;
      err_85v_range += 1;
  }
  if ( c->t85h < 125.0 ||  c->t85h > 295.0) {
      err_85h_range += 1;
      nerr += 1;
  }
/* Polarization test:       */
  if ( c->t85h > c->t85v ) {
      nerr += 1;
      err_85_polar += 1;
  }
  if (nerr != 0) {
      c->latitude     = 0;
      c->longitude    = 0;
      c->t85v         = 0;
      c->t85h         = 0;
  }

  return nerr;
  
}
void show_bufr(ssmi_bufr_line *b) {
  
  printf("in bufr, %d %d %d %d %d %d %d %d\n", b->satno, b->year, 
           b->month, b->day, b->hour, b->mins, b->secs, b->scan_no);

}
