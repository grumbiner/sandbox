#include <stdio.h>
#include <stdlib.h>
#include "ssmi.h"
#include "icessmi.h"

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
      c->position_num = 0;
  }
  if (c->longitude > 360.) {
      nerr += 1;
      c->latitude     = 0;
      c->longitude    = 0;
      c->t85v         = 0;
      c->t85h         = 0;
      c->position_num = 0;
  }
  if ( c->t85v > 295.0 ||  c->t85h > 295.0) {
      nerr += 1;
      c->latitude     = 0;
      c->longitude    = 0;
      c->t85v         = 0;
      c->t85h         = 0;
      c->position_num = 0;
  }
  if ( c->t85v < 150.0 ) {
      nerr += 1;
      c->latitude     = 0;
      c->longitude    = 0;
      c->t85v         = 0;
      c->t85h         = 0;
      c->position_num = 0;
  }
  if ( c->t85h < 125.0 ) {
      nerr += 1;
      c->latitude     = 0;
      c->longitude    = 0;
      c->t85v         = 0;
      c->t85h         = 0;
      c->position_num = 0;
  }

  return nerr;
  
}
