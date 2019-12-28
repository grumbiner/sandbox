#include <stdio.h>
#include <stdlib.h>
#include "ssmi.h"
#include "icessmi.h"

int check_long(struct long_data *b)
/* Perform some data checking for the long data */
/* Right now, look for lat, long out of bounds */
{
  int nerr = 0;

  if (b->latitude > 18000) {
     nerr += 1;
     b->scan_counter = 0;
     b->latitude     = 0;
     b->longitude    = 0;
     b->t19v         = 0;
     b->t19h         = 0;
     b->t22v         = 0;
     b->t37v         = 0;
     b->t37h         = 0;
     b->t85v         = 0;
     b->t85h         = 0;
     b->surface_type = 0;
     b->position_num = 0;
  }
  if (b->longitude > 36000) {
     nerr += 1;
     b->scan_counter = 0;
     b->latitude     = 0;
     b->longitude    = 0;
     b->t19v         = 0;
     b->t19h         = 0;
     b->t22v         = 0;
     b->t37v         = 0;
     b->t37h         = 0;
     b->t85v         = 0;
     b->t85h         = 0;
     b->surface_type = 0;
     b->position_num = 0;
  }

  return nerr;

}
