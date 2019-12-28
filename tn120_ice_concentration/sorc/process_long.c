#include <stdio.h>
#include <stdlib.h>
#include "ssmi.h"
#include "icessmi.h"

int process_long(const char *buffer, int *locate, struct long_data *b)
/* Process the long data records, which include the short data as 
     well. 
   Locate points to the first byte of the long record.
*/
{
  struct short_data c;
  int posit;

  posit = *locate;

  b->scan_counter = 256*buffer[posit   ] + buffer[posit+1];
  posit += 2;

  b->latitude     = 256*buffer[posit   ] + buffer[posit+1];
  b->longitude    = 256*buffer[posit +2] + buffer[posit+3];
  b->t19v         = 256*buffer[posit +4] + buffer[posit+5];
  b->t19h         = 256*buffer[posit +6] + buffer[posit+7];
  b->t22v         = 256*buffer[posit +8] + buffer[posit+9];
  b->t37v         = 256*buffer[posit+10] + buffer[posit+11];
  b->t37h         = 256*buffer[posit+12] + buffer[posit+13];
  b->t85v         = 256*buffer[posit+14] + buffer[posit+15];
  b->t85h         = 256*buffer[posit+16] + buffer[posit+17];
  b->surface_type = buffer[posit+18];
  b->position_num = buffer[posit+19];

  posit  += 20;
  process_short(buffer, &posit, &c);
  b->short_rec[0] = c;
  process_short(buffer, &posit, &c);
  b->short_rec[1] = c;
  process_short(buffer, &posit, &c);
  b->short_rec[2] = c;

  *locate = posit;

  return check_long(b);
}
