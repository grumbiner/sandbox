#include <stdio.h>
#include <stdlib.h>
#include "ssmi.h"
#include "icessmi.h"

void zero_long(struct long_data *b);

int process_data(const char *buffer, struct data_record *a)
/* Procedure to take the REC_LENGTH bytes from the buffer and
     transfer them into the data record pointed to by a.  The 
     indirection is used so that the main program can decide
     how to deal with the multiple data records which will 
     exist. */

{
  int locate, j, nerrs;
  struct long_data b;
  struct short_data c;

  a->header.block_length = 256*buffer[0]+ buffer[1];
  a->header.mode         = buffer[2];
  a->header.submode      = buffer[3];
  a->header.scan_counter = 256*buffer[4]+buffer[5];
  a->header.b_scan_start = ((256*buffer[6]+buffer[7])*256+buffer[8])*256+
                                    buffer[9];
  a->header.checksum     = 256*buffer[10]+buffer[11];

  a->data.block_length  = 256*buffer[12] + buffer[13];
  a->data.mode          = buffer[14];
  a->data.submode       = buffer[15];

  locate = 16;
  nerrs = 0;

  for(j = 0; j < NSCANS; j++)
  {
     nerrs += process_long(buffer, &locate, &b);
     a->data.full[j] = b;
  }
  
/* If any errors were found on the scan line, zero out the entire set */
  if (nerrs != 0) {
    zero_long(&b);
    for (j = 0; j < NSCANS; j++)
    {
      a->data.full[j] = b;
    }
  }

 return nerrs;

}


int process_header(const char *buffer, char *date, int *nrecs)
{
  struct sdr_scan_header a;

/*  FILE *tmp;                                     */
/*  tmp = fopen("ssmitmp","w");                    */
/*  fwrite(buffer, sizeof(char), REC_LENGTH, tmp); */
/*  fread(&a, sizeof(a), 1, tmp);                  */ 
/*  printf("size of scan_header %d\n",sizeof(a));  */
/*  The above will not work because the sizes assigned by some compilers
    for sub-elements of the struct are not equal to the byte counts in
    the structs.  Will need to manually equate elements 
    Robert Grumbine 10 December 1994 */
/* Wish: return the filled structure for later consideration */
/* Wish: do a check on the header data */

/* Begin the equating */
  a.rev_header2.julian_begin = 256*buffer[660] + buffer[661];
  a.rev_header2.hour_begin   = buffer[662];
  a.rev_header2.min_begin    = buffer[663];
  a.rev_header2.sec_begin    = buffer[664];
  a.rev_header2.julian_end = 256*buffer[665] + buffer[666];
  a.rev_header2.hour_end   = buffer[667];
  a.rev_header2.min_end    = buffer[668];
  a.rev_header2.sec_end    = buffer[669];
  a.rev_header2.julian_ascend = 256*buffer[670] + buffer[671];
  a.rev_header2.hour_ascend   = buffer[672];
  a.rev_header2.min_ascend    = buffer[673];
  a.rev_header2.sec_ascend    = buffer[674];


  printf("\nInformation from rev_header_2\n");

  printf("Begin orbit on %4d %2d %2d %2d\n",a.rev_header2.julian_begin,
                                            a.rev_header2.hour_begin, 
                                            a.rev_header2.min_begin, 
                                            a.rev_header2.sec_begin);
  printf("End orbit on   %4d %2d %2d %2d\n",a.rev_header2.julian_end,
                                            a.rev_header2.hour_end, 
                                            a.rev_header2.min_end, 
                                            a.rev_header2.sec_end);
  printf("Ascending node %4d %2d %2d %2d\n",a.rev_header2.julian_ascend,
                                            a.rev_header2.hour_ascend, 
                                            a.rev_header2.min_ascend, 
                                            a.rev_header2.sec_ascend);


  printf("Number of data blocks (sdr dsb) %d\n", 256*buffer[42]+buffer[43]);
  *nrecs = 256*buffer[42]+buffer[43];

  printf("Info from low bytes %4d %2d %2d %2d %2d  %4d\n",
          256*buffer[20]+buffer[21], buffer[22], buffer[23], buffer[24],
          buffer[25], *nrecs);

  sprintf(date,"%03d%02d%02d%02d\0",a.rev_header2.julian_begin,
                                            a.rev_header2.hour_begin, 
                                            a.rev_header2.min_begin, 
                                            a.rev_header2.sec_begin);
  return 1;

}

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

int process_short(const char *buffer, int *locate, struct short_data *c)
/* Process the short data record */
{

  int posit;

  posit = *locate;

  c->latitude     = 256*buffer[posit  ] + buffer[posit+1];
  c->longitude    = 256*buffer[posit+2] + buffer[posit+3];
  c->t85v         = 256*buffer[posit+4] + buffer[posit+5];
  c->t85h         = 256*buffer[posit+6] + buffer[posit+7];
  c->surface_type = buffer[posit+8];
  c->position_num = buffer[posit+9];

  *locate += 10;

  return check_short(c);
}
/* Perform some bounds checking for the long data */
/* Order of checking is related to the frequency with which
   a given field is erroneous */
/* Note that typically if one number is wrong, so are several others */
/* Robert Grumbine 10 February 1994 */

int check_long(struct long_data *b)
{
  int nerr = 0;

  if ((int) b->surface_type > 8 ) {
    nerr += 1;
    zero_long(b);
    return nerr;
  }

  if ((int) b->t19h > 295*100 || (int) b->t19h <  75*100) {
    nerr += 1;
    zero_long(b);
    return nerr;
  } 
  if ((int) b->t19v > 295*100 || (int) b->t19v < 150*100) {
    nerr += 1;
    zero_long(b);
    return nerr;
  } 

  if ((int) b->t22v > 295*100 || (int) b->t22v < 150*100) {
    nerr += 1;
    zero_long(b);
    return nerr;
  } 

  if ((int) b->t37h > 295*100 || (int) b->t37h < 100*100) {
    nerr += 1;
    zero_long(b);
    return nerr;
  } 
  if ((int) b->t37v > 295*100 || (int) b->t37v < 150*100) {
    nerr += 1;
    zero_long(b);
    return nerr;
  } 

  if ((int) b->t85h > 295*100 || (int) b->t85h < 125*100) {
    nerr += 1;
    zero_long(b);
    return nerr;
  } 
  if ((int) b->t85v > 295*100 || (int) b->t85v < 150*100) {
    nerr += 1;
    zero_long(b);
    return nerr;
  } 

  if ((int) b->latitude > 18000) {
     nerr += 1;
     zero_long(b);
     return nerr;
  }
  if ((int) b->longitude > 36000) {
     nerr += 1;
     zero_long(b);
     return nerr;
  }

  return nerr;

}

void zero_long(struct long_data *b)
{
   if ( ! (
     ((int) b->scan_counter == 0 ) &&
     ((int) b->latitude     == 0 ) &&
     ((int) b->longitude    == 0 ) &&
     ((int) b->t19v         == 0 ) &&
     ((int) b->t19h         == 0 ) &&
     ((int) b->t22v         == 0 ) &&
     ((int) b->t37v         == 0 ) &&
     ((int) b->t37h         == 0 ) &&
     ((int) b->t85v         == 0 ) &&
     ((int) b->t85h         == 0 ) &&
     ((int) b->surface_type == 0 ) &&
     ((int) b->position_num == 0 )   ) )
   {
/*     printf("Bad = ");
     printf("%5d %5d %5d %5d %5d %5d %5d %5d %5d %5d %3d %3d\n",
     b->scan_counter ,
     b->latitude     ,
     b->longitude    ,
     b->t19v         ,
     b->t19h         ,
     b->t22v         ,
     b->t37v         ,
     b->t37h         ,
     b->t85v         ,
     b->t85h         ,
     b->surface_type ,
     b->position_num  );
*/
   }


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
  
  return;
}

/* Bounds checking on a short data record */
/* Robert Grumbine 1 March 1995 */

int check_short(struct short_data *c)
{
  int nerr = 0;

  if (c->latitude > 18000) {
      nerr += 1;
      c->latitude     = 0;
      c->longitude    = 0;
      c->t85v         = 0;
      c->t85h         = 0;
      c->position_num = 0;
  }
  if (c->longitude > 36000) {
      nerr += 1;
      c->latitude     = 0;
      c->longitude    = 0;
      c->t85v         = 0;
      c->t85h         = 0;
      c->position_num = 0;
  }
  if ((int) c->t85v > 295*100 || (int) c->t85h > 295*100) {
      nerr += 1;
      c->latitude     = 0;
      c->longitude    = 0;
      c->t85v         = 0;
      c->t85h         = 0;
      c->position_num = 0;
  }
  if ((int) c->t85v < 150*100 ) {
      nerr += 1;
      c->latitude     = 0;
      c->longitude    = 0;
      c->t85v         = 0;
      c->t85h         = 0;
      c->position_num = 0;
  }
  if ((int) c->t85h < 125*100 ) {
      nerr += 1;
      c->latitude     = 0;
      c->longitude    = 0;
      c->t85v         = 0;
      c->t85h         = 0;
      c->position_num = 0;
  }

  return nerr;
  
}
