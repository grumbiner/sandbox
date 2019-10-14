#include "ssmi.h"

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

 return nerrs;

}

