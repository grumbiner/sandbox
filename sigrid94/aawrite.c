#include <stdio.h>
#include "sigridaa.h"

extern int mesh_spac, k_mesh_ratio, inf_lat, inf_long, k_latoffset;

void writerec(int count, char *value, int precs, FILE *lists)
{
char num[2];
int conc1, icecon, i1, i2;
int i, lat , lon_spac;
long longit;
sigrid_point point;

  num[0] = *(value+2);
  num[1] = *(value+3);
  conc1 = atoi(num);
  switch(conc1)
   {
     case  0: {icecon = 0; break;}
     case  1: {icecon = 3; break;}
     case  2: {icecon = 7; break;}
     case 91: {icecon = 95; break;}
     case 92: {icecon = 100; break;}
     default:
       { 
        i1 = conc1 / 10;
        i2 = conc1 % 10;
        if (i2 < i1) i2 += 9;
        icecon = 5*(i1+i2);
       }
   }

  lon_spac =           mesh_spac* k_mesh_ratio ;
  lat      = -inf_lat - mesh_spac*(k_latoffset - 1); 
/*  fprintf(lists, " precs, count, %d %d \n", precs, count); */ 
  for (i = precs+1 ; i <= precs+count ; i++)
   {
     if (icecon >= 7) 
     {
       longit = (-(long)inf_long + (long)lon_spac*(i-1) ) % (long)36000;
       point.lat = lat_min - lat/25 ;
       point.lon = longit/25;
       point.con = icecon;
  /*     fprintf(lists," %5d %5d %3d \n", \
                      lat, longit, icecon); */
       if ( 1 != fwrite(&point, sizeof(sigrid_point), 1, lists) ) {
         printf("Write failed, program trashed.\n");
       }
     }
   }

  return;
}
