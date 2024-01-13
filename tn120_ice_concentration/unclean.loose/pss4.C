#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#ifdef W3LIB
  #include <time.h>
#endif

#include "ncepgrids.h"

// Definitions for the NESDIS 1/16th degree land masks
#include "nesdis.h"
#include "surface_tags.h"


void filter(metricgrid<unsigned char> &gfilt, metricgrid<int> &gland, metricgrid<int> &gsea, metricgrid<float> &gpct, metricgrid<unsigned char> &oldland, char *outname);

template <class T>
void showregion(char *label, latpt ll, latpt ur, metricgrid<T> &gfilt,
                  metricgrid<T> &oldland);


int main(int argc, char *argv[])
{
  dirrec header;
  block  tags;
  short int unblocked[160][160], tmp;
  GRIDTYPE<int> *gland, *gsea;
  GRIDTYPE<unsigned char> *gfilt, *oldland;
  GRIDTYPE<float> *gpct;

  FILE *fin, *fout;
  ijpt x;
  fijpt y;
  latpt ll;
  palette<unsigned char> gg(19,65);

  int i, k, l;
  float latb, lonb;
  #ifdef W3LIB
    char *grib;
    int lgrib = 0;
    tm*  date;
    time_t secs;
    FILE *fgrib;
    int parmno = 255, mxbit = 8, depth = 0, lead = 0;
  #endif


  gland = new GRIDTYPE<int>;
  gsea  = new GRIDTYPE<int>;
 
/* Initialize the ssmi grid counters */
  tmp = 0;
  gland->set(tmp);
  gsea->set(tmp);
  
/* Now get hold of the nesdis data */
  fin = fopen("temphigh","r");
  if (fin == (FILE *) NULL) {
    printf("Failed to open temphigh\n");
    return 1;
  }
  fread(&header, sizeof(dirrec), 1, fin);

  for (i = 0; i < 648; i++) { 
/* Decode a tag block */
    if (header.recno[i] == RECSEA) {
      blocksea(&unblocked[0][0]);    
    }
    else if (header.recno[i] == RECLAND) {
      blockland(&unblocked[0][0]);    
    }
    else {
      fseek(fin, (header.recno[i]-1)*3200, SEEK_SET);
      fread(&tags, sizeof(block), 1, fin);
      unblock(&unblocked[0][0], &tags);
    }

/* Remap over to the destination grid */
    latb = -90.0 + 10.0*( (int) (i/36) );
    lonb = -180. + 10.0*( i % 36 );
    #ifdef VERBOSE
      printf ("block lat, lon %f %f\n",latb, lonb); 
      fflush(stdout);
    #endif

    for (k = 0; k < 160; k++) {
      ll.lat = latb + ((float)k)/16. ;

      for (l = 0; l < 160; l++) {
        ll.lon = lonb + ((float)l)/16. ;

        y = gland->locate(ll);
        if (x.i >= 0.) {
          x.i = (int)(y.i + 0.5);
        }
        else {
          x.i = (int) (y.i - 0.5);
        }
        if (x.j >= 0.) {
          x.j = (int)(y.j + 0.5);
        }
        else {
          x.j = (int) (y.j - 0.5);
        }

        // Skip over points not on the destination grid
          if ( gland->in(x) ) {
            if (unblocked[k][l] == LAND) {
              gland->operator[](x) += 1;
            }
            else if (unblocked[k][l] == SEA) {
              gsea->operator[](x) += 1;
            }
            else {
              printf("unblock value out of range %d %d %d\n",k,l,unblocked[k][l]);
              return -1;
            }
          } /* end of filling in tags */


      } /* end of looping across longitude */
    } /* end of looping across latitude within block */
  } /* end of looping across blocks */

  fout=fopen("globe","w");
  printf("land max min %d %d\n",gland->gridmax(), gland->gridmin() );
  
  printf("sea  max min %d %d\n",gsea ->gridmax(), gsea ->gridmin() );
  gland->xpm("landno.xpm",7,gg);
  gsea->xpm("seano.xpm",7,gg);
  gland->binout(fout);
  gsea->binout(fout);
  fclose(fout);
  fclose(fin);

//////////////////////////////////////////////////////////////////
// Now incorporate the filtering 
  gfilt =   new GRIDTYPE<unsigned char>;
  gpct =    new GRIDTYPE<float>;
  oldland = new GRIDTYPE<unsigned char>;
  fin = fopen(argv[1], "r");
  oldland->binin(fin);
  fclose(fin);
  gfilt->set(100);
  gpct->set(125.0);

  filter(*gfilt, *gland, *gsea, *gpct, *oldland, argv[2]);

// If the W3LIB is defined, we should write out a grib version of
//   the filtered file.  Note that this also requests a third argument.
  #ifdef W3LIB
    grib = new char [ (gfilt->xpoints() * gfilt->ypoints())/8 + 200 ];

    fgrib = fopen(argv[3], "w");
    if (fgrib == (FILE *) NULL) {
      printf("Failed to open the grib output file\n");
      return 1;
    }
    secs = time(&secs);
    date = gmtime(&secs);
    date->tm_year += 1900;  //Need to add this and next because of referencing
    date->tm_mon  += 1; 
    gpct->pds.set_precision(0.01);
    gpct->pds.set_time(date->tm_year, date->tm_mon, date->tm_mday, 
                       date->tm_hour, date->tm_min);
    for (x.j = 0; x.j < gfilt->ypoints() ; x.j++) {
    for (x.i = 0; x.i < gfilt->xpoints() ; x.i++) {
       gpct->operator[](x) = ((float) gfilt->operator[](x)) ;
    }
    }

    *gpct /= 100.;
    printf("About to call gribit\n"); fflush(stdout);
    gpct->gribit(parmno, depth, lead, grib, lgrib, mxbit);
    printf("back from gribit\n"); fflush(stdout);

    printf(" wrote a %d length grib message\n",
           fwrite(grib, sizeof(char), lgrib, fgrib)  ) ; fflush(stdout);
    printf("lgrib = %d\n",lgrib); fflush(stdout);
  #endif

  return 0;
}

#include "filter.C"
#include "region.C"

