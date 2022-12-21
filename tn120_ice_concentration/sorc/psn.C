#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#define VERBOSE
#include "ncepgrids.h"

// Definitions for the NESDIS 1/16th degree land masks
#define RECLAND    -1
#define RECSEA      0 
#define LAND 1
#define SEA  0

typedef struct {
  float latspac;
  float lonspac;
  float tagres; /* km */
  short recno[648];
  char spare[1892];
} dirrec ;
typedef struct {
  unsigned char chars[3200];
} block;


// Definitions of functions
int unblock(short int *unblocked, block *tag); 
int blocksea( short int *unblocked); 
int blockland( short int *unblocked); 

// Definitions for masks:
#define COAST 195
#define MASK 157
#define UNKNOWN 100

void areacheck(latpt &ll1, latpt &ll2, char *name, 
        metricgrid<unsigned char> &gfilt, metricgrid<unsigned char> &oldland) ;

int main(int argc, char *argv[])
{
  FILE *fin, *fout;
  dirrec header;
  block  tags;
  short int unblocked[160][160], tmp;
  northgrid<short int> *gland, *gsea;
  northgrid<unsigned char> *gfilt, *oldland;
  northgrid<float> *gpct;
  ijpt x, x1, x2;
  fijpt y;
  latpt ll, ll1, ll2;
  int count=0, absolute_land=0, absolute_sea=0;
  palette<unsigned char> gg(19,65);

  int i, k, l, index;
  float lat, lon, latb, lonb, dll;
  int mi, mj, nx, ny;

  dll = atof(argv[1]);
//  nx = (int) (360./dll + 0.5);
//  ny = (int) (180./dll + 0.5);

  gland = new northgrid<short int>;
  gsea  = new northgrid<short int>;
  nx = gland->xpoints();
  ny = gland->ypoints();
 
/* Initialize the ssmi grid counters */
  tmp = 0;
  gland->set(tmp);
  gsea->set(tmp);
  
/* Now get hold of the nesdis data */
  fin = fopen("temphigh","r");
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
    printf ("block lat, lon %f %f\n",latb, lonb); fflush(stdout);
    if (latb < 20.0 ) continue;

    for (k = 0; k < 160; k++) {
      lat = latb + ((float)k)/16. ;

      for (l = 0; l < 160; l++) {
        lon = lonb + ((float)l)/16. ;

        ll.lat = lat;
        ll.lon = lon;
        y = gland->locate(ll);
        mi = (int)(y.i+ 0.5);
        mj = (int)(y.j+ 0.5);
        index = mi + mj*nx;

        // Skip over points not on the destination grid
          if (mi >= 0 && mi < nx && mj >= 0 && mj < ny) {
            if (unblocked[k][l] == LAND) {
              gland->grid[index] += 1;
            }
            else if (unblocked[k][l] == SEA) {
              gsea->grid[index] += 1;
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
// Now incorporate the filtering -- This should be a function:
  gfilt = new northgrid<unsigned char>;
  gpct = new northgrid<float>;
  oldland = new northgrid<unsigned char>;
  fin = fopen(argv[2], "r");
  oldland->binin(fin);
  fclose(fin);
  gfilt->set(100);
  gpct->set(1.0);
//  printf("Integral %f\n",gpct->integrate() / 1.e12);
  gpct->set(125.0);

  printf("Passed fixing the land, sea grids \n"); fflush(stdout);
    for (x.j = 0; x.j < gfilt->ypoints() ; x.j++) {
    for (x.i = 0; x.i < gfilt->xpoints() ; x.i++) {
      index = x.i + x.j*gfilt->xpoints() ;
      if (gland->grid[index] >= 32 && gsea->grid[index] == 0 ) {
        gfilt->grid[index] = MASK;
        absolute_land += 1;
      }
      else if (gland->grid[index] == 0 && gsea->grid[index] >= 32) {
        gfilt->grid[index] = SEA;
        absolute_sea += 1;
      }
      else {
        count += 1;
        tmp = gland->grid[index] + gsea->grid[index] ;
        if (tmp == 0) {
          gfilt->grid[index] = UNKNOWN;
          continue;
        }
        if ( (float) gland->grid[index] / tmp > 0.80 ) {
          gfilt->grid[index] = MASK;
        }
        else if ( (float) gland->grid[index] / tmp > 0.20 ) {
          gfilt->grid[index] = COAST;
        }
        else {
          gfilt->grid[index] = SEA;
        }
      }
      // Start figuring up fractional coverages:
      tmp = gland->grid[index] + gsea->grid[index] ;
      if (tmp != 0) {
        gpct->grid[index] = (float) gland->grid[index] / (float) tmp * 100.;
        if (gpct->grid[index] != 0. && gpct->grid[index] != 100.) {
          printf("pct %6.3f  %3d %3d\n",gpct->grid[index], x.i, x.j);
        }
      }

      if (oldland->grid[index] == 157) oldland->grid[index] = MASK;
      if (oldland->grid[index] == 195) oldland->grid[index] = COAST;

      if ( (gfilt->grid[index] == MASK  && oldland->grid[index] != MASK) ||
           (gfilt->grid[index] == SEA   && oldland->grid[index] != 0   ) ||
           (gfilt->grid[index] == COAST && oldland->grid[index] != COAST) ) {

        //ll = gland->locate(x);

        printf("%3d %3d  %3d %3d\n",x.i, x.j, (int) gfilt->grid[index], (int) oldland->grid[index]);
      }
    }
    }

// Done figuring.

    printf("Absolute land points %d\n",absolute_land);
    printf("Absolute sea  points %d\n",absolute_sea );
    printf("Decided points       %d\n",count);
 
    gfilt->xpm("filt.xpm",14,gg);
    fout = fopen(argv[3], "w");
    gfilt->binout(fout);
    fclose(fout);
    printf("Gfilt max, min %d %d\n", (int) gfilt->gridmax(), (int) gfilt->gridmin() );

    printf("Gpct max, min %f %f\n",  gpct->gridmax(),  gpct->gridmin() );
    gpct->xpm("pct.xpm",7, gg);
//    printf("Integrated pctage: %f\n", (float) gpct->integrate()/100./1.e12 );

    for (x.j = 0; x.j < gfilt->ypoints() ; x.j++) {
    for (x.i = 0; x.i < gfilt->xpoints() ; x.i++) {
      index = x.i + x.j * gfilt->xpoints() ;
      if (gfilt->grid[index] == (unsigned char) MASK ) {
        gpct->grid[index] = 1;
      }
      else {
        gpct->grid[index] = 0;
      }
       
      gland->grid[index] = (short int) gfilt->grid[index] 
                         - (short int) oldland->grid[index] 
                         + 128;
    }
    }
    *gland -= gland->gridmin() ;
    printf("Delta max, min: %d %d\n",gland->gridmax(), gland->gridmin() );
    tmp = 2+(gland->gridmax() - gland->gridmin() ) / 19;
    printf("scale by %d\n",tmp);
    gland->xpm("delta.xpm",tmp,gg);
//    printf("Land in filt %f\n", (float) gpct->integrate()/1.e12 );


//  gfilt->printer(stdout); fflush(stdout);

////////////////// Print out grids for a couple selected areas which should have
  ll1.lat = 59.0;
  ll1.lon = -171.0;
  ll2.lat = 56.0;
  ll2.lon = -168.0;
  areacheck(ll1, ll2, "Pribilof Islands", *gfilt, *oldland);

  ll1.lat = 50.0;
  ll1.lon = -93.0;
  ll2.lat = 41.0;
  ll2.lon = -80.0;
  areacheck(ll1, ll2, "Great Lakes", *gfilt, *oldland);

  return 0;
}

void areacheck(latpt &ll1, latpt &ll2, char *name, 
       metricgrid<unsigned char> &gfilt, metricgrid<unsigned char> &oldland) {
  ijpt x, x1, x2;
  fijpt y;
  latpt ll;

  printf("%s\n",name);
  y = gfilt.locate(ll1);
  x1.i = (int) (y.i + 0.5);
  x1.j = (int) (y.j + 0.5);
  y = gfilt.locate(ll2);
  x2.i = (int) (y.i + 0.5);
  x2.j = (int) (y.j + 0.5);
  printf("x1, x2 %3d %3d %3d %3d\n",x1.i, x1.j, x2.i, x2.j);
  for (x.j = min(x1.j,x2.j); x.j < max(x2.j,x1.j); x.j++) {
  for (x.i = min(x1.i,x2.i); x.i < max(x2.i,x1.i); x.i++) {
    ll = gfilt.locate(x);
    if (gfilt[x] != oldland[x] ) {
      printf("%6.3f %6.3f New %3d Old %3d \n",ll.lat, ll.lon, 
         gfilt[x], oldland[x] );
    }
  }
  }

  return; 
}

//////////////////////////////////
/* Given a tag block, unblock it in to a short in array of 1s and 0s */
int unblock(short int *unblocked, block *tag) 
{
  int i, j;
  unsigned char m1=128;
  unsigned char m2= 64;
  unsigned char m3= 32;
  unsigned char m4= 16;
  unsigned char m5=  8;
  unsigned char m6=  4;
  unsigned char m7=  2;
  unsigned char m8=  1;
  int index ;

  for (j = 0; j < 160; j++) {
    for (i = 0; i < 10; i++) {  /* Note range, the 160 is achieved through 
                                   decoding bit by bit through the char */
      index = j*160 + 16*i;

      unblocked[ index   ] = (tag->chars[ index / 8 + 1 ] & m1) != 0 ;
      unblocked[ index+1 ] = (tag->chars[ index / 8  + 1] & m2) != 0 ;
      unblocked[ index+2 ] = (tag->chars[ index / 8  + 1] & m3) != 0 ;
      unblocked[ index+3 ] = (tag->chars[ index / 8  + 1] & m4) != 0 ;
      unblocked[ index+4 ] = (tag->chars[ index / 8  + 1] & m5) != 0 ;
      unblocked[ index+5 ] = (tag->chars[ index / 8  + 1] & m6) != 0 ;
      unblocked[ index+6 ] = (tag->chars[ index / 8  + 1] & m7) != 0 ;
      unblocked[ index+7 ] = (tag->chars[ index / 8  + 1] & m8) != 0 ;
      unblocked[ index+8 ] = (tag->chars[ index / 8 ] & m1) != 0 ;
      unblocked[ index+9 ] = (tag->chars[ index / 8 ] & m2) != 0 ;
      unblocked[ index+10 ] = (tag->chars[ index / 8 ] & m3) != 0 ;
      unblocked[ index+11 ] = (tag->chars[ index / 8 ] & m4) != 0 ;
      unblocked[ index+12 ] = (tag->chars[ index / 8 ] & m5) != 0 ;
      unblocked[ index+13 ] = (tag->chars[ index / 8 ] & m6) != 0 ;
      unblocked[ index+14 ] = (tag->chars[ index / 8 ] & m7) != 0 ;
      unblocked[ index+15 ] = (tag->chars[ index / 8 ] & m8) != 0 ;
    }
  }

  return 0;
}
int blocksea( short int *unblocked) 
{
  int j;
  for (j = 0; j < 160*160; j++) {
    unblocked[j] = SEA ;
  } 

  return 0;
}
int blockland( short int *unblocked) 
{
  int j;
  for (j = 0; j < 160*160; j++) {
    unblocked[j] = LAND ;
  } 

  return 0;
}
