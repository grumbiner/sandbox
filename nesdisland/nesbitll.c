#include <stdio.h>
#include <math.h>
#include <stdlib.h>

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


int unblock(short int *unblocked, block *tag); 
int blocksea( short int *unblocked); 
int blockland( short int *unblocked); 

int main(int argc, char *argv[])
{
  FILE *fin, *fout;
  dirrec header;
  block  tags;
  short int unblocked[160][160];
  short int *gland, *gsea;

  int i, k, l, index;
  float lat, lon, latb, lonb, dll;
  int mi, mj, nx, ny;

  dll = atof(argv[1]);
  nx = (int) (360./dll + 0.5);
  ny = (int) (180./dll + 0.5);

  gland = malloc(sizeof(short int) * nx*ny);
  gsea  = malloc(sizeof(short int) * nx*ny);

 
/* Initialize the ssmi grid counters */
  for (k = 0; k < ny; k++) {
    for (l = 0; l < nx; l++) {
      index = l + k*nx;
      gland[index] = 0;
      gsea[index]  = 0; 
    }
  }
  
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

/* Remap over to the ssmi grid */
    latb = -90.0 + 10.0*( (int) (i/36) );
    lonb = -180. + 10.0*( i % 36 );
    printf ("block lat, lon %f %f\n",latb, lonb);

    for (k = 0; k < 160; k++) {
      lat = latb + ((float)k)/16. ;

      for (l = 0; l < 160; l++) {
        lon = lonb + ((float)l)/16. ;

        mi = (int) ( (lon - 0.5*dll)/dll + 0.5 );
        mj = (int) ( -(lat - 90. + 0.5*dll)/dll + 0.5 );
        if ( mi < 0 ) mi = mi + nx;
        if ( mi >= nx ) mi = mi - nx;
        index = mi + mj*nx;

          if (mi >= 0 && mi < nx && mj >= 0 && mj < ny) {
            if (unblocked[k][l] == LAND) {
              gland[index] += 1;
            }
            else if (unblocked[k][l] == SEA) {
              gsea[index] += 1;
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
  fwrite(gland, sizeof(short int), nx*ny, fout);
  fwrite(gsea , sizeof(short int), nx*ny, fout);


  return 0;
}

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
