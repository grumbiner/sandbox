#include <stdio.h>
#include <macros.h>
#include <stdlib.h>

/* Convert from a raster map to an .xpm (X pixmap) formatted file.  Probably
     several uses, but the one that prompts it is to create a file exportable
     to the www.
   Bob Grumbine 28 July 1995
*/

#define NX 128
#define NY 128

#define CHARBASE 65
#define COLRES    7
#define NCOL     19

typedef struct {
  unsigned char red;
  unsigned char green;
  unsigned char blue;
 } color;

int conctoxpm(FILE *sout, char *fname, int nx, int ny, int ncol, 
              int cbase, int cres, float *map, float *land,
              color *pal, float mul, float add);

int color_bar(color *pal, int ncol, int cbase, int celli, int cellj);

int main(int argc, char *argv[])
{
  float nmap[NY][NX], nland[NY][NX], d1, d2;
  float m, b;
  char **endptr;
  int base;
  FILE *nin, *nout, *nlin;
  int i, j, k;
  int celli=16, cellj=16;
  color pal1[NCOL] = 
  {  0, 55, 80,
     0, 70,100,
    10, 85,100,
    20, 85, 80,
    25,100, 80,
    35,100,100,
    50,100, 90,
    40,100, 50,
    65,100, 40,
    85, 95, 40,
   100, 70, 30,
   100, 50, 20,
   100, 35, 35,
   100,  0,  0,
   100, 100, 100,  /* Water */
   100, 91,  100,  /* Weather */
   100, 75,  100,     /* No data, or bad data */ 
    50, 50, 50,    /* Coast */
     0,  0,  0    }; /* Land */
  color pal[NCOL];


  for ( j = 0; j < NCOL ; j++ ) {
     pal[(NCOL - 1) - j].red   = (pal1[j].red   * 255 )/ 100;
     pal[(NCOL - 1) - j].green = (pal1[j].green * 255 )/ 100;
     pal[(NCOL - 1) - j].blue  = (pal1[j].blue  * 255 )/ 100;
  }

  color_bar(&pal[0], NCOL, CHARBASE, celli, cellj);
  printf("returned from color_bar\n");

  nin = fopen(argv[1], "r");
  nlin = fopen(argv[2], "r");
  nout = fopen(argv[3], "w");
  m = strtol(argv[4], endptr, base);
  b = strtol(argv[5], endptr, base);
  printf("m, b = %f %f\n", m, b);
  fflush(stdout);

  if (nin == NULL || nout == NULL || nlin == NULL ) {
    return -1;
  }

  fread(&d1, sizeof(float), 1, nin);
  fread(&d2, sizeof(float), 1, nin);
  fread(nmap, sizeof(float), NX*NY, nin);


/* d1, d2 are dummy reads for fortran files */
/* nland here is the bathymetry */
  fread(&d1, sizeof(float), 1, nlin);
  fread(&d2, sizeof(float), 1, nlin);
  fread(nland, sizeof(float), NX*NY, nlin);

  conctoxpm(nout, argv[2], NX, NY, NCOL, 
              CHARBASE, COLRES, &nmap[0][0], &nland[0][0], &pal[0], m, b) ;



  return ;

}

int conctoxpm(FILE *sout, char *fname, int nx, int ny, int ncol, 
              int cbase, int cres, float *smap, float *sland,
              color *pal, float mul, float add)
{

  int i, j, index;
  int tc;

/* XPM header */ 
  fprintf(sout,"/* XPM */\n");
  fprintf(sout,"static char * %s [] = {\n",fname);
  fprintf(sout,"\"%d %d %d %d\",\n",nx, ny, ncol , 1);
  for (j = 0; j <  NCOL ; j++) {
    fprintf(sout, "\"%c c #%02x%02x%02x\",\n", (char) (j+ cbase), 
            pal[j ].red, pal[ j ].green, pal[ j ].blue );  
  }

/* Print out the pix map */
  for (j = ny - 1; j >= 0; j-- ) {
    fprintf(sout,"\"");
    for (i = 0; i < nx; i++) {
      index = i + j*nx;

      if (sland[index] < 5.0 ) {
        fprintf(sout,"%c", 0  +  cbase);
      }
      else {
        tc = mul*smap[index]+0.5 + add;
        printf("i, j, smap %3d %3d %d\n", i, j, tc);
        fprintf(sout,"%c", 5 + (min(104, max(0, tc ) ) - 15) 
                            / cres +  cbase);
      }
    }
    fprintf(sout,"\",\n");
  }
  fprintf(sout," }; \n");
    
  return;

}



/* Create a color bar 8 January 1996 */
int color_bar(color *pal, int ncol, int cbase, int celli, int cellj)
{
  char fname[80];
  FILE *sout;
  int k, i, j;

  for (k = 0; k < ncol; k++) {
    sprintf(fname, "colcell%d.xpm", k);
    printf("fname = %s\n",fname);
    sout = fopen(fname,"w");
 
    /* XPM header */ 
    fprintf(sout,"/* XPM */\n");
    fprintf(sout,"static char * %s [] = {\n",fname );
    fprintf(sout,"\"%d %d %d %d\",\n", celli, cellj, 1 , 1);
    fprintf(sout, "\"%c c #%02x%02x%02x\",\n", (char) (0+ cbase), 
              pal[k ].red, pal[ k ].green, pal[ k ].blue );  

    /* Print out the pix map */
    for (j = cellj - 1; j >= 0; j-- ) {
      fprintf(sout,"\"");
      for (i = 0; i < celli; i++) {
        fprintf(sout,"%c", 0  +  cbase);
      }
      fprintf(sout,"\",\n");
    }
    fprintf(sout," }; \n");
    
  }
/* End of printing out the color cells */

  sout = fopen("bar2.html","w");
  for (k = 0; k < ncol; k++) {
    fprintf(sout, "<li><img src=\"colcell%d.xpm\"> Level %d \n", k, k);
  }

  return 0;
}
