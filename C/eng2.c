#include <stdio.h>
#include <stdlib.h>
/* #include <f2c.h> */

#define LAND 157
#define COAST 195

/* Convert from a raster map to an .xpm (X pixmap) formatted file.  Probably
     several uses, but the one that prompts it is to create a file exportable
     to the www.
   Bob Grumbine 28 July 1995
*/

#define CHARBASE 65
#define COLRES    7
#define NCOL     17

typedef struct {
  unsigned char red;
  unsigned char green;
  unsigned char blue;
 } color;

int main(int argc, char *argv[])
{
  unsigned char *smap;
  unsigned char *sland;
  FILE *sin, *sout, *slin;
  int i, j, k, nx, ny, index;
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
   100, 100, 100,
    50,  50, 50,
     0,  0,  0    };
  color pal[NCOL];


  for ( j = 0; j < NCOL ; j++ ) {
     pal[(NCOL - 1) - j].red   = pal1[j].red   * 2.55;
     pal[(NCOL - 1) - j].green = pal1[j].green * 2.55;
     pal[(NCOL - 1) - j].blue  = pal1[j].blue  * 2.55;
  }

  sin = fopen(argv[1], "r");
  slin = fopen(argv[2], "r");
  sout = fopen(argv[3], "w");
  nx   = atoi(argv[4]);
  ny   = atoi(argv[5]);

  if (sin == NULL || sout == NULL || slin == NULL ) {
    printf("failed to open a file!\n");
    return -1;
  }

/* Find out how many points there are */
  smap = malloc(nx*ny*sizeof(unsigned char) );
  sland = malloc(nx*ny*sizeof(unsigned char) );

  printf("About to read some data\n");

  i = fread(smap, sizeof(unsigned char), nx*ny, sin);
  j = fread(sland, sizeof(unsigned char), nx*ny, slin);

  printf("Read in %d %d \n",i,j);


/* XPM header */ 
/* Southern Hemisphere */
  fprintf(sout,"/* XPM */\n");
  fprintf(sout,"static char * %s [] = {\n",argv[1]);
  fprintf(sout,"\"%d %d %d %d\",\n",nx, ny, NCOL , 1);
  for (j = 0; j <  NCOL ; j++) {
    fprintf(sout, "\"%c c #%02x%02x%02x\",\n", (char) (j+CHARBASE), 
            pal[j ].red, pal[ j ].green, pal[ j ].blue );  
  }

  printf("nx, ny = %d %d\n",nx, ny);
/* Print out the pix map */
  for (j = 0; j <= ny; j++ ) {
    fprintf(sout,"\"");

    for (i = 0; i < nx; i++) {
/*      printf("i, j, smap %d %d %d\n",i,j,smap[j*nx+i]); */

      index = j*nx + i;
      if (sland[index] == LAND ) {
        fprintf(sout,"%c", 0  + CHARBASE);
      }
      else if ( sland[index] == COAST ) {
        fprintf(sout,"%c", 1 + CHARBASE);
      }
      else if ( smap[index] <= 25. ) {
        fprintf(sout,"%c", 2  + CHARBASE);
      }
/* add case for no data here  add a color for the case above*/
      else {
        fprintf(sout,"%c",(unsigned char) min(104, max(0, (int) smap[j*nx + i]  ) ) /COLRES + CHARBASE + 1);
      }
    }
    fprintf(sout,"\",\n");
  }
  fprintf(sout," }; \n");
    

  return 0;
}
