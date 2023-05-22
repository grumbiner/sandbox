#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <macros.h>

#include "ncepgrids.h"  

#define MAXSTR 9000
#define LAND 157
#define COAST 195
#define WEATHER 177
#define BAD_DATA 166
#define NO_DATA 224
#define MIN_CONC 15

//Jan 30 1998
int bound(int *x, int imin, int imax) ;
int inparse(fijpt *center, int *range, int *mag, northgrid <unsigned char> b, FILE *cgilog); 

int main(int argc, char *argv[])
{
  northgrid<unsigned char> b, nland;
  grid2<unsigned char> d, dland, e, eland;
  palette gg(19,65);
  FILE *nin, *cgilog;
  int i, j, range;
  int x1, y1, x2, y2, mag;
  float lat, lon, frange;
  latpt ll;
  fijpt center;
  char **endptr;
  char *len[5], *sublen[5];
  char *null, delim;

  mag = 0;
  cgilog = fopen("cgilog","a");

// Section to handle the arguments, read in the basic data, and
//   compute the derived constants.
{

  if (argc == 1) {
    fprintf(cgilog, "invoked with stdin as input \n");
    inparse(&center, &range, &mag, b, cgilog);
    fprintf(cgilog, "returned from inparse\n"); fflush(cgilog);
  }
  else if (argc == 2) {
    fprintf(cgilog, "invoked with a single argument, presumed to be an imagemap\n");
    frange = 550.;
    range = (int) (frange*1000. / b.dx + 0.5);
    bound(&range, 1, min(b.xpoints(), b.ypoints() )/3 );
    if (strlen(argv[1]) > MAXSTR/2) {
      printf("Error in input, your value is _far_ too long\n");
      return -1;
    }
    else {
      for (i = 0; i < 1; i++) {
         len[i] = (char *) malloc(MAXSTR*sizeof(char) );
         sublen[i] = (char *) malloc(MAXSTR*sizeof(char) );
      }
      null = NULL;
      delim = ',';
      len[0] = strtok(argv[1], &delim);
      len[1] = strtok(null, &delim);
      //i = (int) strtod(len[0], endptr);
      //j = (int) strtod(len[1], endptr);
      i = (int) atoi(len[0]);
      j = (int) atoi(len[1]);
      fprintf(cgilog,"Center point at %d %d\n", i, j); fflush(cgilog);
      // Note the following transforms because the upper left corner is
      //  1,1 in imagemaps
      center.i = i - 1;
      center.j = b.ypoints()  - j;
      if (center.i < 0 || center.i >= b.xpoints() ||
          center.j < 0 || center.j >= b.ypoints() ) {
        fprintf(cgilog,"Point out of range\n");
        return -2;
      }
    }  // End of imagemap case
  }

    
    else if (argc < 4) {
      fprintf(cgilog, "Error, insufficient arguments\n");
      return -1;
    }
    else {
      fprintf(cgilog,"Working with command line\n"); fflush(cgilog);
      i = strlen(argv[1]);
      if ( i > 12) { fprintf(cgilog,"Latitude too long\n"); return -1;}
      lat = atof(argv[1]);
      if (fabs(lat) > 90. || lat < 30. ) {
        fprintf(cgilog, "Latitude out of range\n");
        return -3;
      }
    
      if (strlen(argv[2]) > 12) { fprintf(cgilog,"Longitude too long\n"); return -1;}
      lon = atof(argv[2]);
      if (fabs(lon) > 360.) {
        fprintf(cgilog, "Longitude out of range\n");
        return -4;
      }
    
      if (strlen(argv[3]) > 12) {fprintf(cgilog, "Range string too long\n"); return -1;}
      frange = atof(argv[3]);
      range = (int) (frange*1000. / b.dx + 0.5);
      bound(&range, 1, min(b.xpoints(), b.ypoints() )/3 );
  
      fprintf(cgilog,"arg list = %f %f, %d \n", lat, lon, range);

    if (argc <= 4) {
      mag = 0;
    }
    else {
      mag = atoi(argv[4]);
      bound(&mag, 1, 1600);
    }

    ll.lat = lat;
    ll.lon = lon;
    center = b.locate(ll); 
    if (center.i < 0 || center.i >= b.xpoints() ||
        center.j < 0 || center.j >= b.ypoints() ) {
      printf("Point out of range\n");
      return -2;
    }
  }   // End of the >= 4 argument clause

  fprintf(cgilog, "done parsing arguments\n"); fflush(cgilog);
  //nin = fopen("../seaice/analysis/north", "r");
  nin = fopen("north", "r");
  if (nin != NULL ) {
    b.read(b, nin);
  }
  else {
    printf("Failed to open north file\n");
  }
  fclose(nin);


  x1 = (int) (0.5 + center.i - range);
  x2 = (int) (0.5 + center.i + range);
  y1 = (int) (0.5 + center.j - range);
  y2 = (int) (0.5 + center.j + range);
  bound(&x1, 0, b.xpoints()-1);
  bound(&x2, 0, b.xpoints()-1);
  bound(&y1, 0, b.ypoints() -1);
  bound(&y2, 0, b.ypoints() -1);

  if (mag == 0) {
    mag = min(640 / (x2 - x1 + 1), 480 / (y2 - y1 + 1) );
    fprintf(cgilog,"Setting magnification to %d\n", mag);
  }
  else {
    if (mag * (x2 - x1 + 1) > 1600 || mag * (y2 - y1 +1 ) > 1200) {
      mag = min(1600 / (x2 - x1 + 1) , 1200 / (y2 - y1 + 1) );
      fprintf(cgilog,"Resetting Selected magnification to %d\n", mag);
    }
  }
} // end of arghand subsection

////////////////////////////////////////////////////////////////////////

  //nin = fopen("../seaice/analysis/nland.map","r");
  nin = fopen("nland.map","r");
  if (nin == NULL ) {
    printf("Failed to open the land file\n");
    return -1;
  }
  nland.read(nland,nin);
  fclose(nin);


  fprintf(cgilog,"x1, y1, x2, y2 %3d %3d %3d %3d \n",x1, y1, x2, y2); 
  fflush(cgilog);
  d = b.subset(x1, y1, x2, y2);
  dland = nland.subset(x1, y1, x2, y2);
  
  e = d.magnify(mag);
  eland = dland.magnify(mag);
  {
    ijpt loc;
    for (loc.j = 0; loc.j < e.ypoints() ; loc.j++) {
    for (loc.i = 0; loc.i < e.xpoints() ; loc.i++) {
       if (eland[loc] == LAND) {
         e[loc] = 0;
       }
       else if (eland[loc] == COAST) {
         e[loc] = 1;
       }
       else if (e[loc] == NO_DATA) {
         e[loc] = 2;
       }
       else if (e[loc] == WEATHER) {
         e[loc] = 3;
       }
       else {
         e[loc] = 4 + min(100,e[loc])/7; 
       }
    }
    }
  }
  e.xpm("e.xpm", 1, eland, gg);

////////////////////////////////////////////////////////////////////////

  return 0;
}

int bound(int *x, int imin, int imax) {
  if (imin > imax) { return -1; }
  if (*x < imin) *x = imin;
  if (*x > imax) *x = imax;
  return 0;
} 

int inparse(fijpt *center, int *range, int *mag, northgrid<unsigned char> b, FILE *cgilog) 
{
  char *len[5], *sublen[5];
  char stin[MAXSTR];
  char **endptr;
  char *null, delim;
  int i, j;
  latpt ll;
  float lat, lon, frange;
  fijpt tcen;
  
  fgets(&stin[0], MAXSTR-1, stdin);
  if (strlen(stin) > MAXSTR/2) {
    printf("string ridiculously long\n");
    return -1;
  }
    
  for (i = 0; i < 5; i++) {
     len[i] = (char *) malloc(MAXSTR*sizeof(char) );
     sublen[i] = (char *) malloc(MAXSTR*sizeof(char) );
  }

  null = NULL;
  delim='&';
  len[0] = strtok(&stin[0], &delim);
  if (len[0] != NULL) {
    for (i = 1; i < 5; i++) {
      len[i] = strtok(null, &delim);
      if (len[i] == NULL) break;
    }
  }
  for (j = 0; j < i; j++) {
  }


  delim = '=';

  i = strlen(len[0]);
  if ( i > 24) { printf("Lat string too long\n"); return -1;}
  sublen[0] = strtok(len[0], &delim);
  sublen[1] = strtok(null, &delim);
  lat = strtod(sublen[1], endptr);
  if (fabs(lat) > 90. ) {
    printf("Latitude out of range\n");
    return -3;
  }

  if (strlen(len[1]) > 24) { printf("Lon too long\n"); return -1;}
  sublen[0] = strtok(len[1], &delim);
  sublen[1] = strtok(null, &delim);
  lon = strtod(sublen[1], endptr);
  if (fabs(lon) > 360.) {
    printf("Longitude out of range\n");
    return -4;
  }

  if (strlen(len[2]) > 24) {printf("Range string too long\n"); return -1;}
  sublen[0] = strtok(len[2], &delim);
  sublen[1] = strtok(null, &delim);
  frange = strtod(sublen[1], endptr);
  *range = (int) (frange*1000. / b.dx + 0.5);
  bound(range, 1, min(b.xpoints(), b.ypoints() )/3 );

  if (strlen(len[3]) > 24) { printf("Mag too long\n"); return -1;}
  sublen[0] = strtok(len[3], &delim);
  sublen[1] = strtok(null, &delim);
  *mag = (int) strtol(sublen[1], endptr, 0);


  ll.lat = lat;
  ll.lon = lon;
  tcen = b.locate(ll); 
  if (tcen.i < 0 || tcen.i >= b.xpoints() ||
      tcen.j < 0 || tcen.j >= b.ypoints() ) {
    printf("Point out of range\n");
    return -2;
  }

  center->i = tcen.i;
  center->j = tcen.j;
  fprintf(cgilog,"Leaving inparse, center.i,j = %f %f\n", center->i, center->j);
  fflush(cgilog);

  return 0;
}
