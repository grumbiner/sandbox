#include "ncepgrids.h"

// Read in GMT coastline file and .............
// Robert Grumbine 2 March 2005

///////////////// GMT shorelines
#include "gshhs.h"

///////////// Utilities
// Sbr to get the next segment:
size_t getseg(int &level, float &west, float &east,
            float &north, float &south, FILE *fin, FILE *fout);

///////////////////////////////////////////////////////////////////
int bcount;

int main(int argc, char *argv[]) {
  FILE *fin, *fout;

  float north, south, east, west;
  ijpt loc, tloc;
  int j, k;
  int level;
  size_t found;

////////////////////////////////////////

  fin = fopen(argv[1], "r");
  if (fin == (FILE *) NULL) {
    printf("Failed to open %s\n",argv[1]);
    return 1;
  }
  fout = fopen(argv[2], "w");
  if (fout == (FILE *) NULL) {
    printf("Failed to open output file %s\n",argv[2]);
    return 2;
  }

  j = 0; k = 0;
  while (!feof(fin) ) {
    found = getseg(level, west, east, north, south, fin, fout);
    k += 1;
    if (found != (size_t) 0) {
      j += 1;
      continue;
    }

  } // end looping through segments
  fclose(fin);

  printf("read in %d segments and used %d\n",k, j);

   
  return 0;
}

size_t getseg(int &level, float &west, float &east,
            float &north, float &south, FILE *fin, FILE *fout) {
// Coastline file input
  struct POINT gshhspoint;
  struct GSHHS gshhsheader;

  int i;
  size_t found;

  found = fread ( (void*) &gshhsheader, sizeof(struct GSHHS), 1, fin);
  if (found == (size_t) 0) return found;

  // We do need FLIP in Linux on Intel
  #ifdef FLIP
     gshhsheader.id = swabi4 ((unsigned int)gshhsheader.id);
     gshhsheader.n = swabi4 ((unsigned int)gshhsheader.n);
     gshhsheader.level = swabi4 ((unsigned int)gshhsheader.level);
     gshhsheader.west = swabi4 ((unsigned int)gshhsheader.west);
     gshhsheader.east = swabi4 ((unsigned int)gshhsheader.east);
     gshhsheader.south = swabi4 ((unsigned int)gshhsheader.south);
     gshhsheader.north = swabi4 ((unsigned int)gshhsheader.north);
     gshhsheader.area = swabi4 ((unsigned int)gshhsheader.area);
     gshhsheader.greenwich = swabi2 ((unsigned int)gshhsheader.greenwich);
     gshhsheader.source = swabi2 ((unsigned int)gshhsheader.source);
  #endif

  printf("area = %d\n",gshhsheader.area);

  //if (gshhsheader.area > 10000 || 
  //    gshhsheader.area < 30                       ) {
  // if it is land, or land inside water on land, skip
  //  if it is smaller than 10% of a highres grid cell, skip
  if ( (gshhsheader.level == 3 || gshhsheader.level == 1) ||
        gshhsheader.area < 12.7*12.7*10 /10.  ) {
    found = 0;
  }

  if (found != 0) {
    fwrite ( (void*) &gshhsheader, sizeof(struct GSHHS), 1, fout);
  }

  for (i = 0; i < gshhsheader.n; i++) {
      
    if (fread ((void *)&gshhspoint, (size_t)sizeof(struct POINT), 
                            (size_t)1,                  fin) != 1) {
      fprintf (stderr, 
           "gshhs:  Error reading for polygon %d, point %d.\n", 
           gshhsheader.id, i);
        return 0;
      }
      #ifdef FLIP
        gshhspoint.x = swabi4 ((unsigned int)gshhspoint.x);
        gshhspoint.y = swabi4 ((unsigned int)gshhspoint.y);
      #endif
      if (found != 0) {
        fwrite ((void *)&gshhspoint, (size_t)sizeof(struct POINT), 
                                     (size_t)1,                  fout) ;
      }

    }

  return found;
}
