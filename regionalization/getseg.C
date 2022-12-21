#include "mvector.h"
#include "gshhs.h"
#include "points.h"

size_t getseg(mvector<latpt> &locations, int &level, float &area, 
              float &west, float &east,
              float &north, float &south, FILE *fin) {
// Coastline file input
  struct POINT gshhspoint;
  struct GSHHS gshhsheader;
  int max_east = 270000000;
  
  int i; 
  latpt llat;
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


    #ifdef VERBOSE
       printf("About to resize locations\n"); fflush(stdout);
    #endif 
    locations.resize(gshhsheader.n);
    #ifdef VERBOSE
       printf("back from resize locations\n"); fflush(stdout);
    #endif 
  
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
        //llat.lon = (gshhsheader.greenwich && gshhspoint.x > max_east) ? 
        //   gshhspoint.x * 1.0e-6 - 360.0 : gshhspoint.x * 1.0e-6;
        if (gshhsheader.greenwich && gshhspoint.x > max_east) {
          llat.lon = gshhspoint.x * 1.0e-6 - 360.0;
        }
        else {
          llat.lon = gshhspoint.x * 1.0e-6;
        }

        llat.lat = gshhspoint.y * 1.0e-6;

        locations[i].lon = llat.lon;
        locations[i].lat = llat.lat;
      }
      north = gshhsheader.north / 1.e6;
      south = gshhsheader.south / 1.e6;
      east = gshhsheader.east / 1.e6;
      west = gshhsheader.west / 1.e6;
      area = gshhsheader.area / 10.;
      level = gshhsheader.level;
// Cannot, at the moment, handle boundary crossings
      if (east * west == 0.) {
         printf("%d %d west, east\n",gshhsheader.west, gshhsheader.east);
         fflush(stdout);
         // If unmanageable, call again 
         getseg(locations, level, area, west, east, north, south, fin);
      }

    if (locations[0].lat != locations[gshhsheader.n-1].lat || 
        locations[0].lon != locations[gshhsheader.n-1].lon    ) {
      locations[gshhsheader.n-1] = locations[0];
      //printf("Resetting terminal point\n"); fflush(stdout);
    }
  
  return found;
}
