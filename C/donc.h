/* Include file to declare NIC-specific information */
/* Bob Grumbine 11 October 1994 */

/* Regions */
/* Note that regions are currently specified by Dave Helms only
   In terms of longitude, with an assumed 50-pole latitude for all
   regions */

typedef struct { float latmin; float latmax; 
                float longmin; float longmax;
                char *name; } region;

#define NORTH_REGIONS 1

const region northern[NORTH_REGIONS] = 
  { {50.0, 75.0, -200.0, -140.0, "Beaufort"}
  };

#define SOUTH_REGIONS 0

