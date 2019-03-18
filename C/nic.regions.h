/* Include file to declare NIC-specific information */
/* Bob Grumbine 11 October 1994 */

/* Regions */
/* Note that regions are currently specified by Dave Helms only
   In terms of longitude, with an assumed 50-pole latitude for all
   regions */

typedef struct { float latmin; float latmax; 
                 float longmin; float longmax;
                 char *name; }                 region;

#define NORTH_REGIONS 10

const region northern[NORTH_REGIONS] = 
  { {50.0, 90.0,   65.0,   95.0, "Kara"},
    {50.0, 90.0,    0.0,   65.0, "Barents"},
    {50.0, 90.0,  -50.0,    0.0, "EGS"},
    {50.0, 90.0,  -75.0,  -50.0, "Lab"},
    {50.0, 90.0,  -95.0,  -75.0, "Hudson"},
    {50.0, 90.0, -125.0,  -95.0, "Canarc"},
    {50.0, 90.0, -155.0, -125.0, "Beau"},
    {50.0, 90.0, -179.0, -155.0, "Chuck"},
    {50.0, 90.0,  140.0,  179.0, "esib"},
    {50.0, 90.0,   95.0,  140.0, "Laptev"}
  };

#define SOUTH_REGIONS 6

const region southern[SOUTH_REGIONS] =
   { {-90., -50.,   0.0,  60.0, "e30"},
     {-90., -50.,  60.0, 120.0, "e90"},
     {-90., -50., 120.0, 180.0, "e150"},
     {-90., -50.,-180.0,-120.0, "w150"},
     {-90., -50.,-120.0, -60.0, "w90"},
     {-90., -50., -60.0,   0.0, "w30"},
   };

