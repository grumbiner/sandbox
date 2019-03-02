const int MAXLINE =  81;
const int MAXBLK  = 721;

#define nlat     181
#define nlong   1440
#define lat_min 4500/25
#define nom_res   25.0
#ifndef bgnh
  #define rearth  6370.0
#endif

typedef struct { unsigned int lat :  9;
                 unsigned int lon : 11;
                 unsigned int con :  8;
               } sigrid_point;
