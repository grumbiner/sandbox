const int MAXLINE =  81;
const int MAXBLK  = 721;

#define nlat     136
#define nlong   1440
#define lat_min -5000/25
#define nom_res   25.0
#define rearth  6370.0

typedef struct { signed int lat :   10;
                 signed int lon :   12;
                 unsigned int con :  8;
               } sigrid_point;
