/* For working with NESDIS mastermap files on 1/8 Bedient grid */
/* Bob Grumbine last change 9 Feb 1995 */
#define GRID        512
#define NFIELD       19
#define NORTH         0
#define SOUTH         1

#define NO_VALUE      0
#define LAND        157

#define TB_19V        0
#define TB_19H        1
#define TB_22V        2
#define TB_37V        3
#define TB_37H        4
#define TB_85V        5
#define TB_85H        6
#define ORBIT         7
#define YEAR          8
#define JULIAN        9
#define ZULUFLG      10
#define LATITUDE     11
#define LONGITUDE    12
#define SOLAR_ZENITH 13
#define TERRAIN_ELEV 14
#define ICE_SNOW_37  15
#define ICE_SNOW_85  16
#define PRECIP       17
#define UNUSED       18

short int type[NFIELD] = {2, 2, 2, 2, 2, 2, 2, 1, 1, 1,
                          3, 2, 2, 2, 1, 3, 3, 3, 1}  ;
float scale[NFIELD] = {64.0,  64.0,  64.0,  64.0, 64.0,
                       64.0,  64.0,   1.0,   1.0,  1.0,
                      100.0, 128.0, 128.0, 128.0,  1.0,
                       10.0,  10.0,  10.0,   1.0}  ;
float low[NFIELD]   = {  0.0,   0.0,    0.0,    0.0,    0.0,
                         0.0,   0.0,    0.0, 1970.0,    0.0,
                         0.0, -99.0, -190.0,   -1.0, -100.0,
                         0.0,   0.0,    0.0,    0.0}  ;
float high[NFIELD]  = {600.0,  600.0,  600.0,  600.0,  600.0,
                       600.0,  600.0, 32768., 2010.0,  370.0,
                        24.0,   99.0,  369.0,  189.0, 6000.0,
                     32768.0, 32768., 32768., 32768.}  ;

#define WEATHER_PARAM 0.05

typedef struct {unsigned int v19    : 16;
                unsigned int h19    : 16;
                unsigned int v37    : 16;
                unsigned int nasa   : 8;
                unsigned int nesdis : 8;
                }                         ssmi;

