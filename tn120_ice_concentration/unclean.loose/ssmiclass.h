#ifndef SSMICLASS_INCLUDE
#define SSMICLASS_INCLUDE

#ifndef NCEPGRIDS
  #include "ncepgrids.h"
#endif
#ifndef POINTSH
  #include "points.h"
#endif

// ssmiclass begun 5/5/2004  Robert Grumbine
//   Purpose is to transition to having a class for ssmi processing,
//   rather than requiring C-only codes 

/* Maximum observed latitude = */
  #define MAX_LATITUDE  87.5
/* Define the characteristics of the data file structure */
  #define NORBITS          1
  #define NSCANS           64
/* Structures which relate to bufr decoding */
  typedef struct {
      int kwrit;
      float latitude, longitude;
      int sftg, posn;
      float t85v, t85h;
  } short_bufr;

  typedef struct {
      int scan_counter;
      float latitude, longitude;
      int surface_type;
      int position_num;
      float t19v, t19h, t22v, t37v, t37h, t85v, t85h;
      short_bufr hires[3];
  } bufr_point;

  typedef struct {
      int satno, year, month, day, hour, mins, secs, scan_no;
      bufr_point full[NSCANS];
  } bufr_line;
/* Declare BUFR function prototypes */
extern int process_bufr(bufr_line *b);
extern int check_bufr(bufr_line *b);
extern int check_short_bufr(short_bufr *b);
extern void zero_bufr(bufr_line *b, int i);


// That's the end of the prior ssmi.h file 
/* define field numbers */
#define T19V 0
#define T19H 1
#define T22V 2
#define T37V 3
#define T37H 4
#define T85V 5
#define T85H 6
#define CONC_BAR 7
#define BAR_CONC 8
#define COUNT    9
#define HIRES_CONC 10
#define WEATHER_COUNT 11

/* oldssmi is what was in use before approximately 18 November 1998 */
  typedef struct { unsigned int t19v : 16;
                   unsigned int t19h : 16;
                   unsigned int t22v : 16;
                   unsigned int t37v : 16;
                   unsigned int t37h : 16;
                   unsigned int t85v : 16;
                   unsigned int t85h : 16;
                   unsigned int conc_bar :  8;
                   unsigned int bar_conc :  8;
                 } oldssmi;

  typedef struct { unsigned int t19v : 16;
                   unsigned int t19h : 16;
                   unsigned int t22v : 16;
                   unsigned int t37v : 16;
                   unsigned int t37h : 16;
                   unsigned int t85v : 16;
                   unsigned int t85h : 16;
                   unsigned int conc_bar :  8;
                   unsigned int bar_conc :  8;
                   unsigned int count    :  8; /* Added 18 Nov 1998 */
                   unsigned int hires_conc : 8; /* Added 10 October 2001 */
                   unsigned int weather_count : 8; /* Added 30 April 2004 */
                   unsigned int old_conc      : 8; /* Added 23 Sep 2005 */
                 } ssmi;

  typedef struct { unsigned int t19v : 24;
                   unsigned int t19h : 24;
                   unsigned int t22v : 24;
                   unsigned int t37v : 24;
                   unsigned int t37h : 24;
                   unsigned int t85v : 24;
                   unsigned int t85h : 24;
                   unsigned int conc_bar  :  16;
                   unsigned int hires_bar :  16;
                   unsigned int count     :   8;
                   unsigned int weather_count : 8; /* Added 30 April 2004 */
                   unsigned int old_conc_bar  :  16; /* Added 23 Sep 2005 */
                 } ssmi_tmp;



class ssmipt {
  public:
    ssmi obs;
    ssmipt();
    operator double();
    ssmipt(ssmipt &);
    ssmipt & operator=(ssmipt &);
// Future: To add, after methods in ssmiclass.operations.h 
//    ssmipt & operator=(int );
//    point3<float> polarization();
//    point3<float> gradient();
//    point3<float> differences();
//    int qc();
//    void show();
};
ssmipt::ssmipt() {
  obs.t19v = 0;
  obs.t19h = 0;
  obs.t22v = 0;
  obs.t37v = 0;
  obs.t37h = 0;
  obs.t85v = 0;
  obs.t85h = 0;
  obs.conc_bar      = 0;
  obs.bar_conc      = 0;
  obs.count         = 0;
  obs.hires_conc    = 0;
  obs.weather_count = 0;
}
ssmipt::operator double() {
  return (double) obs.conc_bar;
}
ssmipt::ssmipt(ssmipt &x) {
  obs.t19v = x.obs.t19v;
  obs.t19h = x.obs.t19h;
  obs.t22v = x.obs.t22v;
  obs.t37v = x.obs.t37v;
  obs.t37h = x.obs.t37h;
  obs.t85v = x.obs.t85v;
  obs.t85h = x.obs.t85h;
  obs.conc_bar      = x.obs.conc_bar;
  obs.bar_conc      = x.obs.bar_conc;
  obs.count         = x.obs.count;
  obs.hires_conc    = x.obs.hires_conc;
  obs.weather_count = x.obs.weather_count;
}
ssmipt & ssmipt::operator=(ssmipt &x) {
  obs.t19v = x.obs.t19v;
  obs.t19h = x.obs.t19h;
  obs.t22v = x.obs.t22v;
  obs.t37v = x.obs.t37v;
  obs.t37h = x.obs.t37h;
  obs.t85v = x.obs.t85v;
  obs.t85h = x.obs.t85h;
  obs.conc_bar      = x.obs.conc_bar;
  obs.bar_conc      = x.obs.bar_conc;
  obs.count         = x.obs.count;
  obs.hires_conc    = x.obs.hires_conc;
  obs.weather_count = x.obs.weather_count;
  return *this;
}


#endif
