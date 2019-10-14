/* Information for decoding the SDR orbit by orbit files */
/* Robert Grumbine 16 December 1994 */
/* Extraneous definitions removed 18 November 1998 */
/* -- extraneous due to changes in input data from SDR to BUFR */
/* New variant: Full resolution 85 GHz from BUFR */

#ifndef SSMI_INCLUDE

  #define SSMI_INCLUDE

/* Define the characteristics of the DMSP F-11 satellite orbit */
/* 15 August 2000: Note that it is F-13 in use by this point, but
   the orbit is nearly the same */
/* Orbit time = 1:41:57 */
/* Orbit inclination =  */
/* Swath width = 1600? */
/* Maximum observed latitude = */
  #define MAX_LATITUDE  87.5

/* Satellite Altitude = */
/* Antenna size = */
/* Exact operating frequencies = */

/* Define the characteristics of the data file structure */

  #define NSCANS           64
  #define REC_LENGTH     3348

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

/* Declare (sdr?) function prototypes */
int process_header(const char *buffer, char *date, int *nrecs);
int process_data(const char *buffer, struct data_record *a);


/* Declare BUFR function prototypes */
int process_bufr(bufr_line *b);
int process_short_bufr(short_bufr *c);
int check_bufr(bufr_line *b);
int check_short_bufr(short_bufr *b);
void zero_bufr(bufr_line *b, int i);

#endif
