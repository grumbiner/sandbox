/* Information for decoding the SDR orbit by orbit files */
/* Robert Grumbine 16 December 1994 */
/* Extraneous definitions removed 18 November 1998 */
/* -- extraneous due to changes in input data from SDR to BUFR */

#ifndef SSMI_INCLUDE

  #define SSMI_INCLUDE

/* Define the characteristics of the DMSP F-11 satellite orbit */
/* Orbit time = 1:41:57 */
/* Orbit inclination =  */
/* Swath width = 1600? */
/* Maximum observed latitude = */
  #define MAX_LATITUDE  87.5

/* Satellite Altitude = */
/* Antenna size = */
/* Exact operating frequencies = */

/* Define the characteristics of the data file structure */
  #define NORBITS          1
  #define RECS_PER_ORBIT 1725
  #define NSCANS           64
  #define REC_LENGTH     3348

/* Structures which relate to bufr decoding */
  typedef struct {
      float latitude, longitude, t85v, t85h;
  } short_bufr;
 
  typedef struct {
      int scan_counter;
      float latitude, longitude;
      int surface_type;
      int position_num;
      float t19v, t19h, t22v, t37v, t37h, t85v, t85h;
/*      short_bufr hires[3]; */
  } bufr_point;

  typedef struct {
      int satno, year, month, day, hour, mins, secs, scan_no;
      bufr_point full[NSCANS];
  } bufr_line;

/* Structures for SDR decoding */
  struct short_data {
    unsigned int latitude     : 16;
    unsigned int longitude    : 16;
    unsigned int t85v         : 16;
    unsigned int t85h         : 16;
    unsigned int surface_type :  8;
    unsigned int position_num :  8;
  } ;

  struct long_data {
    unsigned int scan_counter : 16;
    unsigned int latitude     : 16;
    unsigned int longitude    : 16;
    unsigned int t19v         : 16;
    unsigned int t19h         : 16;
    unsigned int t22v         : 16;
    unsigned int t37v         : 16;
    unsigned int t37h         : 16;
    unsigned int t85v         : 16;
    unsigned int t85h         : 16;
    unsigned int surface_type :  8;
    unsigned int position_num :  8;
    struct short_data short_rec[3];
  };
  
  struct data_block {
    unsigned int block_length : 16;
    unsigned int mode         :  8;
    unsigned int submode      :  8;
    struct long_data  full[NSCANS];
    unsigned int checksum     : 16;
  };
  
  struct scan_header {
    unsigned int block_length : 16;
    unsigned int mode         :  8;
    unsigned int submode      :  8;
    unsigned int scan_counter : 16;
    unsigned int b_scan_start : 32;
    unsigned int checksum     : 16;
    };
  
  struct data_record{
    struct scan_header header;
    struct data_block  data;
    };

/* Now lay out the header record */
/* The fields marked char[] are dummies for occupying space.
   data is not actually a character array */
  struct product_id { 
    char idrec[28];
  };

  struct data_sequence {
    char dseq[26];
  };


  struct scan_header_data {
    char dummy[34];
  };

/* Declare function prototypes */
int process_header(const char *buffer, char *date, int *nrecs);

int process_data(const char *buffer, struct data_record *a);

int process_long(const char *buffer, int *locate, struct long_data *b);
int check_long(struct long_data *b);

int process_short(const char *buffer, int *locate, struct short_data *c);
int check_short(struct short_data *c);

/* Declare BUFR function prototypes */
int process_bufr(bufr_line *b);
int process_short_bufr(short_bufr *c);
int check_bufr(bufr_line *b);
int check_short_bufr(short_bufr *b);
void zero_bufr(bufr_line *b, int i);

#endif
