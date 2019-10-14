/* Information for decoding the SDR orbit by orbit files */
/* Bob Grumbine 16 December 1994 */

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

  struct rev_header_data {
    char dummy[20];
    unsigned int orbit_num : 16;
    char dummy2[10];
    unsigned int julian_begin : 16;
    char dummy3[10];
    unsigned int hour_begin : 16;
    char dummy4[10];
    unsigned int minute_begin : 16;
    char dummy5[10];
    unsigned int second_begin : 16;
    char dummy6[10];
    unsigned int julian_end : 16;
    char dummy7[10];
    unsigned int hour_end : 16;
    char dummy8[10];
    unsigned int minute_end : 16; 
    char dummy9[10]; 
    unsigned int second_end : 16;
    char dummy10[10];
    unsigned int ascend_day : 16;
    char dummy11[10];
    unsigned int ascend_hour : 16;
    char dummy12[10];
    unsigned int ascend_minute : 16;
    char dummy13[10];
    unsigned int ascend_sec    : 16;
    char dummy14[24];
  }; 

  struct scan_header_data {
    char dummy[34];
  };

  struct sdr_data_des {
    char dummy[370];
  };

  struct rev_header_data2 {
    unsigned int block_len : 16;
    unsigned int block_id  : 16;
    unsigned int spacecraft : 32;
    unsigned int revolution : 32;
    unsigned int julian_begin : 16;
    unsigned int hour_begin : 8;
    unsigned int min_begin  : 8;
    unsigned int sec_begin  : 8;
    unsigned int julian_end : 16;
    unsigned int hour_end   : 8;
    unsigned int min_end    : 8;
    unsigned int sec_end    : 8;
    unsigned int julian_ascend : 16;
    unsigned int hour_ascend : 8;
    unsigned int min_ascend  : 8;
    unsigned int sec_ascend  : 8;
    unsigned int satid       : 8;
    unsigned int checksum    : 16;
  }; 

  struct sdr_scan_header {
    struct product_id       id;
    struct data_sequence    dseq;
    struct rev_header_data  rev_header;
    struct scan_header_data sheader;
    struct sdr_data_des     sdrdata;
    struct rev_header_data2 rev_header2;
  };
    
/* Declare function prototypes */
int process_header(const char *buffer, char *date);

int process_data(const char *buffer, struct data_record *a);

int process_long(const char *buffer, int *locate, struct long_data *b);
int check_long(struct long_data *b);

int process_short(const char *buffer, int *locate, struct short_data *c);
int check_short(struct short_data *c);

#endif
