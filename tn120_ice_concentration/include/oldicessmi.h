#define OPERATIONS
/* Specifications for computing with the SSMI orbital data */
/* Robert Grumbine 10 Feb 1995 */

#ifndef ICESSMI

  #define ICESSMI

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

  typedef struct { oldssmi;
                   unsigned int count    :  8; /* Added 18 Nov 1998 */
                 } ssmi;

  typedef struct { unsigned int t19v : 24;
                   unsigned int t19h : 24;
                   unsigned int t22v : 24;
                   unsigned int t37v : 24;
                   unsigned int t37h : 24;
                   unsigned int t85v : 24;
                   unsigned int t85h : 24;
                   unsigned int conc_bar :  16;
                   unsigned int count    :   8;
                 } ssmi_tmp;

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

/* Define special values for concentrations */
#define LAND     157
#define COAST    195
#define BAD_DATA 166
#define WEATHER  177
#define NO_DATA  224
#define MIN_CONC  15

/* Define terms for handling transfer from SSMI file to ice grid */
#define DATA_WINDOW 12

#endif
