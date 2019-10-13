//Include file for various NESDIS-defined quantities.
//Robert Grumbine
//23 June 1999

#ifndef NESDISH
  #define NESDISH
////////////////////////////////////////////////////////
//For working on the NESDIS 1/16th degree land mask file

#define RECLAND    -1
#define RECSEA      0
#define NESLAND 1
#define NESSEA  0
#define PER_DEGREE 16.0

typedef struct {
  float latspac;
  float lonspac;
  float tagres; /* km */
  short recno[648];
  char spare[1892];
} dirrec ;
typedef struct {
  unsigned char chars[3200];
} block;

// Definitions of functions
extern int unblock(short int *unblocked, block *tag);
extern int blocksea( short int *unblocked);
extern int blockland( short int *unblocked);

#endif
