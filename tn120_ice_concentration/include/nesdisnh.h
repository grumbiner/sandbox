/* Include file defining the NESDIS mastermap on 1/8th Bedient */
/* Bob Grumbine 5 April 1994 */

#define NES_NX 513
#define NES_NY 511

      const float nes_latmin =  81.25;
      const float nes_dx     =  47.625E3;
      const float nes_dy     =  47.625E3;
      const float nes_polei  = 257.;
      const float nes_polej  = 256.;
      
/*    Parameters for the remapping routines */
      const float nes_sgn    =    1.0;
      const float nes_rearth = 6378.273E3;
      const float nes_e2     =    0.006693883;
      const float nes_slat   =   60.0;
#define nes_xorig  (nes_polei*nes_dx);
#define nes_yorig  (nes_polej*nes_dy);

/*    Describing the data file */
      typedef struct {unsigned int v19    : 16;
                      unsigned int h19    : 16;
                      unsigned int v37    : 16;
                      unsigned int nasa   :  8;
                      unsigned int nesdis :  8;
      		     }                       ssmi;
