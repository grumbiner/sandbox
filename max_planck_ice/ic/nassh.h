#define NES_NX 316
#define NES_NY 332

      const float nes_dx     =  25.000E3;
      const float nes_dy     =  25.000E3;
      const float nes_polei  = 157.5; /* These values are shifted 1 point */
      const float nes_polej  = 165.5; /*  because of the change in range for
                                          array indices */
      
/*    Parameters for the remapping routines */
      const float nes_sgn    =   -1.0;
      const float nes_rearth = 6378.273E3;
      const float nes_eccen2 =    0.006693883;
      const float nes_slat   =   70.0;
      const float nes_slon   =  -45.0;
#define nes_xorig  (-nes_polei*nes_dx);
#define nes_yorig  (-nes_polej*nes_dy);
