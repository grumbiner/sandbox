#define NSIDC_NORTH_NX 304
#define NSIDC_NORTH_NY 448

      const float nsidc_NORTH_dx     =  25.000E3;
      const float nsidc_NORTH_dy     =  25.000E3;
      const float nsidc_NORTH_polei  = 153.5; /* These values are shifted 1 */
      const float nsidc_NORTH_polej  = 213.5; /*  point because of the change */
                                              /*  in range for array indices */
#define nsidc_NORTH_xorig  (-nsidc_NORTH_polei*nsidc_NORTH_dx)
#define nsidc_NORTH_yorig  (-nsidc_NORTH_polej*nsidc_NORTH_dy)
      
/*    Parameters for the remapping routines */
      const float nsidc_NORTH_sgn    =    1.0;
      const float nsidc_NORTH_rearth = 6378.273E3;
      const float nsidc_NORTH_eccen2 =    0.006693883;
      const float nsidc_NORTH_slat   =   70.0;
      const float nsidc_NORTH_slon   =  -45.0;

#define NSIDC_SOUTH_NX 316
#define NSIDC_SOUTH_NY 332
      const float nsidc_SOUTH_dx     =  25.000E3;
      const float nsidc_SOUTH_dy     =  25.000E3;
      const float nsidc_SOUTH_polei  = 157.5; /* These values are shifted 1 */
      const float nsidc_SOUTH_polej  = 165.5; /*  point because of the change */
                                              /*  in range for array indices */
#define nsidc_SOUTH_xorig  (-nsidc_SOUTH_polei*nsidc_SOUTH_dx)
#define nsidc_SOUTH_yorig  (-nsidc_SOUTH_polej*nsidc_SOUTH_dy)
      
/*    Parameters for the remapping routines */
      const float nsidc_SOUTH_sgn    =   -1.0;
      const float nsidc_SOUTH_rearth = 6378.273E3;
      const float nsidc_SOUTH_eccen2 =    0.006693883;
      const float nsidc_SOUTH_slat   =   70.0;
      const float nsidc_SOUTH_slon   =   90.0;
