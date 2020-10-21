/* Include file defining the northern hemisphere grid in sea ice model.  */
/* For remapping off the NESDIS mastermap grid file onto ice model grid. */
/* Bob Grumbine 5 April 1994 */

#define NX 76
#define NY 92
#define bgnh

      const float latmin = 81.25;
      const float dx     = 127.E3;
      const float dy     = 127.E3;
      const float polei  =  38.; /* originally 38 */
      const float polej  =  46.; /* originally 46 */
      
/*    Parameters for the remapping routines */
      const float sgn    = 1.0;
      const float rearth = 6378.273E3;
      const float e2     =    0.006693883;
      const float slat   =   60.0;
      const float slon   =  +10.0;

#define xorig (polei*dx)
#define yorig (polej*dy)
