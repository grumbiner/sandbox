#define NX 56
#define NY 60

      const float latmin = -81.25;
      const float dx     = 127.E3;
      const float dy     = 127.E3;
      const float polei  =  28.;
      const float polej  =  26.;
      
/*    Parameters for the remapping routines */
      const float sgn    = -1.0;
      const float rearth = 6378.273E3;
      const float eccen2 =    0.006693883;
      const float slat   =   60.0;
      const float slon   =  170.0;

#define xorig (-(polei)*dx)
#define yorig (-(polej)*dy)
