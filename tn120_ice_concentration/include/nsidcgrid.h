#ifndef ICEGRIDS
  #define ICEGRIDS

  #define NX_NORTH    304
  #define NY_NORTH    448
  #define polei_NORTH (3850./25.)
  #define polej_NORTH (5850./25.)
  #define NX_SOUTH    316
  #define NY_SOUTH    332
  #define polei_SOUTH (3950./25.)
  #define polej_SOUTH (4350./25.)
  
  #define sgn_NORTH      1.0
  #define slat_NORTH    70.0
  #define slon_NORTH  (-45.0 )
  #define xorig_NORTH (-polei_NORTH*dx)
  #define yorig_NORTH ( polej_NORTH*dy) 

  #define sgn_SOUTH   (-1.0)
  #define slat_SOUTH    70.0
  #define slon_SOUTH  ( 90.0 )
  #define xorig_SOUTH (-polei_SOUTH*dx)
  #define yorig_SOUTH ( polej_SOUTH*dy) 

  #define dx  (25.0e3)
  #define dy  (25.0e3)
  
/* error found 13 September 1995  #define rearth  6738.273e3 */
  #define rearth  6378.273e3
  #define eccen2     0.006693883

#endif
