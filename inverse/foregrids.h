#ifndef FOREGRIDS
  #define FOREGRIDS

  #define DIVISOR 1

  #define FORE_NORTH_NX    (385/DIVISOR)
  #define FORE_NORTH_NY    (465/DIVISOR)
  #define FORE_NORTH_polei (190/DIVISOR)
  #define FORE_NORTH_polej (230/DIVISOR)
  #define FORE_SOUTH_NX    (345/DIVISOR)
  #define FORE_SOUTH_NY    (355/DIVISOR)
  #define FORE_SOUTH_polei (150/DIVISOR)
  #define FORE_SOUTH_polej (180/DIVISOR)
  
  #define FORE_NORTH_sgn      1.0
  #define FORE_NORTH_slat    60.0
  #define FORE_NORTH_slon  (-10.0)
  #define FORE_NORTH_xorig (-FORE_NORTH_polei*FORE_NORTH_dx)
  #define FORE_NORTH_yorig (-FORE_NORTH_polej*FORE_NORTH_dy)

  #define FORE_SOUTH_sgn   (-1.0)
  #define FORE_SOUTH_slat    60.0
  #define FORE_SOUTH_slon   170.0
  #define FORE_SOUTH_xorig (-FORE_SOUTH_polei*FORE_SOUTH_dx)
  #define FORE_SOUTH_yorig (-FORE_SOUTH_polej*FORE_SOUTH_dy)

  const float FORE_NORTH_dx = (25.4e3*DIVISOR);
  const float FORE_NORTH_dy = (25.4e3*DIVISOR);
  const float FORE_SOUTH_dx = (25.4e3*DIVISOR);
  const float FORE_SOUTH_dy = (25.4e3*DIVISOR);
  
  const float FORE_NORTH_rearth = 6378.273e3;
  const float FORE_NORTH_eccen2 =    0.006693883;
  const float FORE_SOUTH_rearth = 6378.273e3;
  const float FORE_SOUTH_eccen2 =    0.006693883;

  const float MAX_FREEZE = -35.0;
  const float MAX_GROW_FREEZE = -20.0;

#endif
/* Last Modified 2 January 1996 */
