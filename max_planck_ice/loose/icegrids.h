#ifndef ICEGRIDS
  #define ICEGRIDS

  #define INFLATE 5

  #define NORTH_NX    (77*INFLATE)
  #define NORTH_NY    (93*INFLATE)
  #define NORTH_polei (38.*INFLATE)
  #define NORTH_polej (46.*INFLATE)
  #define SOUTH_NX    (69*INFLATE)
  #define SOUTH_NY    (71*INFLATE)
  #define SOUTH_polei (30.*INFLATE)
  #define SOUTH_polej (36.*INFLATE)
  
  #define NORTH_sgn      1.0
  #define NORTH_slat    60.0
  #define NORTH_slon  (-10.0)
  #define NORTH_xorig (-NORTH_polei*NORTH_dx)
  #define NORTH_yorig (-NORTH_polej*NORTH_dy)

  #define SOUTH_sgn   (-1.0)
  #define SOUTH_slat    60.0
  #define SOUTH_slon   170.0
  #define SOUTH_xorig (-SOUTH_polei*NORTH_dx)
  #define SOUTH_yorig (-SOUTH_polej*NORTH_dy)

  const float NORTH_dx = (127.0e3/INFLATE);
  const float NORTH_dy = (127.0e3/INFLATE);
  const float SOUTH_dx = (127.0e3/INFLATE);
  const float SOUTH_dy = (127.0e3/INFLATE);
  
  const float NORTH_rearth = 6738.273e3;
  const float NORTH_eccen2 =    0.006693883;
  const float SOUTH_rearth = 6738.273e3;
  const float SOUTH_eccen2 =    0.006693883;

#endif
