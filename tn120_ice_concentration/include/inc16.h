C     INCLUDE file for 1/16 mesh
      INTEGER grid, nhead, nfield
      PARAMETER (grid   = 1024)
      PARAMETER (nhead  =    1)
      PARAMETER (nfield =   14)

      REAL scale(nfield), low(nfield), high(nfield)
      INTEGER type(nfield)
      LOGICAL flip
      PARAMETER (flip = .FALSE.)
      
      DATA type / 2, 2, 1, 1, 1, 3, 2, 2, 2, 1, 
     1            3, 3, 3, 3/
      DATA scale / 64.,  64.,   1.,   1., 1., 
     1            100., 128., 128., 128., 1.,
     2             10.,  10.,  10.,  10. / 
      DATA low  / 100.0, 100.0,    0.0, 1970.0,    0.0,
     1              0.0, -99.0, -369.0,    0.0, -100.0,
     2              0.0,   0.0,    0.0,    0.0 /
      DATA high / 600.0, 600.0, 32768., 2010.0, 370.,
     1             24.0,  99.0,  369.0,  189.0, 6000.0,
     2            32768., 32768., 32768., 32768. /
