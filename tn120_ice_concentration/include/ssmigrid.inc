C     Include file for working with NESDIS mastermap data files
C     Bob Grumbine 2 June 1994.
C     INCLUDE file for 1/8 mesh
      INTEGER grid, nhead, nfield
      PARAMETER (grid   = 512)
      PARAMETER (nhead  =   2)
      PARAMETER (nfield =  19)

      REAL scale(nfield), low(nfield), high(nfield)
      INTEGER type(nfield)
      LOGICAL flip
      PARAMETER (flip = .FALSE.)

      DATA type / 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 
     1            3, 2, 2, 2, 1, 3, 3, 3, 1/
      DATA scale /  64.0,  64.0,  64.0,  64.0, 64.0, 
     1              64.0,  64.0,   1.0,   1.0,  1.0,
     1             100.0, 128.0, 128.0, 128.0,  1.0,
     1              10.0,  10.0,  10.0,   1.0 /
      DATA low  /  0.0,   0.0,    0.0,    0.0,    0.0,
     1             0.0,   0.0,    0.0, 1970.0,    0.0,
     1             0.0, -99.0, -190.0,   -1.0, -100.0,
     1             0.0,   0.0,    0.0,    0.0 /
      DATA high /   600.,   600.,   600.,   600.,   600.,
     1              600.,   600., 32768.,  2010.,   370.,
     2               24.,    99.,   369.,   189.,  6000.,
     3            32768., 32768., 32768., 32768. /

