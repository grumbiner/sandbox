      BLOCK DATA
C=======================================================================
C  Portable pseudo-random integer generator, especially for
C  microcomputers with a limitation of 16 bit integers. Translated from
C  original Pascal version to Fortran 77.
C
C   The supporting paper is:
C   (1) B. Wichmann & D. Hill, "Building a Random-Number Generator",
C             BYTE, March, 1987, 12(3):127-128.
C
C   Also see the following related works:
C   (2) Wichmann, B.A. and Hill, I.D., "An Efficient and Portable",
C             Pseudo-random Number Generator", Applied Statistics,
C             Volume 31, 1982, pages 188-190.
C   (3) Haas, Alexander, "The Multiple Prime Random Number Generator",
C             ACM Transactions on Mathematical Software; December,
C             1987, 13(4):368-381.
C   (4) L'Ecuyer, Pierre, "Efficient and Portable Combined Random Number
C             Generators", Communications of the ACM; June, 1988,
C             31(6):742-749,774.
C   (5) L'Ecuyer, Pierre and Serge Cote, "Implementing a Random Number
C             Package with Splitting Facilities", ACM Transactions on
C             Mathematical Software, March 1991, 17(1):98-111.
C
C Use...
C      CALL RAND(U,N)
C          To generate a sequence, U, of N Uniform(0,1) numbers.
C          Cycle length is ((30269-1)*(30307-1)*(30323-1))/4  or
C          6953607871644  > 6.95E+12.
C
C     To access the SEED vector in the calling program use statements:
C     INTEGER SEED(3)
C     COMMON/RANDOM/SEED
C
C  The common variable SEED is the array of three current seeds.
      INTEGER SEED(3)
      COMMON/RANDOM/SEED
      DATA SEED(1),SEED(2),SEED(3)/1,10000,3000/
      END
