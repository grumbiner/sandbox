      INTEGER FUNCTION degrib(nx, ny, sst)

      IMPLICIT none

      INTEGER nx, ny
      INTEGER lugb, lugbi, j, jf, jpds(25), jgds(22)
      INTEGER k, kf, kpds(25), kgds(22), iret
      LOGICAL lb(200000)
      REAL sst(nx*ny)
      INTEGER ind, ifld(12), ilevel(12)
      INTEGER i, index

C     Ifld is the IPDS descriptor for getting a desired field.

      ifld(1) = 011 !sst
      ilevel(1) = -1

      ind = 1

      LUGB=11
      LUGBI=0
      J=-1
      JF=nx*ny
      DO 1 i = 1, 25
        JPDS(i) =-1
   1  CONTINUE
      DO 2 i = 1, 22
        JGDS(i) =-1
   2  CONTINUE
      DO 3 i = 1, 200000
        lb(i) = .FALSE.
   3  CONTINUE
      DO 4 i = 1, nx*ny
        sst(i) = 0.0
   4  CONTINUE

      REWIND(LUGB)
      JPDS(5) = ifld(ind)
      JPDS(6) = ilevel(ind)
      JPDS(7) = -1
      JPDS(8) = -1
      JPDS(9) = -1
      JPDS(10) = -1

      CALL getgb(lugb, lugbi, jf, j, jpds, jgds, kf, k, kpds, kgds, 
     1             lb, sst, iret)
      degrib = iret

      RETURN
      END 
