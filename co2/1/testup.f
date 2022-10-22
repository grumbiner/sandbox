
      PROGRAM testup

C*************************************************----------++++++++++!!
      IMPLICIT none

      INTEGER nlayer, chemu, region
      PARAMETER (region = 3)
      PARAMETER (chemu = 4)
      PARAMETER (nlayer = 76)
      DOUBLE PRECISION deltaz
      DOUBLE PRECISION a, b, c

      DOUBLE PRECISION temper(region), wind(region)
      DOUBLE PRECISION faa(nlayer, region), aamix
      DOUBLE PRECISION w(nlayer, region)
      DOUBLE PRECISION fna(nlayer, region), fiw(nlayer, region)
      DOUBLE PRECISION csdept, ishelf, time
      LOGICAL forced

      INCLUDE "chmptr.inc"

      INTEGER ntimes
      PARAMETER (ntimes = 20)
      REAL depth(ntimes), shelf(ntimes), dates(ntimes)
      REAL refdep
      PARAMETER (refdep = 3750.0)

      INTEGER i, j

      REAL area(3), rho(3)

      INCLUDE "arrho.inc"

      DATA dates(1)  /-20700./
      DATA dates(2)  /-19550./
      DATA dates(3)  /-18400./
      DATA dates(4)  /-17250./
      DATA dates(5)  /-16100./
      DATA dates(6)  /-14950./
      DATA dates(7)  /-13800./
      DATA dates(8)  /-12650./
      DATA dates(9)  /-11500./
      DATA dates(10) /-10350./
      DATA dates(11) /- 9200./
      DATA dates(12) /- 8050./
      DATA dates(13) /- 6900./
      DATA dates(14) /- 5750./
      DATA dates(15) /- 4600./
      DATA dates(16) /- 3450./
      DATA dates(17) /- 2300./
      DATA dates(18) /- 1150./
      DATA dates(19) /-    0./
      DATA dates(20) /- 1000./

      DATA depth(1)  /-120./
      DATA depth(2)  /-120./
      DATA depth(3)  /-119./
      DATA depth(4)  /-116./
      DATA depth(5)  /-112./
      DATA depth(6)  /-107./
      DATA depth(7)  /-102./
      DATA depth(8)  /- 89./
      DATA depth(9)  /- 69./
      DATA depth(10) /- 60./
      DATA depth(11) /- 40./
      DATA depth(12) /- 25./
      DATA depth(13) /- 16./
      DATA depth(14) /- 11./
      DATA depth(15) /-  7./
      DATA depth(16) /-  6./
      DATA depth(17) /-  5./
      DATA depth(18) /-  4./
      DATA depth(19) /-  4./
      DATA depth(20) /-  0./

C*************************************************----------++++++++++!!

      forced = .TRUE.
      deltaz = 50.0
      time = 0.0
      PRINT *,'Calling aabw'
      CALL aabw(temper, wind, csdept, ishelf, time, faa, aamix,
     1            fna, fiw,
     1            forced, deltaz, nlayer, region)

      PRINT *,'Calling upwell'
      CALL upwell(fna, faa, fiw, deltaz, nlayer, w, region)

      a = 0.0
      b = 0.0
      c = 0.0
      j = 3
      DO 1000 i = 1, nlayer
        a = a+DABS(faa(i,j))
        b = b+DABS(fna(i,j))
        c = c+DABS(fiw(i,j))
        WRITE (*,9001)  i, faa(i,j), fna(i,j),
     1                     fiw(i,j), w(i,j)*area(j)
 1000 CONTINUE
      PRINT *, a, b, c

 9001 FORMAT (I3, 4E14.6)

      END