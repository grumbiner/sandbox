      PROGRAM toanse
      IMPLICIT none

!Read in the transposed file (spec'd by i,j) and the iers file
!  average the meteorological file 4:1
!  write out with julian days of the iers file

      INTEGER nx, ny, nt
      PARAMETER (nx = 192)
      PARAMETER (ny =  94)
      PARAMETER (nt = 68668)
      REAL y(nt), r(nt)
      DOUBLE PRECISION dum, sumy, sumy2, sumr, sumr2

      INTEGER i, j, k, l, m
      CHARACTER*80 fname

      INTEGER start_date 
      REAL jd, xiers, yiers, utc, lod

!      READ (*,*) l, m
!      WRITE(fname,9002) l, m
! 9002 FORMAT("trans.",I3.3,".",I2.2)
      READ (*,*) fname
!      PRINT *,fname
      sumy = 0.D0
      sumy2 = 0.D0

      OPEN(11, FILE=fname, FORM="FORMATTED", STATUS="OLD")
      OPEN(12, FILE="iers.reform", FORM="FORMATTED", STATUS="OLD")
      OPEN(13, FILE="rsplice", FORM="FORMATTED", STATUS="OLD")
      DO i = 1, nt
        READ(13,*) r(i)
        READ(11,*) dum, y(i)
      ENDDO

      start_date = 37665
      DO k = 1, nt, 4
        sumy = 0
        sumr = 0
        DO i = 0, 3
          sumy = sumy + y(k+i)
          sumr = sumr + r(k+i)
        ENDDO
        READ (12,*) jd, xiers, yiers, utc, lod
        WRITE(*,9001) jd, sumy/4., sumr/4., xiers, yiers, 
     1                sqrt(xiers**2 + yiers**2) ,lod
      ENDDO

 9001 FORMAT (F7.1,"	",E13.6, "	",E13.6, "	", 4(F11.6,"	") )

      END
