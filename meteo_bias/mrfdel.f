      PROGRAM delta
C     Compute statistics from a gaussian trio of experiments.
      IMPLICIT none

      INCLUDE "icegrid.inc"

      INTEGER nlong, nlat
      INTEGER nwave
      REAL dlatm, dlonm
      PARAMETER (dlatm = 1.0)
      PARAMETER (dlonm = 1.0)
      PARAMETER (nlong = 360./dlonm)
      PARAMETER (nlat = 180./dlatm + 1.)
      PARAMETER (nwave = 62)

      INTEGER idim, jdim
      PARAMETER (idim = 3*nwave+6)
      PARAMETER (jdim = (3*nwave+2)/2 )

      REAL f1(idim*jdim), ll1(nlong, nlat), nh1(LP, MP)
      REAL f2(idim*jdim), ll2(nlong, nlat), nh2(LP, MP)
      REAL f3(idim*jdim), ll3(nlong, nlat), nh3(LP, MP)

      REAL sum1(nlat), sum2(nlat), sum3(nlat)
      REAL sumsq1(nlat), sumsq2(nlat), sumsq3(nlat)
      REAL sigma1, sigma2, sigma3
      INTEGER c1(nlat), c2(nlat), c3(nlat)
      INTEGER pole, i, j

      IF (latmin .GE. 0) THEN
        pole = 1
      ELSE
        pole = 2
      ENDIF

      OPEN (10, FILE="fort.10", FORM="UNFORMATTED", STATUS="OLD")
      READ (10) f1 !read in gaussian field
      CALL ffld(ll1, f1, idim) ! convert to lat-long
      CALL terp(ll1, nh1, pole, .FALSE.) !convert from lat-long to polar stereo

      OPEN (11, FILE="fort.11", FORM="UNFORMATTED", STATUS="OLD")
      READ (11) f2 !read in gaussian field
      CALL ffld(ll2, f2, idim) ! convert to lat-long
      CALL terp(ll2, nh2, pole, .FALSE.) !convert from lat-long to polar stereo

      OPEN (12, FILE="fort.12", FORM="UNFORMATTED", STATUS="OLD")
      READ (12) f3 !read in gaussian field
      CALL ffld(ll3, f3, idim) ! convert to lat-long
      CALL terp(ll3, nh3, pole, .FALSE.) !convert from lat-long to polar stereo

      DO 1000 j = 1, nlat
      DO 1000 i = 1, nlong
        ll2(i,j) = ll2(i,j) - ll1(i,j)
        ll3(i,j) = ll3(i,j) - ll1(i,j)
 1000 CONTINUE

      DO 2000 j = 1, MP
      DO 2000 i = 1, LP
        nh2(i,j) = nh2(i,j) - nh1(i,j)
        nh3(i,j) = nh3(i,j) - nh1(i,j)
 2000 CONTINUE

      WRITE (20) ll1
      WRITE (21) nh1
      WRITE (22) ll2
      WRITE (23) nh2
      WRITE (24) ll3
      WRITE (25) nh3

      DO 3000 j = 1, nlat
        sum1(j) = 0.
        sum2(j) = 0.
        sum3(j) = 0.
        c1(j) = 0
        c2(j) = 0
        c3(j) = 0
        DO 3100 i = 1, nlong
          IF (ABS(ll1(i,j)) .LT. 1E10) THEN 
               sum1(j) = sum1(j) + ll1(i,j)
               c1(j) = c1(j) + 1
          ENDIF
          IF (ABS(ll2(i,j)) .LT. 1E10) THEN 
               sum2(j) = sum2(j) + ll2(i,j)
               c2(j) = c2(j) + 1
          ENDIF
          IF (ABS(ll3(i,j)) .LT. 1E10) THEN 
               sum3(j) = sum3(j) + ll3(i,j)
               c3(j) = c3(j) + 1
          ENDIF
 3100   CONTINUE
CD        PRINT *,c1(j),c2(j),c3(j)
 3000 CONTINUE

      DO 4000 j = 2, nlat-1
        sumsq1(j) = 0.
        sumsq2(j) = 0.
        sumsq3(j) = 0.
        DO 4100 i = 1, nlong
          IF (ABS(ll1(i,j)) .LT. 1E10) 
     1         sumsq1(j) = sumsq1(j) + ll1(i,j)*ll1(i,j)
          IF (ABS(ll2(i,j)) .LT. 1E10) 
     1         sumsq2(j) = sumsq2(j) + ll2(i,j)*ll2(i,j)
          IF (ABS(ll3(i,j)) .LT. 1E10) 
     1         sumsq3(j) = sumsq3(j) + ll3(i,j)*ll3(i,j)
 4100   CONTINUE
        IF (c1(j) .GT. 1) 
     1   sigma1 = (sumsq1(j) - sum1(j)**2/FLOAT(c1(j)) )/FLOAT(c1(j)-1) 
        IF (c2(j) .GT. 1) 
     1   sigma2 = (sumsq2(j) - sum2(j)**2/FLOAT(c2(j)) )/FLOAT(c2(j)-1) 
        IF (c3(j) .GT. 1) 
     1   sigma3 = (sumsq3(j) - sum3(j)**2/FLOAT(c3(j)) )/FLOAT(c3(j)-1) 
        sigma1 = SQRT(sigma1)
        sigma2 = SQRT(sigma2)
        sigma3 = SQRT(sigma3)
        IF (sigma2 .NE. 0. .AND. sigma3 .NE. 0. 
     1      .AND. c1(j)*c2(j)*c3(j) .NE. 0) THEN
        WRITE (*,9001) 91-j,
     1     sum1(j)/c1(j), sum2(j)/c2(j), sum3(j)/c3(j),
     1               sigma1, sigma2, sigma3, 
     2               (sum2(j)/c2(j) * SQRT(c2(j)) / sigma2),
     3               (sum3(j)/c3(j) * SQRT(c3(j)) / sigma3)
        ELSE IF (c1(j)*c2(j)*c3(j) .NE. 0) THEN
        WRITE (*,9001) 91-j,
     1     sum1(j)/c1(j),sum2(j)/c2(j),sum3(j)/c3(j),
     1               sigma1, sigma2, sigma3 
        ENDIF
 4000 CONTINUE

 9001 FORMAT (I4,2x,F9.2,2F8.2,2x,F9.2,2F8.2,2x,2F8.4)


      END
