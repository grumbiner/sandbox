      SUBROUTINE SK2SETUP(x, y, x0, y0, dx, dy, skpt, npts, nnpts,
     1     minsep)
C     Assign initial positions to the virtual floes.
C     Bob Grumbine 4 April 1994.

      IMPLICIT none
      INCLUDE "sicedrft.inc"

      INTEGER npts, nnpts
      REAL x(npts), y(npts), x0(npts), y0(npts), dx(npts), dy(npts)
      INTEGER skpt(npts)
      INTEGER k

C     Variables for iceline reads
      REAL xo, yo, xi(maxpts), yi(maxpts)
      CHARACTER(1) ew(maxpts)
      CHARACTER(60) header
      INTEGER i, j, nuse, nuses
      REAL arcdis, minsep

C     Bullet-proofing variables
      REAL rlat1, rlat2
      PARAMETER (rlat1 = lat1)
      PARAMETER (rlat2 = lat2)
      REAL dist

C     Bullet-proofing code: verify that the lats and longs are
C       set up correctly.
      IF (lat2 .LT. lat1) THEN
        PRINT *,'Latitudes are reversed.  FATAL error'
        STOP
      ENDIF
      IF (long2 .LT. long1) THEN
        PRINT *,'Longitudes are reversed. FATAL error'
        STOP
      ENDIF
      IF (lat1 .LT. -90.) THEN
        PRINT *,'Southern boundary off the planet. FATAL error'
        STOP
      ENDIF
      IF (lat2 .GT. 90.) THEN
        PRINT *,'Northern boundary off the planet. FATAL error'
        STOP
      ENDIF
      IF (ABS(long1) .GT. 360.) THEN
        PRINT *,'Base longitude out of range. FATAL error'
        STOP
      ENDIF
      IF (ABS(long2) .GT. 360.) THEN
        PRINT *,'Eastern longitude out of range. FATAL error'
        STOP
      ENDIF

C     Operational Code
      k     = 0
      nnpts = 0
 1000 CONTINUE
        k = k + 1
        READ (47,*,END=1010,ERR=1020) skpt(k), y(k), x(k)
        GO TO 1000
 1010 CONTINUE
      nnpts = k-1

 1020 CONTINUE
      IF (nnpts .LE. 1) THEN
C       Use a default table setup.

        PRINT *,'Using the default skiles points table'
        nnpts = 18
        skpt(1) = 77
        skpt(2) = 78
        skpt(3) = 93
        skpt(4) = 94
        skpt(5) = 95
        skpt(6) = 96
        skpt(7) = 111
        skpt(8) = 112
        skpt(9) = 113
        skpt(10) = 114
        skpt(11) = 130
        skpt(12) = 131
        skpt(13) = 132
        skpt(14) = 147
        skpt(15) = 148
        skpt(16) = 161
        skpt(17) = 162
        skpt(18) = 174

        y(1) = 54.29596
        y(2) = 57.63493
        y(3) = 54.46198
        y(4) = 57.82261
        y(5) = 61.24105
        y(6) = 64.71256
        y(7) = 54.29596
        y(8) = 57.63493
        y(9) = 61.02662
        y(10) = 64.46420
        y(11) = 53.80368
        y(12) = 57.07969
        y(13) = 60.39427
        y(14) = 56.17905
        y(15) = 59.37450
        y(16) = 54.96604
        y(17) = 58.01163
        y(18) = 53.48018

        x(1) = 195.7106
        x(2) = 196.3402
        x(3) = 190.0
        x(4) = 190.0
        x(5) = 190.0
        x(6) = 190.0
        x(7) = 184.2894
        x(8) = 183.6598
        x(9) = 182.8750
        x(10) = 181.8699
        x(11) = 178.6901
        x(12) = 177.4712
        x(13) = 175.9637
        x(14) = 171.5650
        x(15) = 169.4439
        x(16) = 166.0375
        x(17) = 163.4350
        x(18) = 160.9454

      ENDIF

C     7 March 1997  Add point in the Cook Inlet for use by Anchorage 
C       WSFO.  Robert Grumbine
      yi(1) = 360. - 152.
      xi(1) = 60.0

C     i starts at 2 because of Cook Inlet pt.
C     Now read in ice lines for extra use.
C     Original (1997) was reading a NIC file which was dropped from
C       their operations in 1999.  A fixed file was used in NCEP since
C       then.  New implementation 2011 to read a locally-supported file.
C     3 Nov 2011 Robert Grumbine
C     Note that at this point, there is no antarctic edge 2011 
      i = 2
      OPEN(48,STATUS="OLD", ERR=9990)

      READ (48, 9002) header
      PRINT *,'header north ',header
      PRINT *,'will apply minimum separation of ',minsep
 9002 FORMAT (A60)
 2000 CONTINUE
        READ (48, *, ERR=2009, END=2010) xi(i), yi(i)
CD        WRITE (*,*) "ice edge pt",i,xi(i), yi(i)
        i = i + 1
        GO TO 2000
 2009 CONTINUE
      PRINT *,'Reading error, in north ice line, i = ',i
 2010 CONTINUE

 3000 CONTINUE
      nuses = i - 1


C     Now go through ice line point list and transfer those which are
C       more than 1/2 degree apart to the virtual floe list.
C     Changed 3 November 2011 to being dlat*111 km apart
      x(nnpts+1) = yi(1)
      y(nnpts+1) = xi(1)
      skpt(nnpts+1) = nnpts+1
      xo         = xi(1)
      yo         = yi(1)
      j = nnpts+2
      DO i = 2, nuses
        dist = arcdis(yo, xo, yi(i), xi(i) )
CD        PRINT *,"dist = ",dist

        IF ( arcdis(yo, xo, yi(i), xi(i) ) .GT. minsep  .AND.
     1        yi(i) .NE. 0.0 .AND. xi(i) .NE. 0.0 ) THEN
          yo = yi(i)
          xo = xi(i)
          x(j) = yi(i)
          y(j) = xi(i)
          skpt(j) = j
          j = j + 1
         ELSE
        ENDIF
      END DO
      nnpts = j - 1
      PRINT *,'There are ',nnpts,' points in the aggregate file.'

C     Set up the initial location and drift positions.
      DO 4000 k = 1, nnpts
        x0(k) = x(k)
        y0(k) = y(k)
        dx(k) = 0.0
        dy(k) = 0.0
 4000 CONTINUE

      RETURN

C Error on trying to open the ice edge file
 9990 PRINT *,'Running without an ice edge file'
      GO TO 3000

      RETURN
      END
