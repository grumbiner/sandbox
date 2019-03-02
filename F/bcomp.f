      PROGRAM bcomp
C     Compare buoy temperatures and pressures with mrf forecasts.
      IMPLICIT none

      INTEGER nfield, L, M
      PARAMETER (nfield = 34)
      PARAMETER (L = 68)
      PARAMETER (M = 76)
      REAL polei, polej, dx, deg, rads
      PARAMETER (polei = 37.)
      PARAMETER (polej = 44.)
      PARAMETER (dx    = 127.0)
      PARAMETER (deg   = 111.111)
C     Forecast declarations
      REAL for00(0:L,0:M,nfield), for12(0:L,0:M,nfield)
      REAL in(0:L,0:M)

C     Buoy declarations
      INTEGER nbmax
      PARAMETER (nbmax = 220000)
      CHARACTER*7 id(nbmax)
      INTEGER nbuoy, date(nbmax)
      REAL lat(nbmax), long(nbmax), p(nbmax), t(nbmax)
      INTEGER day(nbmax), hour(nbmax)
C     note that p = 0 is a bad value.

C     Local
      CHARACTER*60 fname
      INTEGER i, j, vday, ipt, jpt
      REAL r

C     Read in the forecast data
      DO 1000 j = 1, nfield
        READ (10) in
        CALL set(in, for00, j)
 1000 CONTINUE
      DO 1100 j = 1, nfield
        READ (11) in
        CALL set(in, for12, j)
 1100 CONTINUE

C     Now get the buoy data
      j = 0    
 9002 FORMAT (A7,I9,F5.1,F6.1,F7.1,F6.1)
 1200 CONTINUE
        j = j + 1
        READ (12, 9002, END=1300) id(j), date(j), lat(j), long(j),
     1                        p(j), t(j)
        GO TO 1200
 1300 CONTINUE
      nbuoy = j - 1

C     Convert buoy longitudes and split dates
      DO 2000 j = 1, nbuoy
        long(j) = 360. - long(j)
        day(j) = date(j)/100
        hour(j) = MOD(date(j),100)
 2000 CONTINUE

      rads = 4.*ATAN(1.)/180. 
C     Find out the date we're trying to verify
      READ (*,*) vday
      DO 3000 j = 1, nbuoy
        IF (day(j) .EQ. vday-1 .AND. hour(j) .GE. 21  .OR.
     1      day(j) .EQ. vday   .AND. hour(j) .LE.  3      ) THEN
         r = deg*(90.-lat(j))/dx
         ipt = INT(polei+r*COS(long(j)*rads) + 0.5 )
         jpt = INT(polej+r*SIN(long(j)*rads) + 0.5 )
         IF (ipt .LT. 0 .OR. ipt .GT. L) GO TO 3000
         IF (jpt .LT. 0 .OR. jpt .GT. M) GO TO 3000
         IF (for00(ipt,jpt,29) .LE. 1.0) GO TO 3000
         WRITE (13,9003) id(j), lat(j), long(j), date(j),
     1    for00(ipt,jpt,34), p(j), p(j)-for00(ipt,jpt,34),
     1    for00(ipt,jpt,32)-273.15, t(j), for00(ipt,jpt,32)-t(j)-273.15
        ELSE IF (day(j) .EQ. vday .AND. 
     1         (hour(j) .GE. 9 .AND. hour(j) .LE. 15) ) THEN
         r = deg*(90.-lat(j))/dx
         ipt = INT(polei+r*COS(long(j)*rads) + 0.5 )
         jpt = INT(polej+r*SIN(long(j)*rads) + 0.5 )
         IF (ipt .LT. 0 .OR. ipt .GT. L) GO TO 3000
         IF (jpt .LT. 0 .OR. jpt .GT. M) GO TO 3000
         IF (for12(ipt,jpt,29) .LE. 1.0) GO TO 3000
         WRITE (13,9003) id(j), lat(j), long(j), date(j),
     1    for12(ipt,jpt,34), p(j), p(j)-for12(ipt,jpt,34),
     1    for12(ipt,jpt,32)-273.15, t(j), for12(ipt,jpt,32)-t(j)-273.15
        ENDIF
 3000 CONTINUE

 9003 FORMAT (A7, F6.1,F7.1,I9, 3F8.1, 2x, 3F6.1)
      STOP
      END
