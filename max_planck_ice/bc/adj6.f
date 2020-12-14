      SUBROUTINE adj6(tin, sin, tout, sout, imask)
C===========================================================----------++
C     Take a file with temperatures or salinities at the Levitus 
C       points, read in the ice model land-sea mask, and interpolate/
C       average the data onto the masked ocean points.  Don't worry
C       about the Great Lakes or Caspian Sea, but do worry about 
C       Hudson's Bay, the Baltic, and Sea of Okhotsk.
C     Author: Robert Grumbine
C     LAST MODIFIED 21 September 1994.

      IMPLICIT none
      INCLUDE "icegrid.inc"
      INCLUDE "physical.inc"
      REAL mask(0:L,0:M), tout(0:L,0:M), sout(0:L,0:M)
      REAL tin(360, 180), sin(360, 180)
      INTEGER imask(0:L,0:M)

      CHARACTER*60 fname
      INTEGER i, j, k, npts
      REAL lats(LP*MP), longs(LP*MP), tav(LP*MP), sav(LP*MP)

      REAL tdef, sdef, tfreez
      PARAMETER (sdef = 34.7)
      CHARACTER*7 formp
C===========================================================----------++
C     Read in mask
      PRINT *,'What is the name of the mask file?'
      READ (*,9001) fname
      PRINT *, fname
      PRINT *,'the above should be the name of the mask file'

      OPEN (12, FILE=fname, FORM='FORMATTED', STATUS='OLD')
      REWIND (12)
C     Dummy reads to get past the velocity grid
      DO 1100 j = 1, M
        READ(12,9001) fname
 1100 CONTINUE
      PRINT *,'Reading in the scalar mask'
C     Read in the mask for scalars
      WRITE (formp,9002) LP
      DO 1000 j = 0, M
        READ (12,formp) (imask(i,j),i=0,L)
CD        WRITE (*,formp) (imask(i,j),i=0,L)
 1000 CONTINUE

      DO 1200 j = 0, M
        DO 1201 i = 0, L
          mask(i,j) = FLOAT(imask(i,j))
 1201   CONTINUE
 1200 CONTINUE

 9001 FORMAT (A60)
 9002 FORMAT ('(',I3,'I1)')
C===========================================================----------++

C     Have data.  Now need to locate all ocean points from ice
C       grid.  Convention is that ocean = 1.
      PRINT *,'Now to try for data cleaning'
      npts = 0
      DO 2000 j = 0, M
        DO 2100 i = 0, L
          PRINT *,'i, j, mask ',i,j,mask(i,j)
          IF (mask(i,j) .NE. 0) THEN
            npts = npts+1
            CALL mapxy(i*dx+xorig, j*dy+yorig, lats(npts), longs(npts),
     1                 slat, slon, sgn, SQRT(eccen2), rearth)
CD            lats(npts) = SQRT( (DX*(i-polei))**2 +
CD     1                    (DY*(j-polej))**2) / DXDEG - 90.
CD            lats(npts) = SIGN(lats(npts),LATMIN)
CD            IF (j .EQ. polej .AND. i .EQ. polei) THEN
CD              longs(npts) = 0.0
CD             ELSE
CD              longs(npts) = ATAN2(j-polej,i-polei)
CD     1                  *180./(ATAN(1.)*4.) + 10.
CD            ENDIF
            IF (longs(npts) .LT. 0.) THEN
              longs(npts) = 360.+longs(npts)
            ENDIF
          ENDIF
 2100   CONTINUE
 2000 CONTINUE
      PRINT *,'escaped the masking loop'
C===========================================================----------++
C     Call subroutine for temperature and for salinity interpolations:
      PRINT *,'sdef = ',sdef
      PRINT *,'sdef = ',sdef
      PRINT *,'sdef = ',sdef
      PRINT *,'sdef = ',sdef
      PRINT *,'sdef = ',sdef
      PRINT *,'sdef = ',sdef
      PRINT *,'sdef = ',sdef
      PRINT *,'sdef = ',sdef
      tdef = tfreez(sdef) +  0.01
      PRINT *,'About to call regrid for temps'
      PRINT *,'About to call regrid for temps'
      PRINT *,'About to call regrid for temps'
      PRINT *,'About to call regrid for temps'
      PRINT *,'About to call regrid for temps'
      PRINT *,'About to call regrid for temps'
      PRINT *,'About to call regrid for temps'
      PRINT *,'About to call regrid for temps'
      PRINT *,'About to call regrid for temps'
      PRINT *,'About to call regrid for temps'
      PRINT *,'About to call regrid for temps'
      PRINT *,'About to call regrid for temps'
      PRINT *,'About to call regrid for temps'
      PRINT *,'About to call regrid for temps'
      CALL regrid(lats, longs, npts, tin, tav, tdef)
      PRINT *,'About to call regrid for salts'
      CALL regrid(lats, longs, npts, sin, sav, sdef)

C     Unpack the averaged values onto the output grids
      k = 0
      DO 3000 j = 0, M
        DO 3100 i = 0, L
          IF (mask(i,j) .NE. 0) THEN
            k = k+1
            tout(i,j) = tav(k)
            sout(i,j) = sav(k)
           ELSE
            tout(i,j) = tdef
            sout(i,j) = sdef
          ENDIF
 3100   CONTINUE
 3000 CONTINUE
      IF (k .NE. npts) PRINT *,'Warning!!! npts mismatch'

      CLOSE (12)
      RETURN
      END 
