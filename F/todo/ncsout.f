C***********************************************************__________!!
      SUBROUTINE ncsout(etan, emax, emin, step, zo)
      INCLUDE "grid.inc"
      
      REAL etan(nlong, nlat), emax, emin
      CHARACTER*60 fname
      INTEGER step
      
      REAL upper, lower
      CHARACTER*1 zo(nlat*nlong)
      INTEGER i, j, k
      
      upper = emax
      lower = emin
      
      fname = 'shallout'
      iref = INDEX(fname, ' ')
CD      PRINT *,'step = ', step, LONG(362)
      
      k = 0
      DO 1010 j = 1, nlat
        DO 1020 i = 1, nlong
          k = k+1
          zo(k) = CHAR( MOD( 
     1       INT(256.* (etan(i,j)-lower)/ (ABS(lower)+ABS(upper)) )
     2                      , 255 ) )
 1020   CONTINUE
 1010 CONTINUE
C     Warning!! if step > 255, the character conversion will not work properly
      fname(iref  :iref  ) = CHAR(65+step/26)
      fname(iref+1:iref+1) = CHAR(65+MOD(step,26))
      OPEN (UNIT=11, FILE=fname, FORM='UNFORMATTED', STATUS='NEW')
      WRITE (11) zo
      CLOSE (UNIT=11)
      
      RETURN
      END
