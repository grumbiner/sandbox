	PROGRAM prepgr
C	rewrite model output for input to NCSA graphics package.

	INTEGER nx, ny
	PARAMETER (nx=144)
	PARAMETER (ny=365)
	
	REAL ducup, duclo, dutup, dutlo, dvtup, dvtlo
	REAL dssup, dsslo, dsdup, dsdlo
	PARAMETER (ducup =  0.025)
	PARAMETER (duclo = -0.025)
	PARAMETER (dutup =  0.1)
	PARAMETER (dutlo = -0.1)
	PARAMETER (dvtup =  0.25)
	PARAMETER (dvtlo = -0.25)
	PARAMETER (dssup =  0.5)
	PARAMETER (dsslo = -dssup)
	PARAMETER (dsdup = 0.0)
	PARAMETER (dsdlo = -dssup)

      REAL uc(nx, ny), vc(nx, ny)
	
	REAL z(nx, ny)
	CHARACTER*1 zo(nx*ny)
	CHARACTER*60 fname, dummy
	LOGICAL yes
	REAL upper, lower
	INTEGER nstep, i, j, k, n

 9001 FORMAT (A60)
 9002 FORMAT (E13.6)
 9003 FORMAT (I3)

    1 CONTINUE
	  PRINT *,'What is the name of the file?'
	  READ (*,9001) fname
	  OPEN (UNIT=10, FILE=fname, FORM='FORMATTED', STATUS='OLD')
	  PRINT *,'What is the upper bound?'
	  READ (*,9002) upper
	  PRINT *,'What is the lower bound?'
	  READ (*,9002) lower

	  DO 1000 n =1, ny
	    READ (10, 9019) dummy
          READ (10, 9020) (z(l,n),l=1,nx)
 1000   CONTINUE
 9019   FORMAT (A60)
 9020   FORMAT (16F5.1)
 
	    k = 0
	    DO 1010 j = 1, ny, 1
	      DO 1020 i = 1, nx, 1
		  k = k+1							
		  zo(k) = CHAR(INT(256* (z(i,j)+lower)/
     1                            (ABS(lower)+ABS(upper)) ) )
 1020       CONTINUE
 1010     CONTINUE
 	    i = INDEX(fname,' ')
	    fname(i:i) = 'O'
C	    Warning!! if n > 255, the character conversion will not work properly
	    OPEN (UNIT=11, FILE=fname, FORM='UNFORMATTED', STATUS='NEW')
	    WRITE (11) zo
	    CLOSE (UNIT=11)
    

	  PRINT *,'Would you like to process another file?'
	  IF (yes(.FALSE.)) GO TO 1

	END
