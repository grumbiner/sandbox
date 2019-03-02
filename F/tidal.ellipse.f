      PROGRAM ellips
C     Program to find the parameters for the tidal ellipse, given
C       the amplitude and phase of the u and v components.
C     Robert Grumbine 27 September 1994

      CHARACTER*60 oname, uname, vname
      REAL ap, am, phip, phim
      REAL au, av, bu, bv
      REAL major, minor, polar, alpha, beta
      REAL ufreq, uamp, upha, vfreq, vamp, vpha
      REAL pi
      PARAMETER (pi = 3.141592654)

      LOGICAL yes

 2000 CONTINUE
      PRINT *,'What do you want to call the output file?'
      READ (*,9001) oname

      PRINT *,'What is the name of the u velocity file?'
      READ (*,9001) uname

      PRINT *,'What is the name of the v velocity file?'
      READ (*,9001) vname

      OPEN (1,  FILE=oname, STATUS='NEW')
      OPEN (10, FILE=uname, STATUS='OLD')
      OPEN (11, FILE=vname, STATUS='OLD')

      WRITE (1, 9005)
      WRITE (*, 9005)
 9005 FORMAT (11X,'frequency',2X,'major',2X,'minor',2X,'eccen',
     1 4X,'alpha',
     2 6X,'beta',3X,'polarization')

C     While not at end of file, do computation.
   1  CONTINUE
      READ (10, 9002, END=1000) ufreq, uamp, upha
      READ (11, 9002, END=1000) vfreq, vamp, vpha

C     If the frequencies do not match, end the calculation.
      IF (vfreq .NE. ufreq) GO TO 1000

      au = uamp * sin ( upha )
      bu = uamp * cos ( upha )

      av = vamp * sin ( vpha )
      bv = vamp * cos ( vpha )

      ap = .5 * ( (bu + av)**2 + (bv - au)**2 ) **.5
      am = .5 * ( (-bu + av)**2 + (bv + au)**2 ) **.5 

      phip = atan2( (bv - au), ( bu + av) )
      phim = atan2( (bv + au), (-bu + av) )

      major = ap + am
      minor = ABS( ap - am )
      polar = ap - am
      alpha = (phip - phim)/2.
      beta  = (phip + phim)/2.

C     convert alpha, beta to degrees:
      alpha = alpha * 180./pi
      beta  = beta  * 180./pi

      IF (polar .GE. 0.) THEN 
        WRITE (*,9003) ufreq, major, minor, 
     1        (major**2-minor**2)**.5/major, alpha, beta
        WRITE (1,9003) ufreq, major, minor,
     1        (major**2-minor**2)**.5/major, alpha, beta
       ELSE
        WRITE (*,9004) ufreq, major, minor, 
     1        (major**2-minor**2)**.5/major, alpha, beta
        WRITE (1,9004) ufreq, major, minor, 
     1        (major**2-minor**2)**.5/major, alpha, beta
      ENDIF

      GO TO 1

 1000 CONTINUE

      CLOSE (1 , STATUS='KEEP')
      CLOSE (10, STATUS='KEEP')
      CLOSE (11, STATUS='KEEP') 
     
      PRINT *,'Would you like to analyze another file?'
      IF (yes(.FALSE.)) GO TO 2000


 9001 FORMAT (A60)

 9002 FORMAT (3E15.7)

 9004 FORMAT (11X,F8.6,2F7.2,F7.3,2F10.1,7X,'clock')

 9003 FORMAT (11X,F8.6,2F7.2,F7.3,2F10.1,7X,'anti ')

      END
 
