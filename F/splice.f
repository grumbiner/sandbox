      PROGRAM splice
C     splice together the concentrations from the forward (bering),
C       straw man (bering.dat), and variational (freezout) routines.
C     Last file date modification 30 Mar 1996
C     Robert Grumbine
      IMPLICIT none

      INTEGER nmax
      PARAMETER (nmax = 641)
      REAL atrue(nmax), anoise(nmax), avary(nmax)
      INTEGER i, n
      REAL dt
      PARAMETER (dt = 0.25)

      OPEN (10, FILE='bering', FORM='FORMATTED', STATUS='OLD')
      OPEN (11, FILE='bering.dat', FORM='FORMATTED', STATUS='OLD')
      OPEN (12, FILE='freezout', FORM='FORMATTED', STATUS='OLD')
      OPEN (13, FILE='spliced', FORM='FORMATTED', STATUS='UNKNOWN')
  
      i = 0
 1000 CONTINUE
        i = i + 1 
        PRINT *,'i = ', i
        READ (10, 9001, END=2000) atrue(i)
        READ (11, 9001, END=2000) anoise(i)
        READ (12, 9002, END=2000) avary(i)
        GO TO 1000

 2000 CONTINUE 

      n = i - 1

      PRINT *,'reached write loop'
      DO 3000 i = 1, n
        WRITE (13, 9003) (i-1)*dt, atrue(i), anoise(i), avary(i)
        WRITE (*, 9003) (i-1)*dt, atrue(i), anoise(i), avary(i)
 3000 CONTINUE

 9001 FORMAT (F6.4)
 9002 FORMAT (10x, F6.4)
 9003 FORMAT (F9.4, 3F7.4)


      STOP
      END
       
