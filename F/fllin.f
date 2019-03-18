      PROGRAM fllin
C     extract the flux across a given line
C     Robert Grumbine 27 Sep 1995

      INTEGER ny, nt
      PARAMETER (ny = 30)
      PARAMETER (nt = 480)

      REAL fl(ny, nt), row(ny)
      INTEGER nsteps, line, i
      CHARACTER*60 fname

      PRINT *,'What is the name of the file?'
      READ (*,9003) fname
      OPEN(10, FILE=fname, FORM='UNFORMATTED', STATUS='OLD')
      PRINT *,'What will be the output?'
      READ (*,9003) fname
      OPEN(11, FILE=fname, FORM='FORMATTED', STATUS='NEW')
 
      PRINT *,'How many steps are there?'
      READ (*,9001) nsteps
      PRINT *,'What line do you want?'
      READ (*,9001) line
      DO 1000 i = 1, nsteps
        READ (10) row
        WRITE (11, 9002) i, row(line)
	WRITE (* , 9002) i, row(line)
 1000 CONTINUE

      CLOSE(10, STATUS='KEEP')
      CLOSE(11, STATUS='KEEP')

 9001 FORMAT (I4)
 9002 FORMAT (I4, E13.6)
 9003 FORMAT (A60)

      END
C***********************************************************----------!!
      SUBROUTINE ftrc(f, c, buffer, n, iwk, wk)

      INTEGER n
      COMPLEX c(n)
      REAL f(n), wk(3*n+150)
      INTEGER iwk(3*n+150), i
      REAL buffer

C     Compute the fast fourier transform of the input vector using the
C       IMSL routine FFTRC (which also calls FFTCC and FFT2C).

      CALL FFTRC(f, n, c, iwk, wk)
      DO 1000 i = 1, n/2 + 1
        c(i) = conjg(c(i))/FLOAT(n)
 1000 CONTINUE

      DO 1100 i = 2, n/2
        c(n+2-i) = CONJG(c(i))
 1100 CONTINUE

      RETURN
      END
