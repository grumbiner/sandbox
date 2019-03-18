      PROGRAM collec
C     Collect the spectra from flpower output and compute amplitude
C       vs distance fits for each frequency.
C     Robert Grumbine 27 September 1994

      IMPLICIT none
    
      INTEGER nfreqs, nfiles
      INTEGER i, j, n
      REAL amp(40,100)
      REAL temp(100), temp2(100), dist(40), y(100)
      REAL av, avx, avy, sigx, sigy, corxy
      REAL a0, a1
      LOGICAL yes

      PRINT *,'How many data files are there?'
      READ (*,9001) nfiles
      PRINT *,'How many frequencies are in each file?'
      READ (*,9001) nfreqs
      DO 1000 i = 1, nfiles
        PRINT *,'file?',i
        CALL readin(temp, nfreqs, 10+i)
        DO 1010 j = 1, nfreqs
          amp(i,j) = temp(j)
 1010   CONTINUE
 1000 CONTINUE

      PRINT *,'Name of the distance file?'
      CALL readin(dist, nfiles, 10+1+i)

C     Now compute the best fit amplitude - distance relation for 
C        each frequency, print the best fit line, and the r**2 value.
      DO 2000 i = 1, nfreqs
        DO 2010 j = 1, nfiles
          temp(j) = amp(j,i)
CD        PRINT *,temp(j)
 2010   CONTINUE

C       Look at linear fit:
        CALL detrnd(dist, temp, nfiles, a0, a1, av, .FALSE.)
        DO 2020 j = 1, nfiles
          y(j) = a0 + a1*dist(j)
 2020   CONTINUE
        CALL correl(y, temp, nfiles, sigx, sigy, corxy, avx, avy)
        WRITE (*,9002) 'Linear     ', a0, a1, corxy*corxy
        
C       Now look for exponential fit:
        DO 2030 j = 1, nfiles
          temp2(j) = ALOG10(temp(j))
 2030   CONTINUE
        CALL detrnd(dist, temp2, nfiles, a0, a1, av, .FALSE.)
        DO 2040 j = 1, nfiles
          y(j) = a0 + a1*dist(j)
 2040   CONTINUE
        CALL correl(y, temp, nfiles, sigx, sigy, corxy, avx, avy)
        WRITE(*,9002) 'Exponential', a0, a1, corxy*corxy

        PRINT *,'Print out i, amplitudes?'
        IF (yes(.FALSE.)) THEN
          PRINT *,i
          WRITE (*,9003) (j, temp(j),j=1,nfiles)
        ENDIF
 2000 CONTINUE

 9001 FORMAT (I5)

 9002 FORMAT (A12, 3(2X,E10.4) )

 9003 FORMAT (I4, F10.6)

      END 
