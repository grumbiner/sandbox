C*************************************************----------++++++++++!!      
      SUBROUTINE subsec(x, nrec, len, del)
C     Program to analyze part of a data record at a time.
C     Robert Grumbine 7-9-86.
C     Modified to subroutine 7-19-86.

      INTEGER del, len, nrec
      REAL x(nrec), xpart(2500), avg(2500), sd(2500)

      INTEGER i, j
      REAL dummy, avtmp, sdtmp

      DO 1000 j = 1, INT( (nrec-len) /del )

        DO 1010 i = 1, len
          xpart(i) = x(i + (j-1)*del )
 1010   CONTINUE

        CALL rmsval(xpart, len, dummy, avtmp, sdtmp )
        avg(j) = avtmp
        sd (j) = sdtmp

 1000 CONTINUE

      PRINT *,'Name of the file with the averages?'
      CALL ritout (INT(nrec/del), avg, 11)
      CLOSE (11, STATUS='KEEP')

      PRINT *,'Name of the file with the s. devs.?'
      CALL ritout (INT(nrec/del), sd, 11)
      CLOSE (11, STATUS='KEEP')


      RETURN
      END
