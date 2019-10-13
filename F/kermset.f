      PROGRAM kermst
C     Program to read in the results of a long ll and write out
C       as many kermit master files as required for transfer,
C       subject to a limit on the maximum (file, combined) size
C       of transfer.
C     Robert Grumbine 27 September 1994

      IMPLICIT none

      INTEGER nmax
      PARAMETER (nmax = 5000)
      INTEGER fsize(nmax)
      CHARACTER*13 fname(nmax)
      INTEGER rsize, dsize
      CHARACTER*60 name
      INTEGER i, imax, j

      PRINT *,'What is the maximum disk size (Kbytes)?'
      READ (*,9001) dsize
      dsize = dsize*1024
      PRINT *,'What is the name of the ll file?'
      READ (*,9002) name
      OPEN (10, FILE=name, FORM='FORMATTED', STATUS='OLD')
 9001 FORMAT (I4)
 9002 FORMAT (A60)
 9003 FORMAT (34x,I6,14x,A13)
 9004 FORMAT ('kermit s ',A13)

      i = 0
 1000 CONTINUE
        i = i + 1
        READ (10, 9003, END=2000) fsize(i), fname(i)
        WRITE (*,9003) fsize(i), fname(i)
        GO TO 1000

 2000 CONTINUE
      imax = i
      j = 1 
      PRINT *,'What would you like to call the first command file?'
      READ (*,9002) name
      OPEN (10+j, FILE=name, FORM='FORMATTED', STATUS='NEW')
      i = 0 
      rsize = 0
 3000 CONTINUE
        i = i + 1
        IF (rsize + fsize(i) .LT. dsize .AND. i .LE. imax) THEN
          rsize = rsize + fsize(i)
          WRITE (10+j, 9004) fname(i)
         ELSE IF (fsize(i) .GT. dsize) THEN
          PRINT *,'Skipping file',fname(i),'Size = ',fsize(i)
          GO TO 3000
         ELSE IF (i .LE. imax) THEN
          i = i - 1
          j = j + 1
          rsize = 0
          PRINT *,'What would you like to call the next file?'
          READ (*,9002) name
          OPEN (10+j, FILE=name, FORM='FORMATTED', STATUS='NEW')
         ELSE
          GO TO 4000
        ENDIF
      GO TO 3000 

 4000 CONTINUE
      PRINT *,'done setting up files'
      PRINT *,'number of files created (number of disks required)=',j
      DO 5000 i = 1, j
        CLOSE (10+j, STATUS='KEEP')
 5000 CONTINUE

      END

 
