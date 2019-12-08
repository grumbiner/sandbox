C***********************************************************----------!!
      SUBROUTINE readin (xraw, yraw, size)
C     Subroutine to read in the data.  The format and style are specified
C       in the arguments.
C     Read in control params locally.  Cannot read in size here since use
C       it for adjustable size array.  6-25-86

      INTEGER size
      REAL xraw(size), yraw(size)

C     Local variables:
      INTEGER i, ifmt, istyle
      CHARACTER*60 fname
      CHARACTER*15 fmt
      CHARACTER respon

C     Prepare the data file(s).
      PRINT *,'Are the data formatted?'
      READ (*,9002) respon
      IF (respon .EQ. 'n') THEN
        ifmt = 2
       ELSE
        ifmt = 1
      ENDIF

      PRINT *,'What style are the data in: 1 x y pairs'
      PRINT *,'                            2 x vector y vector'
      READ (*,9004) istyle
	
	PRINT *,'ifmt, istyle', ifmt, istyle
	
      IF (ifmt .EQ. 1) THEN
C       Formatted
        PRINT *,'What is the data format?'
        READ (*,9003) fmt
        PRINT *,'What is the name of the data file?'
        READ (*,9001) fname
        OPEN(10, FILE = fname, STATUS = 'OLD', FORM = 'FORMATTED')
       
       ELSE
C       Unformatted
        PRINT *,'What is the name of the data file?'
        READ (*,9001) fname
        OPEN (10, FILE=fname, STATUS='OLD', FORM='UNFORMATTED')

      ENDIF
      PRINT *,'format is', fmt
C     Read in the data.
      IF (ifmt .EQ. 1) THEN
C       Formatted
        IF (istyle .EQ. 1) THEN
          DO 1000 I = 1, size
            READ (10, fmt) xraw(i), yraw(i)
	      WRITE (*, fmt) xraw(i), yraw(i)
 1000     CONTINUE
         ELSE 
          DO 1100 i = 1, size
            READ (10, fmt) xraw(i)
 1100     CONTINUE
          DO 1200 i = 1, size
            READ (10, fmt) yraw(i)
 1200     CONTINUE

        ENDIF

       ELSE
C       Unformatted
        IF (istyle .EQ. 1) THEN
          DO 2000 i = 1, size
            READ (10) xraw(i), yraw(i)
 2000     CONTINUE
         ELSE 
          READ (10) xraw
          READ (10) yraw
        ENDIF

      ENDIF

 9001 FORMAT (A60)

 9002 FORMAT (A1)

 9003 FORMAT (A30)

 9004 FORMAT (I1)

      RETURN
      END
