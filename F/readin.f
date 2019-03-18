      SUBROUTINE readin (xraw, size)

C     Subroutine to read in data.  The format and style are specified
C       in the arguments.
C     Read in control params locally.  Cannot read in size here since use
C       it for adjustable size array.  6-25-86
C     Version modified to only read in one vector.  7-9-86.
C     Written by Bob Grumbine

      INTEGER size
      REAL xraw(size)

C     Local variables
      INTEGER ifmt, i
      CHARACTER*60 fname
      CHARACTER*30 fmt
      CHARACTER respon

C***********************************************************----------!!
C     Prepare the data file(s).
      PRINT *,'Are the data formatted (y or n)?'
      READ (*,9002) respon
      IF (respon .EQ. 'n') THEN
        ifmt = 2
       ELSE
        ifmt = 1
      ENDIF

C***********************************************************----------!!
      IF (ifmt .EQ. 1) THEN
C       Formatted
        PRINT *,'What is the data format (the form must be in parens)?'
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
C***********************************************************----------!!

C     Read in the data.
      IF (ifmt .EQ. 1) THEN
C       Formatted
        DO 1000 i = 1, size
          READ (10, fmt) xraw(i)
 1000   CONTINUE

       ELSE
C       Unformatted
        READ (10) xraw

      ENDIF
C***********************************************************----------!!

C     Close the file.
      CLOSE (10, STATUS='KEEP')

C***********************************************************----------!!

 9001 FORMAT (A60)

 9002 FORMAT (A1)

 9003 FORMAT (A30)

 9004 FORMAT (I1)

      RETURN
      END
