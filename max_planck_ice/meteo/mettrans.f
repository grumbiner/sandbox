      PROGRAM mettrans
C     Translate meteorological output to formatted file for 
C       transfer to other machines, or to translate such a file
C       to unformatted form.
C     Author: Bob Grumbine
C     LAST MODIFIED: 11 January 1996.

      IMPLICIT none
      INCLUDE "icegrid.inc"
      REAL uwin(L,M), vwin(L,M)
      REAL ta(LP,MP), td(LP, MP), pa(LP, MP)
      REAL prec(LP,MP), lwdn(LP, MP), swdn(lp,mp)
      REAL lwup(LP, MP), mask(LP, MP), tsfc(LP, MP)

      INTEGER i, j, k, dir, steps
      CHARACTER*60 fname

      PRINT *,'Are you going from unformatted to formatted (1)?'
      READ (*,9001) dir
      IF (dir .EQ. 1) THEN
        PRINT *,'What is the name of the unformatted input file?'
        READ (*,9002) fname
        OPEN (10, FILE=fname, FORM='UNFORMATTED', STATUS='OLD')
        PRINT *,'What is the name for the formatted output file?'
        READ (*,9002) fname
        OPEN (11, FILE=fname, FORM='FORMATTED', STATUS='NEW')
       ELSE
        PRINT *,'What is the name of the unformatted output file?'
        READ (*,9002) fname
        OPEN (10, FILE=fname, FORM='UNFORMATTED', STATUS='NEW')
        PRINT *,'What is the name for the formatted input file?'
        READ (*,9002) fname
        OPEN (11, FILE=fname, FORM='FORMATTED', STATUS='OLD')
      ENDIF
      PRINT *,'What would you like to call the header file?'
      READ (*,9002) fname
      OPEN (12, FILE=fname, FORM='FORMATTED', STATUS='NEW')

      PRINT *,'How many steps were there?'
      READ (*,9001) steps
      
      IF (dir .EQ. 1) THEN
C       unformatted to formatted
        DO 1000 k = 1, steps
          READ (10) ta
          READ (10) pa
          READ (10) td
          READ (10) tsfc
          READ (10) swdn
          READ (10) lwdn
          READ (10) lwup
          READ (10) prec
          READ (10) mask
          READ (10) uwin
          READ (10) vwin

          WRITE (11,9003) ((ta(i,j),i=1,LP),j=1,MP)
          WRITE (11,9003) ((pa(i,j),i=1,LP),j=1,MP)
          WRITE (11,9003) ((td(i,j),i=1,LP),j=1,MP)
          WRITE (11,9003) ((tsfc(i,j),i=1,LP),j=1,MP)
          WRITE (11,9003) ((swdn(i,j),i=1,LP),j=1,MP)
          WRITE (11,9003) ((lwdn(i,j),i=1,LP),j=1,MP)
          WRITE (11,9003) ((lwup(i,j),i=1,LP),j=1,MP)
          WRITE (11,9003) ((prec(i,j),i=1,LP),j=1,MP)
          WRITE (11,9003) ((mask(i,j),i=1,LP),j=1,MP)
          WRITE (11,9003) ((uwin(i,j),i=1,L),j=1,M)
          WRITE (11,9003) ((vwin(i,j),i=1,L),j=1,M)

 1000   CONTINUE

       ELSE
        DO 1100 k = 1, steps
          READ (11,9003) ((ta(i,j),i=1,LP),j=1,MP)
          READ (11,9003) ((pa(i,j),i=1,LP),j=1,MP)
          READ (11,9003) ((td(i,j),i=1,LP),j=1,MP)
          READ (11,9003) ((tsfc(i,j),i=1,LP),j=1,MP)
          READ (11,9003) ((swdn(i,j),i=1,LP),j=1,MP)
          READ (11,9003) ((lwdn(i,j),i=1,LP),j=1,MP)
          READ (11,9003) ((lwup(i,j),i=1,LP),j=1,MP)
          READ (11,9003) ((prec(i,j),i=1,LP),j=1,MP)
          READ (11,9003) ((mask(i,j),i=1,LP),j=1,MP)
          READ (11,9003) ((uwin(i,j),i=1,L),j=1,M)
          READ (11,9003) ((vwin(i,j),i=1,L),j=1,M)
          WRITE (10) ta
          WRITE (10) pa
          WRITE (10) td
          WRITE (10) tsfc
          WRITE (10) swdn
          WRITE (10) lwdn
          WRITE (10) lwup
          WRITE (10) prec
          WRITE (10) mask
          WRITE (10) uwin
          WRITE (10) vwin
 1100   CONTINUE

      ENDIF

 9001 FORMAT (I3)
 9002 FORMAT (A60)
 9003 FORMAT (8F9.2)

      CLOSE (10, STATUS='KEEP')
      CLOSE (11, STATUS='KEEP')

      STOP
      END
