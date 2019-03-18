      PROGRAM enhead
C     Act as a front end for the subroutine which compares arrays
      
      INTEGER nx, ny
      PARAMETER (nx = 20)
      PARAMETER (ny = 20)
      REAL un(nx, ny), ue(nx, ny)
      REAL uangle(nx, ny)
      CHARACTER*60 fname
      INTEGER i, nypts

      PRINT *,'What is the name of the first comparison file?'
      READ (*,9001) fname
      OPEN (10, FILE = fname, FORM='UNFORMATTED', STATUS='OLD')
      PRINT *,'What is the name of the second comparison file?'
      READ (*,9001) fname
      OPEN (12, FILE = fname, FORM='UNFORMATTED', STATUS='OLD')
    
      PRINT *,'What would you like to call the angle file?'
      READ (*,9001) fname
      OPEN (14, FILE = fname, FORM='UNFORMATTED', STATUS='NEW')

C     For now, assume that all time steps are to be analyzed, 
C         and that the number is unknown.
      PRINT *,'What is the second dimension?'
      READ (*,9002) nypts
      PRINT *,'What would you like to call the file with correlations?'
      READ (*,9001) fname
      OPEN (99, FILE=fname, FORM='FORMATTED', STATUS='NEW')

      i = 0
 1000 CONTINUE
        i = i + 1
        READ (10, END = 2000) ue
        READ (12, END = 2000) un
        CALL energy(ue, un, uangle, i, nx, nypts)
        WRITE (14) uangle
        GO TO 1000

 2000 CONTINUE

 9001 FORMAT (A60)

 9002 FORMAT (I5)

      END
