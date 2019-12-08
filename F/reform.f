      PROGRAM reform
C     Program to read in pc formatted files and write out as unformatted.
C     Base version from acor3.for 12-08-93.
C     
C     Subroutines:
C       Readat - 12-09-93 Read in a month's worth of nsidc maps      
C       Writ   - 12-09-93 Write out the month of reanalyzed maps

      IMPLICIT none
      
      INTEGER nday, nx, ny
      PARAMETER (nday =   1)
      PARAMETER (nx   = 304)
      PARAMETER (ny   = 448)
      CHARACTER*1 x(nday, nx, ny)

      CHARACTER*60 base, tag

      PRINT *,'What is the base name of the input file?'
      READ (*,9009) base
      PRINT *,'What is the file tag?'
      READ (*,9009) tag
 9009 FORMAT (A60) 
      
      CALL readat(x, nday, nx, ny, base, tag)

      PRINT *,'What is the base name for the output files?'
      READ (*,9009) base
      PRINT *,'What is the tag for the output files?'
      READ (*,9009) tag
      CALL writ(x, nday, nx, ny, base, tag)


      STOP
      END
