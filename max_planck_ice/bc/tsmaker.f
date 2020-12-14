      PROGRAM tsmaker
C     Read in Levitus ocean climatology file,s and create the
C       tsshal and tsdeep files needed for MPI program.
C     Robert Grumbine 1 August 1995

      IMPLICIT none

      INCLUDE "icegrid.inc"

      INTEGER dlevel
      REAL sdeep(360, 180), tdeep(360, 180)
      REAL sshal(360,180), tshal(360,180)
      REAL tsgrid(0:L,0:M), ssgrid(0:L,0:M)
      REAL tdgrid(0:L,0:M), sdgrid(0:L,0:M)
      INTEGER imask(0:L,0:M)

      CHARACTER*80 fin
      CHARACTER*60 fout

      READ (*,9001) dlevel
 9001 FORMAT (I2)
 9002 FORMAT (A80)
 9003 FORMAT (A60)

      READ (*,9002) fin
      READ (*,9003) fout 
      PRINT *,fin
      PRINT *,'fout = ',fout
      OPEN (10, FILE=fin, FORM='FORMATTED', STATUS='OLD')
      OPEN (20, FILE=fout, FORM='UNFORMATTED', STATUS='NEW')
      CALL levitus(10, 20)
      CLOSE (10)
      CLOSE (20)

      CALL layers(dlevel, fout, sdeep, sshal, 20)

      READ (*,9002) fin
      READ (*,9003) fout 
      PRINT *,'fout 2 = ',fout
      OPEN (11, FILE=fin, FORM='FORMATTED', STATUS='OLD')
      OPEN (21, FILE=fout, FORM='UNFORMATTED', STATUS='NEW')
      CALL levitus(11, 21)
      CLOSE (11)
      CLOSE (21)

      CALL layers(dlevel, fout, tdeep, tshal, 21)

      PRINT *,'fout is ',fout
      PRINT *,'Calling adj6 for shallow'
      CALL adj6(tshal, sshal, tsgrid, ssgrid, imask)
      PRINT *,'Calling adj6 for deep'
      CALL adj6(tdeep, sdeep, tdgrid, sdgrid, imask)

      PRINT *,'Calling tsexam'
      CALL tsexam(tsgrid, ssgrid, tdgrid, sdgrid, imask)

      READ (*,9002) fin
      OPEN (98, FILE=fin, FORM='UNFORMATTED', STATUS='NEW')
      READ (*,9002) fin
      OPEN (99, FILE=fin, FORM='UNFORMATTED', STATUS='NEW')
      WRITE (98) tsgrid
      WRITE (98) ssgrid
      WRITE (99) tdgrid
      WRITE (99) sdgrid

      CLOSE (98, STATUS='KEEP')
      CLOSE (99, STATUS='KEEP')

      STOP
      END
