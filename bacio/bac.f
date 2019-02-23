      PROGRAM ba
C     Example of working on/with byte-addressable C routine for IO.
C     Two routines exist -- bacio (byte-addressable character IO)
C                      and  banio (byte-addressable numeric IO).
C     Two include files should be used: 
C       -- clib.inc includes definitions for the C interface
C       -- locale.inc includes definitions for the system you're 
C             running on (size of integers, reals, etc.)
C     Robert Grumbine 16 March 1998

      INTEGER mode, start, size, no, nactual, fdes, newpos
      CHARACTER*80 fname
      CHARACTER*1 dataary(3072)
      INTEGER intdat(3072)

      INCLUDE "clib.inc"
      INTEGER jret, bacio, banio 
  
      mode = BAOPEN_RW + BAREAD + BACLOSE 
      WRITE (fname, 9001) "file1   "
 9001 FORMAT (A8)

      start = 0
      size  = 1
      no    = 1024 * 3 
CD      DO i = 1, no
CD        dataary(i) = CHAR(i/16)
CD      ENDDO
      jret = bacio(mode, start, newpos, size, no, nactual, fdes, 
     1                fname, dataary)

      PRINT *,'jret, nactual, start, newpos = ',
     1     jret, nactual, start, newpos
    
      IF (jret .GE. 0 .AND. nactual .GE. 0) THEN 
        DO 1000 i = 1, 3072
          PRINT *,i,ICHAR(dataary(i))
          intdat(i) = ICHAR(dataary(i))
 1000   CONTINUE
      ENDIF

C     Now test the numeric io mode:
      mode = BAOPEN_RW + BAWRITE + BACLOSE 
      start = 0
      size  = 4
      no    = 1024 * 3 
      WRITE (fname, 9001) "file2   "
      jret = banio(mode, start, newpos, size, no, nactual, fdes, 
     1               fname, intdat)
      PRINT *,'jret, nactual, start, newpos = ',
     1               jret, nactual, start, newpos

      mode = BAOPEN_RW + BAREAD + BACLOSE 
      start = 0
      size  = 4
      no    = 1024 * 3 
      WRITE (fname, 9001) "file2   "
      jret = banio(mode, start, newpos, size, no, nactual, fdes, 
     1               fname, intdat)
      PRINT *,'jret, nactual, start, newpos = ',
     1               jret, nactual, start, newpos
      DO 2000 i = 1, 3072
        PRINT *,'readback ',i,intdat(i)
 2000 CONTINUE
 
      END
