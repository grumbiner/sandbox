      PROGRAM land
      IMPLICIT NONE 

      INTEGER nx, ny
      PARAMETER (nx = 12 * 360)
      PARAMETER (ny = 12 * 180)

      CHARACTER*1 mask(nx, ny)
      INTEGER imask(nx, ny)

C Bacio-related:
      INTEGER ierr
      INTEGER fdes, newpos, nactual, start, bacio
      INCLUDE "clib.inc" 
      INCLUDE "locale.inc" 
      INTEGER i
      CHARACTER*90 fname


      WRITE (fname, 9001)
 9001 FORMAT ("seaice_gland5min")

      i = nx*ny
      start = 0
      newpos = 0
      ierr = bacio(BAOPEN_RONLY + BAREAD + BACLOSE, start, newpos,
     1              SIZEOF_CHARACTER, i, nactual, fdes, fname, mask)
      
      IF (ierr .NE. 0) THEN
        PRINT *,'bacio ierr = ',ierr
        STOP "error from bacio read "
      ENDIF

      DO i = 1, ny
        WRITE (*,9002) i,ICHAR(mask(1,i))
      ENDDO
 9002 FORMAT(I4,1x,I3)
 
      STOP
      END
