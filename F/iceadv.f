      PROGRAM iceadv
C     Study the advection of sea ice under prescribed winds.
C     BG 6/13/90.
      IMPLICIT none

      INCLUDE "grid.inc"

      REAL ui(nx, ny), vi(nx, ny), ua(nx, ny), va(nx, ny)
      REAL iarea(nx, ny), ithick(nx, ny), dummy(nx, ny)
      REAL delx, dely, delt, ratio

      INTEGER i, j
      DOUBLE PRECISION sum

      ratio = FLOAT(nx)/36.
      delx = 2.0E4/ratio
      dely = 2.0E4/ratio
      delt = 2.08E4/ratio
      
      OPEN (UNIT=10, FILE='icecover', FORM='UNFORMATTED', STATUS='NEW')
      OPEN (UNIT=11, FILE='icevel', FORM='UNFORMATTED', STATUS='NEW')
      OPEN (UNIT=12, FILE='airvel', FORM='UNFORMATTED', STATUS='NEW')
      OPEN (UNIT=13, FILE='divvort', FORM='UNFORMATTED', STATUS='NEW')

      CALL arset(iarea, nx, ny, 0.5)
      CALL arset(ithick, nx, ny, 0.6)
      CALL arset(dummy, nx, ny, 0.0)
      
      CALL airvel(ua, va, delx, dely)
      WRITE (12) ua
      WRITE (12) va
      CALL div(dummy, ua, va, nx, ny, delx, dely)
      WRITE (13) dummy
      CALL vort(dummy, ua, va, nx, ny, delx, dely)
      WRITE (13) dummy
      CALL icevel(ua, va, ui, vi, nx, ny)
      CALL vort  (dummy, ui, vi, nx, ny, delx, dely)
      WRITE (11) ui
      WRITE (11) vi
 9001 FORMAT (8E10.3)
 
      CALL summer(iarea, nx, ny, sum)
      PRINT *,'total ice is', sum,iarea(nx/2,ny/2)
      WRITE (10) iarea
CD      PRINT *,'time',LONG(362)
      DO 1000 i = 1, 8*30/INT(ratio)
        CALL adv2xy(ui, vi, iarea, dummy, delx, dely, delt,
     1                  i)
CD        CALL summer(iarea, nx, ny, sum)
CD        PRINT *,'total ice is',i, sum,iarea(nx/2,ny/2)
        IF (MOD(i,INT(ratio)) .EQ. 0) WRITE (10) iarea

 1000 CONTINUE
CD      PRINT *,'time',LONG(362)
      PAUSE
      END
