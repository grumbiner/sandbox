      PROGRAM sum

      IMPLICIT none

      INTEGER nx, ny, lnx, lny
      PARAMETER (nx = 384)
      PARAMETER (ny = 190)
      PARAMETER (lnx = 360)
      PARAMETER (lny = 181)
      REAL tmelt, tfreez, theat
      REAL melt(nx, ny), freez(nx, ny), heat(nx, ny)
      REAL lmelt(lnx, lny), lfreez(lnx, lny), lheat(lnx, lny)
      REAL bylat(lny,3)
      INTEGER c1
      CHARACTER*80 fname
      INTEGER kgds

      INTEGER i, j, ti, tj, yy, mm
      INTEGER unit, iunit

      DO 1 j = 1, ny
      DO 1 i = 1, nx
        lmelt(i,j) = 0.0
        lfreez(i,j) = 0.0
        lheat(i,j) = 0.0
        melt(i,j) = 0.0
        freez(i,j) = 0.0
        heat(i,j) = 0.0
  1   CONTINUE

      yy=97
      mm=1
      DO 1000 iunit = 1,12
        mm  = mm + 1 
        IF (mm .GT. 12) THEN 
          yy = yy + 1 
          mm = mm - 12
        ENDIF
        unit = 8 + iunit
     
        WRITE (fname,9010)  yy*100 + mm
 9010   FORMAT("equiv.",I4)
        PRINT *,'file = ',fname
        OPEN (unit, FILE=fname, FORM="FORMATTED", STATUS="OLD")
        DO 1001 j = 1, ny
        DO 1002 i = 1, nx
          READ (unit, 9001) ti, tj, tmelt, tfreez, theat
          melt(i,j) = melt(i,j) + tmelt 
          freez(i,j) = freez(i,j) + tfreez 
          heat(i,j) = heat(i,j) + theat 
 1002   CONTINUE
 1001   CONTINUE
        CLOSE (unit, STATUS="keep")
 1000 CONTINUE

      DO 2000 j = 1, ny
      DO 2001 i = 1, nx
        WRITE (*,9002) i, j, melt(i,j), freez(i,j), heat(i,j), 
     1         melt(i,j)+freez(i,j)
 2001 CONTINUE
 2000 CONTINUE

      kgds = 3*126+6
      CALL ffld(lmelt, melt, kgds)
      PRINT *,'returned from ffld - 1'
      CALL ffld(lfreez, freez, kgds)
      PRINT *,'returned from ffld - 2'
      CALL ffld(lheat, heat, kgds)
      PRINT *,'returned from ffld - 3'
      DO 3000 j = 1, lny
      DO 3001 i = 1, lnx
        WRITE (*,9002) i, j, lmelt(i,j), lfreez(i,j), lheat(i,j), 
     1         lmelt(i,j)+lfreez(i,j)
 3001 CONTINUE
 3000 CONTINUE
      
      DO 4000 j = 1, lny
        bylat(j,1) = 0.0
        bylat(j,2) = 0.0
        bylat(j,3) = 0.0
        c1 = 0
        DO 4100 i = 1, lnx
          IF (lmelt(i,j) .NE. 0. .OR. lfreez(i,j) .NE. 0.) THEN
            bylat(j,1) = bylat(j,1) + lmelt(i,j)
            bylat(j,2) = bylat(j,2) + lfreez(i,j)
            c1 = c1 + 1
          ENDIF
          bylat(j,3) = bylat(j,3) + lheat(i,j)
          
 4100   CONTINUE
        IF (c1 .NE. 0) THEN
          bylat(j,1) = bylat(j,1) / c1
          bylat(j,2) = bylat(j,2) / c1
        ENDIF
        bylat(j,3) = bylat(j,3) / 360.
        WRITE (*,9002) i,91.-j, bylat(j,1), bylat(j,2), bylat(j,3), 
     1             bylat(j,1)+bylat(j,2)
 4000 CONTINUE

 9001 FORMAT (I3,1x,I3,2x,F5.2,1x,F5.2,1x,F6.2)
 9002 FORMAT (I3,1x,I3,2x,F5.2,1x,F5.2,1x,F6.2, 1x,F5.2)

      END

