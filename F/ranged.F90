      PROGRAM unweaver
! Test/demo of successive over-relaxation method to fill in land
! temperatures -- replacement for 'weaver'
      IMPLICIT none
      INTEGER nx, ny, landflag
      REAL dlat, dlon
      PARAMETER (nx = 12*360)
      PARAMETER (ny = 12*180)
      PARAMETER (landflag = 157)
      PARAMETER (dlat = 1./12.)
      PARAMETER (dlon = 1./12.)

      REAL sst(nx, ny), orig(nx, ny), mask(nx, ny)
      REAL weight
      REAL conv
      PARAMETER (conv = dlat*111.1e3)
      INTEGER delta(nx, ny)

      OPEN(11, FILE="outputs", FORM="UNFORMATTED", STATUS="NEW")

      OPEN(10, FILE="sstin", FORM="UNFORMATTED", STATUS="OLD")
      READ(10) orig
      PRINT *,'orig max min ',MAXVAL(orig), MINVAL(orig)
      CLOSE(10)
      WRITE(11) orig

      OPEN(12, FILE="mask", FORM="UNFORMATTED", STATUS="OLD")
      READ(12) mask
      PRINT *,'mask max min ',MAXVAL(mask), MINVAL(mask)
      CLOSE(12)
      mask = mask * 100
      WHERE (mask .GT. 100.1) mask = 157
      WHERE (mask .LT. 100.1) mask = 0
      WRITE (11) mask

      OPEN(13, FILE="dist.bin", FORM="UNFORMATTED", STATUS="OLD")
      READ (13) sst
      CLOSE(13)
      PRINT *,'distances max = ',MAXVAL(sst), MINVAL(sst) 
      sst = sst / conv
      delta = sst
      PRINT *,'initial range max min',MAXVAL(delta), MINVAL(delta)
!      WHERE(delta .GT. 128) delta = 128

!      READ (*,*) weight
      weight = 1.0
      sst = orig - 273.15
      CALL sor(sst, mask, nx, ny, dlat, dlon, landflag, weight, delta)
      PRINT *,'sst-post max min ',MAXVAL(sst), MINVAL(sst)
      sst = sst + 273.15
      WRITE(11) sst

      sst = sst - orig
      WRITE(11) sst
      PRINT *,'delta max min ',MAXVAL(sst), MINVAL(sst)
      
      END

! Note that this will over-write the input analysis grid
      SUBROUTINE sor(analysis, mask, nx, ny, dlat, dlon, landflag, weight, delta) 
      IMPLICIT none
      INTEGER nx, ny, landflag
      REAL dlat, dlon
      REAL analysis(nx, ny), tmp(nx, ny), old(nx, ny)
      REAL mask(nx, ny)
      INTEGER i, itmax, delta(nx, ny)
      REAL rms, limit, dmax, dmin, weight

      i      = 0
      itmax  = 40000
      rms    = 9e9
      limit  = 1.e-4

      old = analysis
!      delta = 256 
      WHERE (delta .NE. 0) delta = 256

      DO WHILE (MAXVAL(delta) .GE. 1) 
        PRINT *,'max delta = ',MAXVAL(delta)
        rms    = 9e9
        DO WHILE (rms .GT. limit .AND. i .LT. itmax)
          CALL iterate(analysis, mask, nx, ny, dlat, dlon, landflag, delta)
          tmp = analysis - old;
      
          dmax = MAXVAL(tmp)
          dmin = MINVAL(tmp)
          rms = max(dmax, -dmin);
    
!          old = analysis;
          old = old + weight*tmp;
          i = i + 1
          PRINT *,'iteration ',MAXVAL(delta), i,'max = ',rms
        END DO
        PRINT *,'final iteration ',MAXVAL(delta), i, rms
!problem here with delta = grid, in that if some portion delta = 1, and
!some it is 32, the division by 2 gives 0 in parts, but they have not
!fully converged
        IF (MAXVAL(delta) .NE. 1) THEN
          delta = MAX(1,delta / 2)
        ELSE
          delta = delta / 2
        ENDIF
      END DO

      RETURN
      END

      SUBROUTINE iterate(y, mask, nx, ny, dlat, dlon, landflag, delta)
      IMPLICIT none
      INTEGER nx, ny, landflag
      REAL dlat, dlon
      REAL y(nx, ny)
      REAL mask(nx, ny)
      INTEGER delta(nx, ny)
      INTEGER i, j
      REAL rdlat, rdlon, firstlat
      REAL theta, divisor, del, rpdg, c1, c2, c3
      INTEGER ipi, ipj, imi, imj, jpi, jpj, jmi, jmj

      rpdg = ABS(acos(-1.)/180.)
      rdlat = dlat * rpdg
      rdlon = dlon * rpdg
      c1 = 1./rdlat/rdlat

      firstlat = 90 - dlat/2.

! Need to take care of j = 1, ny seam/edge
!  well, not really -- in antarctic, it's a fool who thinks the pole is
!  not land, and in the arctic, it's analyzed.
      DO j = 1, ny
        ipj = j
        imj = j
        theta = rpdg * (firstlat - dlat*(j-1))

      DO i = 1, nx
        IF (delta(i,j) .EQ. 0) CYCLE
        jpj = j + delta(i,j)
        IF (jpj .GT. ny) CYCLE
        jmj = j - delta(i,j)
        IF (jmj .LT. 1) CYCLE
        divisor = 2./cos(theta)/cos(theta)/rdlon/rdlon/delta(i,j)/delta(i,j) &
                + 2./rdlat/rdlat/delta(i,j)/delta(i,j)
        c2 = 1./rdlon/rdlon/cos(theta)/cos(theta)/delta(i,j)/delta(i,j)
        c3 = -tan(theta)/2./rdlat/delta(i,j)

        IF (mask(i,j) .EQ. landflag) THEN
          jmi = i
          jpi = i
          imi = i-delta(i,j)
          IF (imi .LE. 0) imi = nx-imi
          ipi = i+delta(i,j)
          IF (ipi .GE. nx+1) ipi = 1+ipi-nx
          del = (y(jpi, jpj)+y(jmi, jmj))*c1 + & 
                (y(ipi, ipj)+y(imi, imj))*c2
          del = del - c3*(y(jpi, jpj)-y(jmi, jmj))
          y(i,j) = del/divisor
        ENDIF

      ENDDO
      ENDDO
      

      RETURN
      END
